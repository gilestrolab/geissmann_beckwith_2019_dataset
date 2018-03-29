rm(list=ls());gc()
options(nwarnings = 1000)
library(scopr)
library(ggetho)
library(sleepr)
library(ggdendro)
library(dendextend)

source("../ggplot_themes.R")

#~ library(ggtern)
#~ library(grid)
#~ library(Gmisc)


FEMALE_MALE_PALETTE <- c("#be2828ff", "#282896ff")
CONTROL_SD_PALETTE <- c( "#969696ff", "#3caa3cff")
METADATA <- "metadata.csv"
CACHE <- "./cache/"
#~ RESULT_DIR <- "./raw_results/"
RESULT_DIR <- "/data/ethoscope_results"
REMOTE_DIR <- "ftp://nas.lab.gilest.ro/auto_generated_data/ethoscope_results/"


THR <- 1
THR_W <- 2.5


met <- fread(METADATA)
met <- met[status == "OK"]
#~ met <- link_ethoscope_metadata_remote(met,
#~                                       remote_dir =  REMOTE_DIR,
#~                                       result_dir = RESULT_DIR,
#~                                       verbose = TRUE)

met <- link_ethoscope_metadata(met, result_dir = RESULT_DIR)
                                      
                                                                            
dt <- load_ethoscope(met,
					   max_time=days(7),
					   reference_hour=9.0,
					   cache = CACHE,
					   FUN = sleep_annotation,
					   ncores=1)

summary(dt)
                                      
   
curate_data <- function(data){
  data[, t := t - days(xmv(baseline_days))]
	# first we remove animals that do not have enought data points
	valid_animals <- data[,.(t_range = max(t) - min(t)), by=id][t_range >= days(5)]$id
	data <- data[t > days(-2.5) & 
				 t < days(+3) &
				 id %in% valid_animals]
	
	data[,x_rel:=ifelse(xmv(region_id) > 10, 1-x, x)]

	norm_x <- function(x){
		min_x <- quantile(na.omit(x),probs=0.01)
		x <- x - min_x
		max_x <- quantile(na.omit(x),probs=0.99)
		x / max_x
		}
	data[, x_rel := norm_x(x_rel), by=id]
}

# then we apply this function to our data
dt <- curate_data(dt)


# we have a look at our resulting data
print(dt)
summary(dt)

ggetho(dt, aes(z=asleep, y=paste(mated,id, sep="|"))) + stat_tile_etho()

all_pl_objs <- list()


####### population ethogrames here

# a set of layers or our next big plots
layers <- function(palette = CONTROL_SD_PALETTE, annotate=TRUE){
  out <- list(
	stat_pop_etho(method= mean_cl_boot),
    stat_ld_annotations(),
    coord_cartesian(xlim = c(days(-2),days(3))),
    scale_y_continuous(limits = c(NA,1)),
    scale_fill_manual(values=palette),
    scale_colour_manual(values=palette),
    ethogram_theme
    )
  if(annotate)
    out <- c(out, list(
        annotate("segment",y = .9, yend = .9,   x = hours(7), xend = hours(9),   colour = "black",alpha=0.5,size=3),
        annotate("text",y=0.95,x=hours(8), label="mating")
    ))
    out
}



all_pl_objs$etho_sleep <- ggetho(dt,
                                    aes(y = asleep, fill=mated)) +
                                layers()
                                

all_pl_objs$etho_quiet <- ggetho(dt,
                                    aes(y = !moving, fill=mated)) +
                                layers()

all_pl_objs$etho_xrel <- ggetho(dt,
                                    aes(y = x_rel, fill=mated)) +
                                layers() + scale_y_continuous(limits = c(0,1))
                                
                                

zt_layers <- list(stat_pop_etho(),
								stat_ld_annotations(),
								scale_y_continuous(limits = c(NA,1)),
								scale_fill_manual(values=CONTROL_SD_PALETTE),
								scale_colour_manual(values=CONTROL_SD_PALETTE))
								
all_pl_objs$etho_sleep_post_mating <- ggetho(dt[t %between% c(days(0.5), days(3))],
                                    aes(y = asleep, fill=mated),
                                    time_wrap=hours(24)) + zt_layers

all_pl_objs$etho_quiet_post_mating  <- ggetho(dt[t %between% c(days(0.5), days(3))],
                                    aes(y = !moving, fill=mated),
                                    time_wrap=hours(24)) + zt_layers


pdf("virgin_vs_mated.pdf", w=12,h=6)
for(p_name in names(all_pl_objs)){
    pl <- all_pl_objs[[which(names(all_pl_objs) == p_name)]]
    print(pl + ggtitle(p_name))
#~     p <- plotly::ggplotly(pl)
#~     htmlwidgets::saveWidget(plotly::as_widget(p),
#~                 sprintf("./virgin_vs_mated-%s.html", p_name))
}
dev.off()




dt <- dt[t > days(0.5)]


#### sorted overview plot 
summary_dt <- dt[, .(
					mean_asleep = mean(asleep),
					mean_quiet = mean(!moving)
					) ,by="id"]
setmeta(dt,rejoin(summary_dt))

# behaviour def
dt[, behaviour:= ifelse(max_velocity > THR_W, "w", ifelse(max_velocity > THR,"m","q"))]
dt[, behaviour := ordered(behaviour, levels = c("q","m","w"))]
dt[, behaviour_num := as.numeric(behaviour)]



dt <- dt[,
			{
			dd <- copy(.SD[, .(t, x_rel,behaviour_num)])
			dd[, t := (floor(t/60))*60]
			dd[,
				.(
					x_rel=mean(x_rel),
					walked_dist = sum(abs(diff(x_rel))),
					behaviour = ifelse(all(behaviour_num==1),1,2)
					),
					by="t"]
			},
			by="id"]


dt[, behaviour := ifelse(behaviour != 1, (walked_dist > .25) + 2  , 1)]
dt[, behaviour:= ordered(c("q","m","w")[behaviour], levels = c("q","m","w"))]
dt[, micro.mov. := (behaviour == "m")]
dt[, walking := (behaviour == "w")]
dt[, quiescent := (behaviour == "q")]


ggetho(dt, aes(t, z=micro.mov., y=paste(mated,id, sep="|"))) + stat_tile_etho()

dt[, hour := (floor(t/hours(.25)) * .25) %% 24 ]

#save(dt, file="virgin_mated_dt.RData")

tern_dt <- dt[t>days(0.5)][,
			{
			dd <- copy(.SD[, .(t, x_rel,behaviour)])
			dd[, hour :=  (floor(t/hours(.25)) * .25) %% 24]
			dd[,
				.(
					value = c(as.vector(table(.SD[,behaviour])/.N), mean(x_rel)),
					behaviour = c(levels(behaviour),"x_rel")
					),
					by="hour"]
			},
			by="id"]
			

tern_dt[, behaviour := factor(behaviour, levels = c("q","m","w","x_rel"))]



tern_dt_wide <- behavr(data.table(reshape(tern_dt[!(behaviour  %in% c("x_rel", "entropy"))], 
							timevar = "behaviour", 
							idvar = c("id", "hour"),
							direction = "wide"), key="id"),
					   	tern_dt[meta=T])
setnames(tern_dt_wide, c("value.q", "value.m", "value.w"),c("q","m","w"))					   	

library(ggtern)

tern_dt_wide <- rejoin(tern_dt_wide)

#~ tern_dt_wide[,
#~ 			sleep_group := make_sleep_group(rank_asleep),
#~ 		    by="sex"]
tern_dt_wide_avg <- tern_dt_wide[,.(q=mean(q), w=mean(w), m=mean(m)), by="mated,hour"]
#tern_dt_wide_avg <- tern_dt_wide[,.(q=mean(q), w=mean(w), m=mean(m)), by="sex,hour"]

tern_dt_wide_avg
zt_to_colour <- function(hour){
	colorspace::hex(
					colorspace::HLS(ifelse(hour %% 24 < 12, 360, 180),
									1 - (hour %% 12 /24 + .25),
									1
									))
									
#~ 	colorspace::hex(
#~ 					colorspace::HLS((360 * hour/24 + 90) %% 360,
#~ 									ifelse(hour %% 24 < 12, 0.75, 0.25),
#~ 									1
#~ 									))
}

tern_dt_wide_avg[, col := zt_to_colour(hour)]

												
hack_dt <- tern_dt_wide_avg[hour ==0]
hack_dt[, hour := 24]
tern_dt_wide_avg <- rbind(tern_dt_wide_avg,hack_dt)[, .SD, keyby="mated,hour"]

pdf("virgin_vs_mated-ternary_plots.pdf", w=8,h=8)

colour_clock_dt <- data.table(hour= 0:(24*4) / 4, y=1)		
colour_clock_dt[, col := zt_to_colour(hour)]
ggplot(colour_clock_dt, aes(hour,y)) + 
				coord_polar() +
				scale_x_continuous(limits=c(0,24), breaks=c(1:8 * 3)) + 
				scale_y_continuous(limits=c(0.9,1.1), breaks=numeric(0)) + 
#				geom_point(alpha=.75) + 
				geom_path(size=2,colour=colour_clock_dt$col, arrow=arrow(length=unit(ifelse(colour_clock_dt$hour %% 2 == 0, .7, 0), "cm"))) +
				ggtitle("Colour coding of ZT")

pl <- ggtern(tern_dt_wide_avg[, by="mated,hour"], aes(q,w,m)) + 
		theme_showarrows() + 
		geom_point( size=ifelse(tern_dt_wide_avg$hour %% 1 == 0, 1, NA), colour=tern_dt_wide_avg$col, alpha=.75)+#, shape = (tern_dt_wide_avg$hour > 12) +3) +
		geom_path(alpha = .75, linejoin= "mitre", size=.45,
		#arrow=arrow(angle= 25, length=unit(ifelse(tern_dt_wide_avg$hour %% 1 == 0, .15, 0), "cm")
		colour=tern_dt_wide_avg$col)
pl + facet_wrap(~ mated,ncol=2)


pl <- ggplot(rejoin(dt)[mated==T], aes(x_rel, colour=behaviour, fill=behaviour)) + geom_density(mapping=aes(y=..count../sum(..count..)), alpha=.2)
pl <- pl  + scale_x_continuous(limits=c(0,1)) + scale_y_continuous(name="density MATED", limits=c(0, 0.030))
print(pl)

pl <- ggplot(rejoin(dt)[mated==F], aes(x_rel, colour=behaviour, fill=behaviour)) + geom_density(mapping=aes(y=..count../sum(..count..)), alpha=.2)
pl <- pl  + scale_x_continuous(limits=c(0,1)) + scale_y_continuous(name="density NON MATED", limits=c(0, 0.030))
print(pl)

dev.off()



multiPlot <- function(nrow, ncol, plots, by_row=FALSE) {
  require(grid)
  if(nrow*ncol<length(plots)) {
    stop("to many plots for this layout")
  }
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow, ncol)))
  k <- 1
  if (by_row) {  
    for (i in 1:nrow) {   
      for (j in 1:ncol) {  
        if(k> length(plots)) {
          return()
        }
        my_plot <- plots[[k]]
        print(my_plot, vp = vplayout(i, j))
        k <- k +1
      }
    }
  }
  else{
    for (j in 1:ncol) {   
      for (i in 1:nrow) {  
        if(k> length(plots)) {
          return()
        }
        my_plot <- plots[[k]]
        print(my_plot, vp = vplayout(i, j))
        k <- k +1
      }
    }
    
  }
}



pl <- ggplot(rejoin(tern_dt)[,.(value=mean(value)),by=c("hour", "mated","behaviour")], 
			aes(hour*hours(1), value, colour=mated))+ geom_line() + facet_grid(behaviour ~ .) +scale_fill_manual(values=CONTROL_SD_PALETTE) +
			scale_colour_manual(values=CONTROL_SD_PALETTE) +
 			scale_x_hours() + stat_ld_annotations()
 			
pdf("ternary_plot_video_virgin_mated.pdf", w=8,h=8)
for(h in tern_dt[,sort(unique(hour))]){
	a <- ggtern(tern_dt_wide[hour==h], aes(q,w,m, colour=mated )) + 
		facet_wrap(~mated,ncol=1) + theme_showarrows() +# scale_colour_discrete(guide=FALSE) +
		stat_density_tern(
			geom='polygon',
			aes(alpha=..level..),
			#bins=2,
			base='identity',
			colour=NA,
			fill="black") +		
			geom_point(size=.2,alpha=.3) +  
			scale_alpha_continuous(limits=c(0,100), name= "density") + 
			scale_fill_manual(values=CONTROL_SD_PALETTE) +
			scale_colour_manual(values=CONTROL_SD_PALETTE) 
    
   multiPlot(1,2, list(a,pl + 
                         geom_vline(xintercept=h* hours(1),size=2, alpha=.5)+
                         labs(title="")
                       ))
                       
}

dev.off()


#### sorted overview plot 
summary_dt <- dt[, .(
					mean_asleep = mean(asleep),
					mean_quiet = mean(!moving)
					) ,by="id"]




path_divergeance <- function(d1, d2){
	valid_t <- intersect(d1$hour, d2$hour)
	d1 <- d1[hour %in% valid_t][,.(q,m,w),keyby=hour][, -"hour"]
	d2 <- d2[hour %in% valid_t][,.(q,m,w),keyby=hour][, -"hour"]
	a <- (rowSums(abs(d1 - d2)))
	bc <- rowSums(sqrt(d1 * d2))
	bc <- ifelse(bc == 0, 1, bc)
	bd <- -log(bc)
	return(mean(bd))
	}

#path_divergeance(list_dt[[234]], list_dt[[237]])
<<<<<<< HEAD
<<<<<<< HEAD

=======
>>>>>>> ad95bd0e2e80b63e427104d671dd03862af1b82c
=======
>>>>>>> ad95bd0e2e80b63e427104d671dd03862af1b82c
set.seed(2)
clust_dt <- copy(tern_dt_wide[, .(id,hour, q,m,w)])
clust_dt <- na.omit(clust_dt)
clust_dt <- clust_dt[,hour:=floor(hour)]
setkeyv(clust_dt, c("id", "hour"))
clust_dt <- clust_dt[,
					 .(q = mean(q),
					   m = mean(m),
					   w = mean(w)
					 ), 
					 by=c("id", "hour")]

#~ valid_ids <- c( as.character(sample(dt[sex=="M",id, meta=T], 1, replace=F)),
#~ 				as.character(sample(dt[sex=="F",id, meta=T], 200, replace=F)))
valid_ids <- dt[,id,meta=T]
clust_dt <- clust_dt[id %in% valid_ids]
list_dt <- split(clust_dt, as.character(clust_dt$id))
distances <- proxy::dist(list_dt, method=path_divergeance)
hcl <- hclust(distances, method="average")
dend <- as.dendrogram(hcl)

<<<<<<< HEAD
<<<<<<< HEAD
new_order <- summary_dt[labels(dend), rank(mean_asleep)]
dend2 <- rotate(dend, new_order)


#~ style <- data.table(id = labels(dend2), y= 0)
#~ style[, x:=1:.N]
#~ style <- dt[ meta=T][style,on="id"]
#~ ggdendro::ggdendrogram(dend2, rotate = TRUE, theme_dendro = FALSE) +
#~ 		geom_point(data=style, aes(x=x,y=y, colour=mated))


labels(dend)

pdf("/tmp/test.pdf", w=16, h=9)
pp <- ggdendro::ggdendrogram(dend2, rotate = TRUE, theme_dendro = FALSE) 
style <- data.table(id = labels(dend2), y= 0)
style[, x:=1:.N]
style <- dt[ meta=T][style,on="id"]


label_pivot <- data.table(id = labels(dend2), lab = ordered(labels(dend2), levels=labels(dend2)), key="id")
p0 <- ggplot(summary_dt[label_pivot], aes(y=-mean_asleep, x=lab)) + 
				geom_bar(stat="identity") + coord_flip()
								
p1 <- pp + geom_point(data=style, aes(x=x,y=y, colour=mated))

plot_data <- tern_dt_wide[labels(dend2)]
#~ =======
#~ =======
#~ >>>>>>> ad95bd0e2e80b63e427104d671dd03862af1b82c
#~ re

#~ labels(dend)
#~ labels(dend)

#~ pdf("/tmp/test.pdf", w=16, h=9)
#~ pp <- ggdendro::ggdendrogram(hcl, rotate = TRUE, theme_dendro = FALSE)

#~ style <- data.table(id = hcl$labels[hcl$order], y= 0)
#~ style[, x:=1:.N]
#~ style <- dt[ meta=T][style,on="id"]

#~ p1 <- pp + geom_point(data=style, aes(x=x,y=y, colour=mated))

#~ plot_data <- tern_dt_wide[hcl$labels[hcl$order]]
#~ <<<<<<< HEAD
#~ >>>>>>> ad95bd0e2e80b63e427104d671dd03862af1b82c
#~ =======
#~ >>>>>>> ad95bd0e2e80b63e427104d671dd03862af1b82c
plot_data <- plot_data[,hour:=floor(hour)]
plot_data <- plot_data[,
					 .(q = mean(q),
					   m = mean(m),
					   w = mean(w)
					 ), 
					 by=c("id", "hour")]
plot_data[,id := factor(id, levels = style[,id])]
p2 <- ggplot(plot_data,aes(x=hour, y= id, fill =q )) + geom_tile()
p3 <- ggplot(plot_data,aes(x=hour, y= id, fill =m )) + geom_tile()
p4 <- ggplot(plot_data,aes(x=hour, y= id, fill =w )) + geom_tile()

scale <- list(scale_y_discrete(labels=NULL,breaks=NULL, name=NULL),  guides(fill=FALSE))


multiPlot(1,5,list(	p0 + scale_x_discrete(name=NULL, labels=NULL,breaks=NULL) + guides(fill=F) ,
					p1 + scale_x_continuous(expand=c(0,0), name=NULL, labels=NULL,breaks=NULL) + guides(fill=F) + scale_y_sqrt(),
					p2 + scale,
					p3+ scale,
					p4+ scale
					))
dev.off()



