rm(list=ls());gc()
options(nwarnings = 1000)
library(scopr)
library(ggetho)
library(sleepr)
library(ggdendro)
library(dendextend)

MY_PALETTE <- c("#969696ff","#3fa83fff","#f6746bff")
GROUP_LEVELS <- c("virgin", "mated", "low_sleeper")
source("../ggplot_themes.R")
                                      
load("baseline_dt.RData")
dt_baseline <- dt[xmv(sex)=="F" & xmv(sleep_group)=="12.5%-25%"]
load("virgin_mated_dt.RData")
dt_virgin_mated <- dt

new_baseline_met <- dt_baseline[, .(id,
									group="low_sleeper",
									mean_asleep), 
								meta=T]
								
setmeta(dt_baseline, new_baseline_met)

new_virgin_met <- dt_virgin_mated[, .(id, group=ifelse(mated, "mated", "virgin"), mean_asleep), meta=T]
setmeta(dt_virgin_mated, new_virgin_met)


dt <- bind_behavr_list(list(dt_virgin_mated, dt_baseline))


dt[,group:= factor(group,level=GROUP_LEVELS), meta=T]


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
tern_dt_wide_avg <- tern_dt_wide[,.(q=mean(q), w=mean(w), m=mean(m)), by="group,hour"]


tern_dt_wide_avg
zt_to_colour <- function(hour){
	colorspace::hex(
					colorspace::HLS(ifelse(hour %% 24 < 12, 360, 180),
									1 - (hour %% 12 /24 + .25),
									1
									))
}

tern_dt_wide_avg[, col := zt_to_colour(hour)]

												
hack_dt <- tern_dt_wide_avg[hour ==0]
hack_dt[, hour := 24]
tern_dt_wide_avg <- rbind(tern_dt_wide_avg,hack_dt)[, .SD, keyby="group,hour"]

pdf("cluster-ternary_plots.pdf", w=8,h=8)
colour_clock_dt <- data.table(hour= 0:(24*4) / 4, y=1)		
colour_clock_dt[, col := zt_to_colour(hour)]
ggplot(colour_clock_dt, aes(hour,y)) + 
				coord_polar() +
				scale_x_continuous(limits=c(0,24), breaks=c(1:8 * 3)) + 
				scale_y_continuous(limits=c(0.9,1.1), breaks=numeric(0)) + 
#				geom_point(alpha=.75) + 
				geom_path(size=2,colour=colour_clock_dt$col, arrow=arrow(length=unit(ifelse(colour_clock_dt$hour %% 2 == 0, .7, 0), "cm"))) +
				ggtitle("Colour coding of ZT")

pl <- ggtern(tern_dt_wide_avg[, by="group,hour"], aes(q,w,m)) + 
		theme_showarrows() + 
		geom_point( size=ifelse(tern_dt_wide_avg$hour %% 1 == 0, 1, NA), colour=tern_dt_wide_avg$col, alpha=.75)+#, shape = (tern_dt_wide_avg$hour > 12) +3) +
		geom_path(alpha = .75, linejoin= "mitre", size=.45,
		#arrow=arrow(angle= 25, length=unit(ifelse(tern_dt_wide_avg$hour %% 1 == 0, .15, 0), "cm")
		colour=tern_dt_wide_avg$col)
pl + facet_wrap(~ group,ncol=3)
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



pl <- ggplot(rejoin(tern_dt)[,.(value=mean(value)),by=c("hour", "group","behaviour")], 
			aes(hour*hours(1), value, colour=group))+ geom_line() + facet_grid(behaviour ~ .)+ 
			scale_fill_manual(values=MY_PALETTE) +
			scale_colour_manual(values=MY_PALETTE) +
 			scale_x_hours() + stat_ld_annotations()
 			
pdf("ternary_plot_cluster_video.pdf", w=8,h=8)
for(h in tern_dt[,sort(unique(hour))]){
	a <- ggtern(tern_dt_wide[hour==h], aes(q,w,m, colour=group )) + 
		facet_wrap(~group,ncol=1) + theme_showarrows() +# scale_colour_discrete(guide=FALSE) +
		stat_density_tern(
			geom='polygon',
			aes(alpha=..level..),
			#bins=2,
			base='identity',
			colour=NA,
			fill="black") +		
			geom_point(size=.2,alpha=.3) +  
			scale_alpha_continuous(limits=c(0,100), name= "density") + 
			scale_fill_manual(values=MY_PALETTE) +
			scale_colour_manual(values=MY_PALETTE) 
    
   multiPlot(1,2, list(a,pl + 
                         geom_vline(xintercept=h* hours(1),size=2, alpha=.5)+
                         labs(title="")
                       ))
                       
}

dev.off()

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

#fixme
#dt[,mean_asleep:= runif(.N), meta=T]

new_order <- dt[labels(dend), rank(mean_asleep), meta=T]
dend2 <- dendextend::rotate(dend, new_order)


pdf("clustering_results.pdf", w=16, h=9)
pp <- ggdendro::ggdendrogram(dend2, rotate = TRUE, theme_dendro = FALSE) 
style <- data.table(id = labels(dend2), y= 0)
style[, x:=1:.N]
style <- dt[ meta=T][style,on="id"]


label_pivot <- data.table(id = labels(dend2), lab = ordered(labels(dend2), levels=labels(dend2)), key="id")
p0 <- ggplot(dt[meta=T][label_pivot], aes(y=-mean_asleep, x=lab, fill=group)) + 
				geom_bar(stat="identity") + coord_flip()
								
p1 <- pp + geom_point(data=style, aes(x=x,y=y, colour=group))

plot_data <- tern_dt_wide[labels(dend2)]
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

multiPlot(1,5,list(	p0 + scale_x_discrete(name=NULL, labels=NULL,breaks=NULL) + guides(fill=F) + scale_fill_manual(values=MY_PALETTE),
					p1 + scale_x_continuous(expand=c(0,0), name=NULL, labels=NULL,breaks=NULL) + guides(fill=F) + scale_y_sqrt()+ 
						scale_colour_manual(values=MY_PALETTE),
					p2 + scale,
					p3+ scale,
					p4+ scale
					))

dev.off()






met <- dt[meta=T]
colbranches <- function(n){
	tmp_labs <- labels(n)
	new_col <- unique(MY_PALETTE[met[tmp_labs,group]])
    if(length(new_col) >1)
        return(n)
  a <- attributes(n) # Find the attributes of current node
  attr(n, "edgePar") <- c(a$edgePar, list(col=new_col, lwd=2))
  n
  }

dend2 <- dendextend::rotate(dend, new_order)
dend2 <- dendrapply(dend2, colbranches)
N <- nrow(dt[meta=T])
l=numeric(N*2)
l[1] <- "black"
i=2
a <- dendrapply(dend2, function(x){
    o = attributes(x)$edgePar$col
    print(o)
    o <- ifelse(is.null(o), "black", o)
    l[i] <<- o
    i <<- i+1
    })





library(ape)
pdf("unrooted_dendro.pdf")
tree <- as.phylo(dend2)

tree$tip.label <- rep(".", times=N)
cols <- MY_PALETTE[met[labs,group]]
plot(tree, show.tip.label=T, lab4ut="axial", tip.color = cols , edge.color=l[3:(N*2)], cex=1.5, type="unrooted")
dev.off()

#~ obj <- as.phylo(dend2)
#~ labs <- obj$tip.label
#~ cols <- MY_PALETTE[dt[labs,group,meta=T]]
#~ obj$tip.label <- rep(".", times=length(labs))

#~ pdf("fan_dendro.pdf")
#~ plot(obj,type="unrooted", tip.color = cols,font=2., cex=1.5, edge.width=1)
#~ dev.off()




#~ library(dendextend)
#~ library(circlize)



#~ dend <- dend %>% 
#~   color_branches(k=num_clades, col=rainbow) %>% 
#~   color_labels(k=num_clades, col=rainbow)

#~ par(mar = rep(0, 4))
#~ circlize_dendrogram(dend, dend_track_height = 0.8) 
