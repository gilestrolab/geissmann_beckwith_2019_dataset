rm(list=ls());gc()
options(nwarnings = 1000)
library(scopr)
library(ggetho)
library(sleepr)

source("../ggplot_themes.R")
#~ library(ggtern)
#~ library(grid)
#~ library(Gmisc)

FEMALE_VIDEO_TABLE <- "female_videos.csv"
#FEMALE_MALE_PALETTE <- c("#be2828ff", "#282896ff")
FEMALE_MALE_PALETTE <- c("#f57d75ff", "#74a2ceff")
CONTROL_SD_PALETTE <- c( "#969696ff", "#3caa3cff")
METADATA <- "metadata.csv"
CACHE <- "./cache/"
#~ RESULT_DIR <- "./raw_results/"
RESULT_DIR <- "/data/ethoscope_results"
REMOTE_DIR <- "ftp://nas.lab.gilest.ro/auto_generated_data/ethoscope_results/"

# remover 0453|2016−04−04_17−39−52_035aee|17 ?!

THR <- 1
THR_W <- 2.5


met <- fread(METADATA)
met <- met[status == "OK"]
#~ met <- link_ethoscope_metadata_remote(met,
#~                                       remote_dir =  REMOTE_DIR,
#~                                       result_dir = RESULT_DIR,
#~                                       verbose = TRUE)

met_init <- link_ethoscope_metadata(met, result_dir = RESULT_DIR)
                                      
                                                                            
dt <- load_ethoscope(met_init,
					   max_time=days(7),
					   reference_hour=9.0,
					   cache = CACHE,
					   FUN = sleep_annotation,
					   ncores=1)

summary(dt)
                 
                 

   
curate_data <- function(data){
  data[, t := t - days(xmv(baseline_days))]
  data <- data[is_interpolated == F]
	# first we remove animals that do not have enought data points
	valid_animals <- data[,.(t_range = max(t) - min(t)), by=id][t_range >= days(5)]$id
	data <- data[t > days(-2) & 
				 t < days(+2) &
				 id %in% valid_animals]
	data[, t := t+ days(2)]
	data <- data[xmv(sdi)==0]
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


met <- link_ethoscope_metadata(fread(FEMALE_VIDEO_TABLE), result_dir = RESULT_DIR)
new_meta <- met[,.(id,sleep_before,video_machine_name, video_region_id, video_id)][dt[meta=T], on="id"]
setmeta(dt, new_meta)

dt_dam <- load_ethoscope(met_init,
					     max_time=days(7),
					     reference_hour=9.0,
					     cache = CACHE,
					     FUN = sleep_annotation,
					     motion_detector_FUN = sleepr::virtual_beam_cross_detector ,
					   ncores=1)


dt_dam[, t := t - days(xmv(baseline_days))]
dt_dam <- dt_dam[t > days(-2) & 
			 t < days(+2) &
			 id %in% dt[, id, meta=T]
			 ]
dt_dam[, t := t+ days(2)]
summary(dt_dam)

summary_dt_dam <- dt_dam[, .(
					mean_asleep_dam = mean(asleep),
					mean_quiet_dam = mean(!moving),
					mean_asleep_dam_l = mean(asleep[t %% days(1) < hours(12)]),
					mean_quiet_dam_l = mean(!moving[t %% days(1) < hours(12)]),
					mean_asleep_dam_d = mean(asleep[t %% days(1) >= hours(12)]),
					mean_quiet_dam_d = mean(!moving[t %% days(1) >= hours(12)])
					) ,by="id"]


#pl <- ggetho(dt_dam[xmv(sex)=="F"], aes(y=asleep))) + stat_pop_etho()

       
       
       
#########################################################


## 5min rule exple

dt5min <- dt[t > days(3) & t < days(3.5) & xmv(sex)=="F"]

stat_summary_dt5min <- dt5min[,
							.(mean_sleep = mean(asleep),
							  mean_sleep3h = mean(asleep[t< days(3) + hours(3)]),
							  mean_quiet = mean(!moving),
							  mean_quiet3h = mean(!moving[t< days(3) + hours(3)])),
							  by="id"
							]

#~ <<<<<<< HEAD
#~ ggetho(dt5min, aes(t, z=asleep, y=sprintf("%04d|%s", rank_asleep, id))) + stat_tile_etho()
#~ ggetho(dt5min, aes(t, z=!moving, y=sprintf("%04d|%s", rank_asleep, id))) + stat_tile_etho()
#~ =======
ggetho(dt5min, aes(t, z=asleep, y=sprintf("%04d|%s", rank_asleep, id))) + stat_tile_etho()
ggetho(dt5min, aes(t, z=!moving, y=sprintf("%04d|%s", rank_asleep, id))) + stat_tile_etho()
#~ >>>>>>> ad95bd0e2e80b63e427104d671dd03862af1b82c


ggplot(stat_summary_dt5min, aes(mean_sleep)) + geom_histogram(bins=100)
ggplot(stat_summary_dt5min, aes(mean_sleep3h)) + geom_histogram(bins=100) 

ggplot(stat_summary_dt5min, aes(mean_quiet)) + geom_histogram(bins=100)
ggplot(stat_summary_dt5min, aes(mean_quiet3h)) + geom_histogram(bins=100)

bout_dt <- bout_analysis(moving, dt)
bout_dt <- bout_dt[moving ==F]
#~ bout_dt[, phase := ifelse(t%%hours(24) > hours(12), "D", "L")]

#~ ggplot(bout_dt, aes(duration)) + geom_histogram(bins=100) + scale_x_sqrt() + scale_y_sqrt() + geom_vline(xintercept=300, colour = "red") + facet_grid( phase ~ .)

#~ bout_dt <- bout_dt_short[t > days(3) & t < days(3.5)]

#~ stat_summary_dt5min2 <- stat_summary_dt5min[
#~ 								bout_dt_short[,.(
#~ 										n_bouts = .N, 
#~ 										n_sleep_bouts = sum(duration>300),
#~ 										sum_quiet = sum(duration),
#~ 										sum_asleep = sum(duration[duration>300]),
#~ 										longest_bout = max(duration)
#~ 										),by=id]
#~ 								]

#~ ggplot(stat_summary_dt5min2, aes(longest_bout, n_bouts)) + geom_point() + # + scale_x_sqrt() + scale_y_sqrt() + geom_vline(xintercept=300, colour = "red") + facet_grid( phase ~ .)
#~ 									geom_vline(xintercept=300, colour = "red")
#~ ggplot(stat_summary_dt5min2, aes(longest_bout, n_sleep_bouts)) + geom_point() + # + scale_x_sqrt() + scale_y_sqrt() + geom_vline(xintercept=300, colour = "red") + facet_grid( phase ~ .)
#~ 									geom_vline(xintercept=300, colour = "red")

#~ ggplot(stat_summary_dt5min2, aes(sum_asleep/hours(1), sum_quiet/hours(1))) + geom_point() #+ # + scale_x_sqrt() + scale_y_sqrt() + geom_vline(xintercept=300, colour = "red") + facet_grid( phase ~ .)
#~ 									#geom_vline(xintercept=300, colour = "red")


p <- ggetho(bout_dt, aes(y=duration, colour=sex), time_wrap =hours(24))  + facet_wrap(~ sex)

p75  <- ggetho(bout_dt, aes(y=duration, colour=sex),summary_FUN=function(x){quantile(x, .75)}, time_wrap =hours(24))
p50  <- ggetho(bout_dt, aes(y=duration, colour=sex),summary_FUN=function(x){quantile(x, .50)}, time_wrap =hours(24))
p25  <- ggetho(bout_dt, aes(y=duration, colour=sex),summary_FUN=function(x){quantile(x, .25)}, time_wrap =hours(24))

p + stat_pop_etho(data = p25$data)  + 
	 stat_pop_etho(data = p50$data) +
	 stat_pop_etho(data = p75$data) 


####################################################
# behaviour def
dt[, behaviour:= ifelse(max_velocity > THR_W, "w", ifelse(max_velocity > THR,"m","q"))]
dt[, behaviour := ordered(behaviour, levels = c("q","m","w"))]
dt[, behaviour_num := as.numeric(behaviour)]


#### sorted overview plot 
summary_dt <- dt[, .(
					mean_asleep = mean(asleep),
					mean_quiet = mean(!moving),
					mean_micro.mov. = sum(behaviour == "m")/.N,
					mean_asleep_l = mean(asleep[t %% days(1) < hours(12)]),
					mean_quiet_l = mean(!moving[t %% days(1) < hours(12)]),
					mean_micro.mov._l = sum(behaviour[t %% days(1) < hours(12)] == "m")/length(t[t %% days(1) < hours(12)]),
					mean_asleep_d = mean(asleep[t %% days(1) >= hours(12)]),
					mean_quiet_d = mean(!moving[t %% days(1) >= hours(12)]),
					mean_micro.mov._d = sum(behaviour[t %% days(1) >= hours(12)] == "m")/length(t[t %% days(1) >= hours(12)])
					) ,by="id"]

summary_dt <- rejoin(summary_dt)
summary_dt[, rank_asleep := rank(mean_asleep,ties.method = "first"),by="sex"]
summary_dt[, rank_quiet := rank(mean_quiet, ties.method = "first" ),by="sex"]


setmeta(dt,summary_dt)

dt[,has_video := !is.na(sleep_before), meta=T]

layers <- list(stat_tile_etho())
layers_bars <- list(geom_bar(stat="identity", width = 1), coord_flip())

pdf("sorted_baseline_overview.pdf", h=16, w=9)
pl <- ggetho(dt[xmv(sex)=="F"], aes(t, z=asleep, y=sprintf("%04d|%s", rank_asleep, id))) + layers
print(pl)
pl <- ggplot(dt[sex=="F", meta=T], aes(y= mean_asleep, x=sprintf("%04d|%s", rank_asleep, id))) + layers_bars
print(pl)
pl <- ggetho(dt[xmv(sex)=="M"], aes(t, z=asleep, y=sprintf("%04d|%s", rank_asleep, id))) + layers
print(pl)
pl <- ggplot(dt[sex=="M", meta=T], aes(y= mean_asleep, x=sprintf("%04d|%s", rank_asleep, id))) + layers_bars
print(pl)
pl <- ggetho(dt[xmv(sex)=="F"], aes(t, z=!moving, y=sprintf("%04d|%s", rank_quiet, id))) + layers
print(pl)
pl <- ggplot(dt[sex=="F", meta=T], aes(y= mean_quiet, x=sprintf("%04d|%s", rank_quiet, id))) + layers_bars
print(pl)
pl <- ggetho(dt[xmv(sex)=="M"], aes(t, z=!moving, y=sprintf("%04d|%s", rank_quiet, id))) + layers
print(pl)
pl <- ggplot(dt[sex=="M", meta=T], aes(y= mean_quiet, x=sprintf("%04d|%s", rank_quiet, id))) + layers_bars
print(pl)

pl <- ggplot(dt[sex=="F", meta=T], aes(y= mean_asleep, x=sprintf("%04d|%s", rank_asleep, id), fill = has_video)) + layers_bars
print(pl)
pl <- ggetho(dt[xmv(sex)=="F" & xmv(has_video) == T], aes(t, z=asleep, y=sprintf("%04d|%s , %s|%02d", rank_asleep, id, video_id, video_region_id))) + layers
print(pl)
pl <- ggplot(dt[sex=="F", meta=T], aes(y= mean_asleep, x=sprintf("%04d|%s", rank_asleep, id))) + layers_bars
print(pl)

dev.off()



# sleep_dam vs sleep_etho. where are the outliers:

summary_dt <- summary_dt[summary_dt_dam]

pdf("sleep_dam_overestimate.pdf", w=16,h=16)

tmp_layers <- list(			
			geom_point(alpha=.66) ,
			scale_y_continuous(limits = c(0,1)) ,
			scale_x_continuous(limits = c(0,1)) ,
			geom_smooth(method="lm"),
			scale_fill_manual(values=FEMALE_MALE_PALETTE),
			scale_colour_manual(values=FEMALE_MALE_PALETTE))
			
ggplot(summary_dt, aes(mean_asleep_dam, mean_asleep, size = mean_micro.mov., colour=sex)) + 
			tmp_layers
			
ggplot(summary_dt, aes(mean_asleep_dam_l, mean_asleep_l, size = mean_micro.mov._l, colour=sex)) + 
		tmp_layers
ggplot(summary_dt, aes(mean_asleep_dam_d, mean_asleep_d, size = mean_micro.mov._d, colour=sex)) + 
		tmp_layers
		
#~ ggplot(summary_dt, aes(mean_asleep_dam_d, mean_micro.mov._d, colour=sex)) + 
#~ 		tmp_layers
	
#~ ggplot(summary_dt, aes(mean_asleep_d, mean_micro.mov._d, colour=sex)) + 
#~ 		tmp_layers

#~ ggplot(summary_dt, aes(mean_asleep_dam_l, mean_micro.mov._l, colour=sex)) + 
#~ 		tmp_layers
	
#~ ggplot(summary_dt, aes(mean_asleep_d, mean_micro.mov._d, colour=sex)) + 
#~ 		tmp_layers


dev.off()			
			


pdf("overnight_dsd_baseline.pdf", w=12,h=6)
layers <- list(stat_pop_etho(), 
									stat_ld_annotations() ,
<<<<<<< HEAD
									scale_y_continuous(limits = c(0,1)),
=======
									scale_y_continuous(limits = c(NA,1)),
>>>>>>> ad95bd0e2e80b63e427104d671dd03862af1b82c
									scale_fill_manual(values=FEMALE_MALE_PALETTE),
									scale_colour_manual(values=FEMALE_MALE_PALETTE),
									ethogram_theme)
pl <- ggetho(dt, aes(y = asleep, fill=sex)) + layers
print(pl)
pl <- ggetho(dt, aes(y = !moving, fill=sex)) + layers
print(pl)
									
<<<<<<< HEAD
pl_asleep <- ggetho(dt, aes(y = asleep, fill=sex), time_wrap = hours(24)) 
pl_asleep_dam <- ggetho(dt_dam, aes(y = asleep, fill=sex), time_wrap = hours(24)) 
print(pl_asleep + stat_pop_etho(data=pl_asleep_dam$data, linetype=2)  + layers)

pl_immobile <- ggetho(dt, aes(y = !moving, fill=sex), time_wrap = hours(24)) 
pl_immobile_dam <- ggetho(dt_dam, aes(y = !moving, fill=sex), time_wrap = hours(24)) 
print(pl_immobile + stat_pop_etho(data=pl_immobile_dam$data, linetype=2)  + layers)

=======
pl <- ggetho(dt, aes(y = asleep, fill=sex), time_wrap = hours(24)) + layers
print(pl)
pl <- ggetho(dt, aes(y = !moving, fill=sex), time_wrap = hours(24)) + layers                                
print(pl)
>>>>>>> ad95bd0e2e80b63e427104d671dd03862af1b82c



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
# test
pdf("walking_threshold_validation.pdf", w=6, h=6)
ggplot(dt[behaviour != "q" & xmv(sex) == "F"], aes(walked_dist * 60)) + 
			geom_histogram(aes(y=100 * ..count../sum(..count..)), bins=200) + 
			scale_x_sqrt(limits = 60 * c(0, 3), name=expression(Total~distance~moved~(mm.min^{-1})), breaks= 60 * c(0.1, 0.2, 0.5,1,2)) +
			scale_y_continuous(name= "Density (%)") +
			geom_vline(xintercept= 60 *.25, size=2, colour="red") 
dev.off()
# ggplot(dt[behaviour != "q" & xmv(sex) == "M"], aes(walked_dist)) + geom_histogram() + scale_x_log10()+ geom_vline(xintercept=.25)
# ggplot(dt[behaviour != "q" & xmv(sex) == "F"], aes(walked_dist)) + geom_histogram() + scale_x_log10()+ geom_vline(xintercept=.25)

#~ pl <- ggetho(dt[xmv(sex)=="F"], aes(t, z=micro.mov.)) + stat_tile_etho()
#~ print(pl)

colours <- c("#999999ff", "#4e9a06ff", "#0070b0ff")
tdt <- copy(dt[xmv(has_video) == T & t < days(2)])

tdt[ , id2 := sprintf("%04d|%s", xmv(rank_asleep), id)] 

# add sleep fraction in the in the title y label

tdt[ , id2 :=  factor(id2, levels=c(rev(sort(unique(id2)))))] 

pl <- ggplot(tdt, aes(t / hours(1),x_rel)) + 
			geom_rect(mapping=aes(xmin=(t) /hours(1), xmax=(60+t)/hours(1),fill=behaviour), ymin=-1, ymax=1, alpha=.90)+
			geom_line() +
			scale_fill_manual(values=colours) +
			geom_hline(yintercept=.5, linetype=2, alpha=.5, size=2) +
			facet_grid(id2 ~ .) 
			
			
pl <- pl + 	scale_x_continuous(breaks=c(0:8 * 6), name="Time (h)") + 
		scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75), name="Position (rel)") + 
		theme(panel.spacing = unit(0, "cm"), strip.text.y = element_text(angle = -0))+
		coord_cartesian(ylim = c(0,1))
		
tdt <- copy(dt[xmv(sex) == "F" & t < days(2)])		
tdt[ , id2 := sprintf("%04d|%f|%s", xmv(rank_asleep),round(xmv(mean_asleep),4), id)] 
tdt[ , id2 :=  factor(id2, levels=c(rev(sort(unique(id2)))))] 
sorted_id2 <- tdt[,sort(unique(id2))]

id2_groups <- split(sorted_id2,floor(1:length(sorted_id2)/10))

pdf("/tmp/all_female_behaviours.pdf", h=16,w=24)
for(i in  id2_groups){
	
	if(length(i) < 10){
		extra <- paste0("No_Invididual_",(1+length(i)):10)
	}
	else{
		extra <-  character(0)
	}
		
	sdt <- copy(tdt[id2 %in% i])
	levs <- c(as.character(i),extra)
	print(levs)
	sdt[,id2 := factor(as.character(id2), levels=levs)]
	pl <- ggplot(sdt, aes(t / hours(1),x_rel)) + 
			geom_rect(mapping=aes(xmin=(t) /hours(1), xmax=(60+t)/hours(1),fill=behaviour), ymin=-1, ymax=1, alpha=.90)+
			geom_line() +
			scale_fill_manual(values=colours) +
			geom_hline(yintercept=.5, linetype=2, alpha=.5, size=2) +
			facet_grid(id2 ~ .,drop=F) 
		
			
	pl <- pl + 	scale_x_continuous(breaks=c(0:8 * 6), name="Time (h)") + 
		scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75), name="Position (rel)") + 
		theme(panel.spacing = unit(0, "cm"), strip.text.y = element_text(angle = -0))+
		coord_cartesian(ylim = c(0,1))
print(pl)
	}
dev.off()
dt[, hour := (floor(t/hours(.25)) * .25) %% 24 ]
#~ tern_dt_wide[,
#~ 			sleep_group := make_sleep_group(rank_asleep),
#~ 		    by="sex"]
#~ tern_dt_wide_avg <- tern_dt_wide[,.(q=mean(q), w=mean(w), m=mean(m)), by="sleep_group,sex,hour"]



make_sleep_group <- function(rank){
	q <- quantile(rank, probs=0:8/8)
	out <- cut(rank, q, include.lowest=T)
	n <- names(q)
	new_lev <- paste0(n[1: length(n) - 1],"-",n[2: length(n)])
	#new_lev <- n[1: length(n) - 1]
	levels(out) <- new_lev
	out
	}
	
	
dt[, sleep_group := make_sleep_group(rank_asleep), by=sex ,meta=T]

tern_dt <- dt[,
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
tern_dt_wide_avg <- tern_dt_wide[,.(q=mean(q), w=mean(w), m=mean(m)), by="sleep_group,sex,hour"]
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
tern_dt_wide_avg <- rbind(tern_dt_wide_avg,hack_dt)[, .SD, keyby="sleep_group,sex,hour"]

pdf("ternary_plots.pdf", w=8,h=8)

colour_clock_dt <- data.table(hour= 0:(24*4) / 4, y=1)		
colour_clock_dt[, col := zt_to_colour(hour)]
pl <- ggplot(colour_clock_dt, aes(hour,y)) + 
				coord_polar() +
				scale_x_continuous(limits=c(0,24), breaks=c(1:8 * 3)) + 
				scale_y_continuous(limits=c(0.9,1.1), breaks=numeric(0)) + 
#				geom_point(alpha=.75) + 
				geom_path(size=2,colour=colour_clock_dt$col, arrow=arrow(length=unit(ifelse(colour_clock_dt$hour %% 2 == 0, .7, 0), "cm"))) +
				ggtitle("Colour coding of ZT")
print(pl)


#~ pl <- ggtern(tern_dt_wide_avg, aes(q,w,m)) + 
#~ 		theme_showarrows() + 
#~ 		geom_point(size=.2, colour=tern_dt_wide_avg$col, alpha=.75)+#, shape = (tern_dt_wide_avg$hour > 12) +3) +
#~ 		geom_path(alpha = .75, linejoin= "mitre", size=.45, 
#~ 		arrow=arrow(angle= 25, length=unit(ifelse(tern_dt_wide_avg$hour %% 1 == 0, .15, 0), "cm")), 
#~ 		colour=tern_dt_wide_avg$col)
		
#~ pl + facet_grid(sleep_group ~ sex)



for(sg in unique(tern_dt_wide_avg$sleep_group)){
	sdt <- tern_dt_wide_avg[sleep_group == sg]
	pl <- ggtern(sdt, aes(q,w,m)) + 
		theme_showarrows() + 
		geom_point(size=.2, colour=sdt$col, alpha=.75)+#, shape = (tern_dt_wide_avg$hour > 12) +3) +
		geom_path(alpha = .75, linejoin= "mitre", size=.45, 
		arrow=arrow(angle= 25, length=unit(ifelse(sdt$hour %% 1 == 0, .15, 0), "cm")), 
		colour=sdt$col)
		
	print(pl + facet_wrap(~ sex,ncol=2) + ggtitle(paste("percentile group:", sg)))
	}
	
tern_dt_wide_avg2 <- tern_dt_wide_avg[,.(q=mean(q), w=mean(w), m=mean(m)), by="sex,hour,col"]



pl <- ggtern(tern_dt_wide_avg2[, by="sex,hour"], aes(q,w,m)) + 
		theme_showarrows() + 
		geom_point(size=.2, colour=tern_dt_wide_avg2$col, alpha=.75)+#, shape = (tern_dt_wide_avg$hour > 12) +3) +
		geom_path(alpha = .75, linejoin= "mitre", size=.45, 
		arrow=arrow(angle= 25, length=unit(ifelse(tern_dt_wide_avg2$hour %% 1 == 0, .15, 0), "cm")), 
		colour=tern_dt_wide_avg2$col)
pl <- pl + facet_wrap(~ sex,ncol=2)
		
print(pl)

<<<<<<< HEAD
pl <- ggplot(rejoin(dt)[sex=="F"], aes(x_rel, colour=behaviour, fill=behaviour)) + geom_density(mapping=aes(y=..count../sum(..count..)), alpha=.2)
pl <- pl  + scale_x_continuous(limits=c(0,1)) + scale_y_continuous(name="density FEMALES", limits=c(0, 0.0075))
print(pl)

pl <- ggplot(rejoin(dt)[sex=="M"], aes(x_rel, colour=behaviour, fill=behaviour)) + geom_density(mapping=aes(y=..count../sum(..count..)), alpha=.2)
pl <- pl  + scale_x_continuous(limits=c(0,1)) + scale_y_continuous(name="density MALES", limits=c(0, 0.0075))
=======
pl <- ggplot(rejoin(dt), aes(x_rel, colour=behaviour, fill=behaviour))	+ geom_density(alpha=.2)
pl <- pl + facet_wrap( ~ sex,ncol=1) + scale_x_continuous(limits=c(0,1))
>>>>>>> ad95bd0e2e80b63e427104d671dd03862af1b82c
print(pl)
dev.off()
		



############################ for video/gif ####################





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



pl <- ggplot(rejoin(tern_dt)[,.(value=mean(value)),by=c("hour", "sex","behaviour")], 
			aes(hour*hours(1), value, colour=sex))+ geom_line() + facet_grid(behaviour ~ .) +scale_fill_manual(values=FEMALE_MALE_PALETTE) +
			scale_colour_manual(values=FEMALE_MALE_PALETTE) +
 			scale_x_hours() + stat_ld_annotations()
 			
pdf("ternary_plot_video.pdf", w=8,h=8)
for(h in tern_dt[,sort(unique(hour))]){
	a <- ggtern(tern_dt_wide[hour==h], aes(q,w,m, colour=sex )) + 
		facet_wrap(~sex,ncol=1) + theme_showarrows() +# scale_colour_discrete(guide=FALSE) +
		stat_density_tern(
			geom='polygon',
			aes(alpha=..level..),
			#bins=2,
			base='identity',
			colour=NA,
			fill="black") +		
			geom_point(size=.2,alpha=.3) +  
			scale_alpha_continuous(limits=c(0,100), name= "density") + 
			scale_fill_manual(values=FEMALE_MALE_PALETTE) +
			scale_colour_manual(values=FEMALE_MALE_PALETTE) 
    
   multiPlot(1,2, list(a,pl + 
                         geom_vline(xintercept=h* hours(1),size=2, alpha=.5)+
                         labs(title="")
                       ))
                       
}

dev.off()

<<<<<<< HEAD

### clustering:


path_divergeance <- function(d1, d2){
	if(!identical(d1$hour, d2$hour)){
		print(d1)
		print(d2)
		return(1)
		stop("time mismatch")
	}
		
	# Bhattacharyya coefficient 
#~ 	bc <- sapply(c("q", "m", "w"), function(b){
#~ 					sqrt(d1[, b, with=F] * d2[, b, with=F])
#~ 				})
	
	# Bha. distance/ over time
	
#~ 	out <- sum(-log(rowSums(as.data.table(bc)))) / nrow(d1)
	bc <- (
						sqrt(d1[,q] * d2[, q]) + 
						sqrt(d1[,m] * d2[, m]) +
						sqrt(d1[,w] * d2[, w])
						)
	out <- mean(
				-log(
					bc 
					)
				)
				
#~ 	print(d1)
#~ 	print(d2)
#~ 	print(bc)
#~ 	print(out)
#~ 	stop()
 	out <- mean(abs(d1[,q] - d2[, q]) + abs(d1[,m] - d2[, m]) + abs(d1[,w] - d2[, w]))

	#print(paste(id1, id2, out))
	if(is.infinite(out))
		return(1)
	out
	}
	
set.seed(2)
clust_dt <- copy(tern_dt_wide[, .(id,hour, q,m,w)])
clust_dt <- na.omit(clust_dt)
setkeyv(clust_dt, c("id", "hour"))

valid_ids <- c( as.character(sample(dt[sex=="M",id, meta=T], 1, replace=F)),
				as.character(sample(dt[sex=="F",id, meta=T], 200, replace=F)))
clust_dt <- clust_dt[id %in% valid_ids]
list_dt <- split(clust_dt, as.character(clust_dt$id))
distances <- proxy::dist(list_dt, method=path_divergeance)
hcl <- hclust(distances)
cols <- ifelse(dt[names(distances),sex,meta=T] == "F","red", "blue")

pdf("/tmp/test.pdf", w=16, h=9)
pp <- ggdendrogram(hcl, rotate = TRUE, theme_dendro = FALSE)

style <- data.table(id = hcl$labels[hcl$order], y= 0)
style[, x:=1:.N]
style <- dt[ meta=T][style,on="id"]

p1 <- pp + geom_point(data=style, aes(x=x,y=y, colour=sex))

plot_data <- tern_dt_wide[hcl$labels[hcl$order]]
plot_data[,id := factor(id, levels = style[,id])]
p2 <- ggplot(plot_data,aes(x=hour, y= id, fill =q )) + geom_tile()
p3 <- ggplot(plot_data,aes(x=hour, y= id, fill =m )) + geom_tile()
p4 <- ggplot(plot_data,aes(x=hour, y= id, fill =w )) + geom_tile()

scale <- list(scale_y_discrete(labels=NULL,breaks=NULL, name=NULL),  guides(fill=FALSE))

multiPlot(1,4,list(	p1 + scale_x_continuous(expand=c(0,0), name=NULL, labels=NULL,breaks=NULL) + guides(colour=F),
					p2 + scale,
					p3+ scale,
					p4+ scale))


dev.off()
=======
>>>>>>> ad95bd0e2e80b63e427104d671dd03862af1b82c
