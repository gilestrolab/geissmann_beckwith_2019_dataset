rm(list=ls());gc()
options(nwarnings = 1000)
library(scopr)
library(ggetho)
library(sleepr)
#~ library(gtools)
source("../ggplot_themes.R")

FEMALE_MALE_PALETTE <- c("#be2828ff", "#282896ff")
CONTROL_SD_PALETTE <- c( "#969696ff", "#3caa3cff")
METADATA <- "metadata.csv"
CACHE <- "./cache/"
#~ RESULT_DIR <- "./raw_results/"
RESULT_DIR <- "/data/ethoscope_results/"
REMOTE_DIR <- "ftp://nas.lab.gilest.ro/auto_generated_data/ethoscope_results/"

met <- fread(METADATA)
#met <- met[status == "OK"]

#~ met <- link_ethoscope_metadata_remote(met,
#~                                       remote_dir =  REMOTE_DIR,
#~                                       result_dir = RESULT_DIR,
#~                                       verbose = TRUE)




met <- link_ethoscope_metadata(met,
                                      result_dir = RESULT_DIR)                                                                            
dt <- load_ethoscope(met,
					   max_time=days(20),
					   reference_hour=9.0,
					   cache = CACHE,
					   FUN = sleep_annotation,
					   ncores=1)

summary(dt)
    
dt_stchd <- stitch_on(dt, on="unique_ID") 
dt_stchd[, walking := max_velocity > 2.5]



dt_stchd <- dt_stchd[xmv(fly_obs) != "food"]
dt_stchd <- dt_stchd[xmv(days_alive) != "non"]
dt_stchd[, days_alive := as.numeric(days_alive), meta=T]
dt_stchd[, right_censored := ifelse(fly_obs == "ok", F, T), meta=T]

dt_stchd[, max_t := days(days_alive) - days(1), meta=T]


pl1 <- ggetho(dt_stchd, aes(z=asleep)) + stat_tile_etho() + scale_x_days(limits=c(0,days(60)))
pl2 <- ggetho(dt_stchd[t < xmv(max_t)], aes(z=asleep)) + stat_tile_etho() + scale_x_days(limits=c(0,days(60)))

pl3 <- ggetho(dt_stchd[t < xmv(max_t)], aes(z=asleep, y=interaction(id, days_alive, treatment, sex))) + stat_tile_etho() + scale_x_days()
pdf("./tile_plots.pdf", w=32, h=16)
pl1
pl2
pl3
dev.off()


dt_stchd_curated <- dt_stchd[t < days(xmv(days_alive))]

palette <- CONTROL_SD_PALETTE

layers <- list(
    #stat_pop_etho(method= mean_cl_boot),
    stat_pop_etho(),
    facet_grid( sex ~ .),
    stat_ld_annotations(),
#    coord_cartesian(xlim = c(days(-3),days(12))),
    scale_y_continuous(limits = c(NA,1)),
    scale_fill_manual(values=palette),
    scale_colour_manual(values=palette), 
    ethogram_theme
    )

all_pl_objs <- list()
all_pl_objs$etho_sleep <- ggetho(dt_stchd_curated,
                                    aes(y = asleep, fill=treatment)) +
									layers





dt_stchd_curated[, t := t - days(xmv(baseline))]


#ggetho(dt_stchd_curated, aes(z=asleep, y=interaction(id, days_alive, treatment, sex))) + stat_tile_etho()
stat_rebound_dt <- rejoin(
						dt_stchd_curated[,
						.(
						overall_sleep = mean(asleep[t > hours(12)]),
						overall_sleep_10d = mean(asleep[t > hours(12) & t < days(10)]),
						interactions = sum(interactions)
						)
						,by = id]
						)




ggplot(stat_rebound_dt, aes()) + ()

ggplot(stat_rebound_dt, aes(x=overall_sleep_10d, y=days_alive, colour=sex, shape=sex)) + 
				geom_point() + 
				geom_smooth(method="lm") +
				facet_grid( treatment ~ .)





##############################################################################
mod <- lm( days_alive ~ treatment * sex,dt_stchd[right_censored == F, meta=T])
summary(mod)

mod <- lm( days_alive ~ treatment ,dt_stchd[right_censored == F, meta=T])
summary(mod)


mod <- lm( days_alive ~ treatment ,dt_stchd[right_censored == F & sex=="female", meta=T])
summary(mod)






library(survminer)
library(survival)

#surv_data <- fread("/tmp/prolonged_sd_stat_dt.csv")
surv_data <- dt_stchd[meta=T]
# animals are 2 days old when we start recording. 
surv_data[, days_alive := days_alive + 2]

surv_data[, dead := 2- right_censored ] # dead = 2, censored = 1
#surv_data[, lifespan_baseline := lifespan - baseline_days]
#surv_data[, lifespan_baseline := ifelse(is.infinite(lifespan), 10,lifespan - baseline_days)]

s <- survfit(Surv(days_alive, dead) ~ sex + treatment, data = surv_data)
ggsurv <- ggsurvplot_facet(s, data=surv_data, conf.int = TRUE, palette=rep(CONTROL_SD_PALETTE, 2), facet.by="sex", nrow=2) + theme_grey()

pdf("sd_until_death_surv.pdf", w=12,h=6)
ggsurv + scale_y_continuous(labels = scales::percent) + scale_x_continuous(name="age (d)") + 
		 geom_vline(xintercept=6, size=1, linetype=2)	+
		 coord_cartesian(xlim=c(0,55) )
dev.off()



#~ ggetho(dt_stchd[xmv(incubator) == 8], aes(y=paste(incubator,sex, id), z=walking)) + 
#~         stat_tile_etho() + 
#~         stat_ld_annotations()
        
#~ crashed_etho_ids <- dt_stchd[ inc_crash != "no" | machine_name == "ETHOSCOPE_024",id,meta=T]
#~ dt_stchd <- dt_stchd[!id %in% crashed_etho_ids]

dt_stchd[, death_date := as.POSIXct(sprintf("%s-2018 09:00:00", week_death), format="%d-%m-%Y %H:%M:%S", tz="UTC"), meta=T]
dt_stchd[, max_t := as.numeric(death_date - as.POSIXct(paste(as.character(as.Date(datetime)),"09:00:00"), tz="UTC"), unit="secs"),meta=T]
dt_stchd[, max_t := ifelse(is.na(max_t),+Inf, max_t), meta=T]
#todo fix death date in 2018-03-05_19-48-56_033d6b|09
dt_stchd[,valid := t < xmv(max_t)]




pdf("/tmp/tmp.pdf", h=48,w=16)

ggetho(dt_stchd[valid ==T], aes(y=paste(incubator,sex, id), z=walking)) + 
        stat_tile_etho() + 
        stat_ld_annotations()
dev.off()


dt_stchd <- dt_stchd[valid == T]
last_read_dt <- dt_stchd[, .(last_read_t = max(t)), by=id]
new_meta <- dt_stchd[meta=T][last_read_dt]
setmeta(dt_stchd, new_meta)


dt_stchd[, last_point_to_keep := last_read_t - days(10), meta=T ]




pdf("/tmp/overview.pdf", h=48,w=16)

ggetho(dt_stchd, aes(y=paste(incubator,sex, id), z=walking)) + 
        stat_tile_etho() + 
        stat_ld_annotations()
dev.off()

dt_stchd[, x_rel := ifelse(xmv(region_id) >10, 1-x, x)]

loop_dt <- dt_stchd[t > xmv(last_point_to_keep)]
i <- loop_dt[,id,meta=T][5]


for(i in loop_dt[,id,meta=T]){
	png(sprintf("/tmp/individual_traces/%s.png",i), width=1920 *5, height=1080, units="px", res= 300)
	print(i)
	pl <- ggplot(loop_dt[id==i], aes(t, x)) + geom_line(size=.2) + geom_point(size=.5, aes(colour=interactions)) + scale_x_days() + stat_ld_annotations()
	pl <- pl + ggtitle(i)
	print(pl)
	dev.off()
}


ggplot(loop_dt[id==i], aes(t, x)) + geom_line() + scale_x_hours()


####################################
tdt <- dt_stchd[valid == T & is_interpolated ==F]
#tdt <- curate_dead_animals(dt_stchd, moving, time_window=hours(6))
tdt <- curate_dead_animals(dt_stchd, walking, time_window=hours(12), prop_immobile=0.03)

p1 <- ggetho(dt_stchd, aes(y=paste(sex, id), z= moving)) 
p1 <- p1 + stat_tile_etho() + 
        stat_ld_annotations()

p2 <- ggetho(tdt, aes(y=paste(sex, id), z= moving)) 
p2 <- p2 + stat_tile_etho() + 
        stat_ld_annotations()

pdf("/tmp/overview_moving.pdf", h=32, w=16)
p1
p2
dev.off()

exple <- c("2018-03-05_19-48-56_033d6b|17",
		  "2018-03-05_19-48-54_050d6b|09", 
		  "2018-03-05_19-48-54_050d6b|03",
		 "2018-03-05_19-48-54_050d6b|09", 
		"2018-03-05_19-48-54_050d6b|16",
		"2018-03-05_19-48-54_050d6b|18")
	
	
pdf("/tmp/exples.pdf", h=9, w=16)
for(e in exple){
	print(e)
	dd <- tdt[id == e]
	
	dd <- bin_apply(dd, y=x, x_bin_length=mins(2), FUN=sd)
	o <- ggplot(dd, aes(x=t, y=floor(x*100))) + geom_raster()
	print(o)
	
}
dev.off()	



p2 <- ggetho(tdt, aes(y= interactions, colour=sex) )
p2 <- p2+     stat_pop_etho() + facet_grid(treatment ~ .)
        stat_ld_annotations()
        
p2 + coord_cartesian(xlim=c(days(20),days(22)), ylim=c(1,4)) 
		
curate_data <- function(data){
  
	data <- data[is_interpolated == F]
	data[, treatment := as.factor(ifelse(sdi == 0, "Control", "SD")), meta=T]
	data[, interval := round(((11-sdi) ^ 1.7)) * 20, meta=T]	
    data[, interval := plyr::revalue(as.factor(interval),c("1180"="Control")), meta=T]
	data[, death_date := ifelse(!is.na(as.Date(death_date)), as.Date(death_date),Inf), meta=T]
	data[, lifespan := as.numeric(death_date) - as.numeric(as.Date(datetime)), meta=T]	
	data <- data[t < days(xmv(lifespan)- xmv(baseline_days))]
	data[, t := t - days(xmv(baseline_days))]
	data <- data[t < days(14)]
	

}

# then we apply this function to our data
dt <- curate_data(dt)



#~ pl

#~ ids <- c("2017-05-08_15-43-20_027c6b|14",#0
#~ 		 "2017-05-08_15-43-20_027c6b|02",#7
#~ 		 "2017-02-09_12-00-32_052c6b|10", # NA
#~ 		 "2017-02-09_11-59-39_047c6b|14",#3
#~ 		 "2017-02-09_11-59-39_047c6b|05",# 0
#~ 		 "2017-02-09_11-56-18_040d6b|10",# 5
#~ 		 "2017-02-09_11-56-14_018c6b|19") #10

#~ pl <- ggetho(dt[is.infinite(xmv(death_date)) & id %in% ids   ], aes(y=paste(as.Date(datetime),  id), z=asleep)) + 
#~ 			stat_tile_etho() 

		 
#~ # we have a look at our resulting data
#~ print(dt)
#~ summary(dt)


#~ sdt <- dt[is.infinite(xmv(death_date)) & xmv(uid) < 321]
#~ pl <- ggetho(sdt, aes(y=paste(as.Date(datetime),  id, uid), z=asleep)) + 
#~ 			stat_tile_etho() + stat_ld_annotations()


dates <- as.character(sort(unique(as.Date(dt[, meta=T]$datetime))))
#dt[, id2 := interaction(region_id,machine_name, date), meta=T]
pdf("overview_all_experiments.pdf", width=9, h=16)
for (d in dates){
  print(d)
  sdt <- dt[as.Date(xmv(datetime)) == d]
  pl <- ggetho(sdt, aes(y=paste(sex, id), z=asleep)) + 
        stat_tile_etho() + 
        ggtitle(d) +
        stat_ld_annotations()
        
  print(pl)
}

dev.off()


all_pl_objs <- list()


# a set of layers or our next big plots
layers <- function(palette = CONTROL_SD_PALETTE, annotate=TRUE){
  out <- list(
    stat_pop_etho(method= mean_cl_boot),
    facet_grid( sex ~ .),
    stat_ld_annotations(),
    coord_cartesian(xlim = c(days(-3),days(12))),
    scale_y_continuous(limits = c(NA,1)),
    scale_fill_manual(values=palette),
    scale_colour_manual(values=palette), 
    ethogram_theme
    )
  if(annotate)
    out <- c(out, list(
        annotate("segment",y = .9, yend = .9,   x = days(.5), xend = days(10),   colour = "black",alpha=0.5,size=3),
        annotate("text",y=0.95,x=days(9.5/2), label="treatment")
    ))
    out
}





zoomed_cartesian <-  coord_cartesian(xlim = c(days(9.5),days(11)))

all_pl_objs$etho_sleep <- ggetho(dt[xmv(sdi) %in% c(0,10)],
                                    aes(y = asleep, fill=treatment)) +
									layers()

all_pl_objs$etho_sleep_zoom <- all_pl_objs$etho_sleep +
                                  zoomed_cartesian

all_pl_objs$etho_quiet <- ggetho(dt[xmv(sdi) %in% c(0,10)],
                                    aes(y = !moving, fill=treatment)) +
									layers()
all_pl_objs$etho_quiet_zoom <- all_pl_objs$etho_quiet +
                                    zoomed_cartesian


all_pl_objs$etho_stimuli <- ggetho(dt[xmv(sdi) %in% c(0,10)],
                                    aes(y = interactions, fill=treatment),
                                    summary_FUN = sum) +
                                  layers() + scale_y_continuous(limits = c(NA,NA))

all_pl_objs$etho_stimuli_zoom <- all_pl_objs$etho_stimuli +
                                        zoomed_cartesian
                                        

dt[, interval := as.numeric(as.character(xmv(interval)))]



all_pl_objs$etho_stimuli_rel_ovrw <- ggetho(dt[t > days(1) &  t < days(10) & xmv(treatment) == "SD"],
                                    aes(z = (interactions / 10) / (1/as.numeric(interval))),
                                    summary_FUN = mean) +
                                    stat_tile_etho()
                                    

all_pl_objs$etho_stimuli_rel <- ggetho(dt[t > days(1) &  t < days(10) & xmv(treatment) == "SD"],
                                    aes(y = (interactions / 10) / (1/as.numeric(interval))),
                                    summary_FUN = mean, time_wrap= hours(24)) +
                                  layers(annotate=F) + scale_y_continuous(name="N_interactions / N_maximum" ,limits = c(NA,NA)) + coord_cartesian(xlim = c(days(0),days(1)))


all_pl_objs$etho_stimuli_rel_day_7_to_10 <- ggetho(dt[t > days(7) &  t < days(10) & xmv(treatment) == "SD"],
                                    aes(y = (interactions / 10) / (1/as.numeric(interval))),
                                    summary_FUN = mean, time_wrap= hours(24)) +
                                  layers(annotate=F) + scale_y_continuous(name="N_interactions / N_maximum" ,limits = c(NA,NA)) + coord_cartesian(xlim = c(days(0),days(1)))


dt[, distance := abs(c(0, diff(x))), by=id]

all_pl_objs$etho_distance <- ggetho(dt[xmv(sdi) %in% c(0,10)],
                                    aes(y = distance, fill=treatment),
                                    summary_FUN = sum) +
                                  layers() + scale_y_continuous(limits = c(NA,NA))

all_pl_objs$etho_distance_wrapped <- ggetho(dt[t > days(1) &  t < days(10) & xmv(sdi) %in% c(0,10)],
                                    aes(y = distance / 9, fill=treatment),
                                    summary_FUN = sum,  time_wrap= hours(24)) +  
									layers() + scale_y_continuous(limits = c(NA,NA)) +
									coord_cartesian(xlim = c(days(0),days(1)))

all_pl_objs$etho_distance_wrapped_day_7_to_10 <- ggetho(dt[t > days(7) &  t < days(10) & xmv(sdi) %in% c(0,10)],
                                    aes(y = distance / 3, fill=treatment),
                                    summary_FUN = sum,  time_wrap= hours(24)) +  
									layers() + scale_y_continuous(limits = c(NA,NA)) +
									coord_cartesian(xlim = c(days(0),days(1)))







#~ all_pl_objs$etho_stimuli_zoom <- all_pl_objs$etho_stimuli +
#~                                         zoomed_cartesian
                                        
                                        
                                        

################### scalar stats here and barplots (1 value/animal)

# shared layers
layer_barpl <- function(){
	list(
		facet_grid(sex ~ .),
		geom_jitter(alpha=.3, height=0),
		stat_summary(fun.y = "mean", geom="point", shape = 4, size=2.5, colour="black"),
		stat_summary(fun.data = "mean_cl_boot", geom="errorbar",colour="black"),
		scale_fill_manual(values=CONTROL_SD_PALETTE),
		scale_colour_manual(values=CONTROL_SD_PALETTE), generic_theme
		)
	}
	
	
stat_rebound_dt <- na.omit(rejoin(
						dt[,
						.(
						overall_sleep = mean(asleep),
						sleep_baseline_day = mean(asleep[t %between%  hours(c(0, 12))]),
						sleep_baseline_night = mean(asleep[t %between% hours(c(-12, 0))]),
						sleep_baseline_day3h = mean(asleep[t %between% hours(c(0, 3))]),
						quiet_baseline_day3h = mean(!moving[t %between% hours(c(0, 3))]),
						sleep_baseline_day6h = mean(asleep[t %between% hours(c(0, 6))]),
						sleep_baseline_all = mean(asleep[t %between% hours(c(-12, 12))]),
						sleep_rebound_day3h = mean(asleep[t %between% (days(9) + hours(c(24, 24 +3)))]),
						quiet_rebound_day3h = mean(!moving[t %between% (days(9) + hours(c(24, 24 +3)))]),
						sleep_rebound_day6h = mean(asleep[t %between% (days(9) + hours(c(24, 24 +6)))]),
						quiet_rebound_day6h = mean(!moving[t %between% (days(9) + hours(c(24, 24 +6)))]),
						interactions = sum(interactions)
						)
						,by = id]
						))


	
all_pl_objs$bar_sleep_reb_day3h <- ggplot(stat_rebound_dt, aes(interval, sleep_rebound_day3h, colour=treatment)) + layer_barpl()
all_pl_objs$bar_sleep_reb_day6h <- ggplot(stat_rebound_dt, aes(interval, sleep_rebound_day6h, colour=treatment)) + layer_barpl()
all_pl_objs$bar_quiet_reb_day3h <- ggplot(stat_rebound_dt, aes(interval, quiet_rebound_day3h, colour=treatment)) + layer_barpl()
all_pl_objs$bar_quiet_reb_day6h <- ggplot(stat_rebound_dt, aes(interval, quiet_rebound_day6h, colour=treatment)) + layer_barpl()
all_pl_objs$bar_interactions <- ggplot(stat_rebound_dt, aes(interval, interactions, colour=treatment)) + layer_barpl() + scale_y_sqrt()

# quiet is more linear!!
all_pl_objs$bar_quiet_reb_day3h <- ggplot(stat_rebound_dt, aes(interval, quiet_rebound_day3h, colour=treatment)) + layer_barpl()
all_pl_objs$bar_quiet_reb_day3h <- ggplot(stat_rebound_dt, aes(interval, quiet_rebound_day3h, colour=treatment)) + layer_barpl()
all_pl_objs$bar_quiet_reb_day6h <- ggplot(stat_rebound_dt, aes(interval, quiet_rebound_day6h, colour=treatment)) + layer_barpl()






ggplot(stat_rebound_dt, aes(x=quiet_baseline_day3h, y=quiet_rebound_day3h)) + geom_point(alpha=.3) + facet_grid(sex ~ .) + scale_x_sqrt()+ scale_y_sqrt()

ggplot(stat_rebound_dt[sdi %in% c(0, 10)], aes(x=quiet_baseline_day3h, y=quiet_rebound_day3h, colour=treatment)) + geom_point(alpha=1) + facet_grid(sex ~ .) + 
			#geom_smooth(data=stat_rebound_dt[treatment=="Control"])
			geom_smooth(method = "lm")




mod <- lm(quiet_rebound_day3h ~ quiet_baseline_day3h * sex, stat_rebound_dt[sdi == 0])

stat_rebound_dt[, quiet_rebound_day3h_pred := predict(mod, stat_rebound_dt)]
stat_rebound_dt[, quiet_rebound_day3h_diff := quiet_rebound_day3h - quiet_rebound_day3h_pred]
stat_rebound_dt[, quiet_rebound_day3h_pred_sign := quiet_rebound_day3h_diff > 0]



all_pl_objs$bar_quiet_reb_day3h_min_baseline <- ggplot(stat_rebound_dt, aes(interval, quiet_rebound_day3h - quiet_baseline_day3h, colour=treatment)) + layer_barpl()
all_pl_objs$bar_quiet_reb_day3h_diff <- ggplot(stat_rebound_dt, aes(interval, quiet_rebound_day3h_diff * 3 * 60, colour=treatment)) + layer_barpl() +
						geom_hline(yintercept=0, colour="red", linetype=2) + scale_y_continuous(name="Extra quiescence in 3h (min)")


summ_stat <- stat_rebound_dt[,{
				w = wilcox.test(quiet_rebound_day3h_diff, mu=0, alternative="greater")
				list(pval=w$p.value, n=.N)},
				keyby="sex,interval,treatment"]
summ_stat[, text:=sprintf("%s\nN=%03d", stars.pval(pval), n)]
all_pl_objs$bar_quiet_reb_day3h_diff <- all_pl_objs$bar_quiet_reb_day3h_diff + geom_label(data= summ_stat, aes(x=interval, label=text),  y=-0.25 * 180, colour="black")


all_pl_objs$bar_quiet_has_reb <- ggplot(stat_rebound_dt, aes(interval, as.numeric(quiet_rebound_day3h_pred_sign > 0), colour=treatment)) + layer_barpl() + 
						scale_y_continuous(labels = scales::percent, name="Animals with positive rebound")	 +
						geom_hline(yintercept=0.5, colour="red", linetype=2)

summ_stat <- stat_rebound_dt[,{
				w= binom.test(sum(quiet_rebound_day3h_pred_sign),.N, alternative="greater")		
				list(
				pval=w$p.value, 
				n=.N, s=sum(quiet_rebound_day3h_pred_sign))},
				keyby="sex,interval,treatment"]
				
summ_stat[, text:=sprintf("%s\nN=%03d", stars.pval(pval), n)]

all_pl_objs$bar_quiet_has_reb <- all_pl_objs$bar_quiet_has_reb  + geom_label(data= summ_stat, aes(x=interval, label=text),  y=0.25, colour="black")


pdf("rebound_quantif.pdf", w=8,h=8)
all_pl_objs$bar_quiet_reb_day3h 
all_pl_objs$bar_quiet_reb_day3h_min_baseline
all_pl_objs$bar_quiet_reb_day3h_diff 
all_pl_objs$bar_quiet_has_reb
dev.off()



library(survminer)
library(survival)

#surv_data <- fread("/tmp/prolonged_sd_stat_dt.csv")
surv_data <- dt[meta=T]
surv_data[, dead := 1+ !is.infinite(lifespan)] # dead = 2, censored = 1
surv_data[, lifespan_baseline := lifespan - baseline_days]
surv_data[, lifespan_baseline := ifelse(is.infinite(lifespan), 10,lifespan - baseline_days)]
s <- survfit(Surv(lifespan_baseline, dead) ~ sex + treatment, data = surv_data)
ggsurv <- ggsurvplot(s, data=surv_data, conf.int = TRUE, palette=rep(CONTROL_SD_PALETTE, 2))
pl <- ggsurv$plot  + facet_grid( sex ~ .) + theme_grey()
pl <- pl + annotate("segment",y = .70, yend = .70,   x = .5, xend = 10,   colour = "black",alpha=0.5,size=3) +
        annotate("text",y=0.75,x= 9.5/2, label="treatment") +
        scale_y_continuous(labels = scales::percent)	
        
pdf("prolonged_sd_surv.pdf", w=12,h=6)
pl
dev.off()


stat_rebound_dt <- na.omit(rejoin(
						dt[,
						.(
						sleep_baseline_day = mean(asleep[t %between%  hours(c(0, 12))]),
						sleep_baseline_night = mean(asleep[t %between% hours(c(-12, 0))]),
						sleep_baseline_day3h = mean(asleep[t %between% hours(c(0, 3))]),
						quiet_baseline_day3h = mean(!moving[t %between% hours(c(0, 3))]),
						sleep_baseline_day6h = mean(asleep[t %between% hours(c(0, 6))]),
						sleep_baseline_all = mean(asleep[t %between% hours(c(-12, 12))]),
						sleep_rebound_day3h = mean(asleep[t %between% (days(9) + hours(c(24, 24 +3)))]),
						quiet_rebound_day3h = mean(!moving[t %between% (days(9) + hours(c(24, 24 +3)))]),
						sleep_rebound_day6h = mean(asleep[t %between% (days(9) + hours(c(24, 24 +6)))]),
						quiet_rebound_day6h = mean(!moving[t %between% (days(9) + hours(c(24, 24 +6)))]),
						overall_sleep = mean(asleep[t %between% days(c(-3, 9))]),
						interactions = sum(interactions)
						)
						,by = id]
						))



corr_data <- stat_rebound_dt[surv_data[, list(id, lifespan_baseline)]][is.finite(lifespan)]


pdf("prolonged_sd_correlations.pdf")
ggplot(corr_data[treatment == "Control"], aes(x=overall_sleep, y=lifespan_baseline, colour=sex, shape=sex)) + 
				geom_point() + 
				geom_smooth(method="lm") 

corr_dt <- corr_data[treatment == "Control" & sex=="M"]				
cor.test(corr_dt$lifespan_baseline, corr_dt$overall_sleep, method="spearman")
corr_dt <- corr_data[treatment == "Control" & sex=="F"]				
cor.test(corr_dt$lifespan_baseline, corr_dt$overall_sleep, method="spearman")


ggplot(corr_data[treatment != "Control"], aes(x=interactions, y=lifespan_baseline, colour=sex, shape=sex)) + 
				geom_point() + 
				geom_smooth(method="lm") 

corr_dt <- corr_data[treatment != "Control" & sex=="M"]				
cor.test(corr_dt$lifespan_baseline, corr_dt$interactions, method="spearman")
corr_dt <- corr_data[treatment != "Control" & sex=="F"]				
cor.test(corr_dt$lifespan_baseline, corr_dt$interactions, method="spearman")

dev.off()


stim_dt <- dt[t > days(1) &  t < days(10) & xmv(treatment) == "SD"]

stim_dt <-behavr::bin_apply_all(stim_dt, y=interactions, x_bin_length=mins(30), FUN=sum)

stim_simple_dt <- rejoin(stim_dt)[, .(N_stimuli = mean(interactions)), by="t,sex"]

ggplot(stim_simple_dt, aes(x=t, y=N_stimuli, colour=sex)) + geom_line()


tmp_ts <- ts(stim_simple_dt[sex=="M", N_stimuli],  frequency=48) # 48 obervations a day
s <- stl(tmp_ts,s.window="per")
apply(s$time.series,2, var) / var(tmp_ts)
plot(s)
ts <- s$time.series
dt_f <- data.table(interactions = ts[, "trend"], t = time(ts) * days(1), sex="M")

tmp_ts <- ts(stim_simple_dt[sex=="F", N_stimuli],  frequency=48) # 48 obervations a day
s <- stl(tmp_ts,s.window="per")
apply(s$time.series,2, var) / var(tmp_ts)
plot(s)
ts <- s$time.series
dt_m <- data.table(interactions = ts[, "trend"], t = time(ts) * days(1), sex="F")


all_pl_objs$etho_stimuli_2 <- ggetho(dt[xmv(sdi) == 10 & t > days(1) & t < days(10)],
                                    aes(y = interactions, fill=sex),
                                    summary_FUN = sum) +
                                    stat_pop_etho(method= mean_cl_boot, linetype=2) +
									stat_ld_annotations() + scale_y_continuous(limits = c(NA,NA)) +
									scale_fill_manual(values=FEMALE_MALE_PALETTE) +scale_colour_manual(values=FEMALE_MALE_PALETTE) +
									geom_line(data=rbind(dt_f, dt_m),  size=2) +
									ethogram_theme

all_pl_objs$etho_stimuli_2




pdf("prolonged_sd_rebound.pdf", w=12,h=6)
for(p_name in names(all_pl_objs)){
	print(p_name)
    pl <- all_pl_objs[[which(names(all_pl_objs) == p_name)]]
    print(pl + ggtitle(p_name))
}
dev.off()


