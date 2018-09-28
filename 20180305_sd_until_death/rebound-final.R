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


boot_km_dt <- surv_data[,{
					s <-Surv(days_alive, dead)
					o <- Hmisc::bootkm(s, B=5000)
					out <- quantile(o, p=c(0.025,.5, 0.975))
					#print(median(s))
					as.list(out)
					}, by= "sex,treatment"]
					

#~       sex treatment 2.5%  50% 97.5%
#~ 1:   male        SD 41.5, [38.0, 44.0]
#~ 2:   male      mock 46.0, [41.0, 48.5]
#~ 3: female        SD 37.5, [33.0, 38.5]
#~ 4: female      mock 41.0, [38.5, 44.0]

#41.5, [38.0, 44.0]
#46.0, [41.0, 48.5]
#37.5, [33.0, 38.5]
#41.0, [38.5, 44.0]

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




layers <- function(palette = CONTROL_SD_PALETTE, annotate=TRUE){
  out <- list(
    stat_pop_etho(method= mean_cl_boot),
    facet_grid( sex ~ .),
    stat_ld_annotations(),
    scale_y_continuous(limits = c(NA,1)),
    scale_fill_manual(values=palette),
    scale_colour_manual(values=palette), 
    ethogram_theme
    )
}


all_pl_objs <- list()

dt_stchd_curated2 <- dt_stchd_curated[, .SD[t < (max(t) - days(1))], by=id] 

all_pl_objs$etho_sleep_1day <- ggetho(dt_stchd_curated2,
                                    aes(y = asleep, fill=treatment),
                                    summary_time_window=hours(24)) +
									layers()
all_pl_objs$etho_sleep <- ggetho(dt_stchd_curated2,
                                    aes(y = asleep, fill=treatment)) +
									layers()



pdf("life_long_sd.pdf", w=12,h=6)
for(p_name in names(all_pl_objs)){
	print(p_name)
    pl <- all_pl_objs[[which(names(all_pl_objs) == p_name)]]
    print(pl + ggtitle(p_name))
}
dev.off()


library(boot)


foo <- function(data, i){
	na.omit(data[i])[, (mean(overall_sleep_10d[treatment == "mock"]) - mean(overall_sleep_10d[treatment == "SD"])) / mean(overall_sleep_10d[treatment == "mock"])]
	}

foo(stat_rebound_dt[sex=="male"], i=1:nrow(stat_rebound_dt))
b <- boot::boot(data=stat_rebound_dt[sex=="male"], statistic = foo, R =10000)
boot::boot.ci(b, type="basic")
#0.9684837
#0.9452,  1.002

foo(stat_rebound_dt[sex=="female"], i=1:nrow(stat_rebound_dt))
b <- boot::boot(data=stat_rebound_dt[sex=="female"], statistic = foo, R =10000)
boot::boot.ci(b, type="basic")

#0.929,[0.893, 0.975],

foo(stat_rebound_dt[], i=1:nrow(stat_rebound_dt))
b <- boot::boot(data=stat_rebound_dt[], statistic = foo, R =10000)
boot::boot.ci(b, type="basic")


pred_lifespan_dt <- rejoin(tdt[, 
                        .(mean_sleep = mean(asleep),
                          mean_sleep_20d = mean(asleep[t < days(20)]),
                          mean_sleep_10d = mean(asleep[t < days(10)])),
                          by=id])
pred_lifespan_dt[, days_alive:= days_alive+2]
pred_lifespan_dt <- pred_lifespan_dt[treatment=="mock"& right_censored==F]





ggplot(pred_lifespan_dt[days_alive > 20 ], aes(mean_sleep_10d, days_alive, colour=sex)) + 
  geom_point() + 
  geom_smooth(method="lm")

boom <- pred_lifespan_dt[days_alive > 20 ]

foo <- function(d, i){
  lm( days_alive ~ mean_sleep_10d * sex, d[i])$coefficients
  
}


bci <- boot::boot(data=boom, statistic=foo, R=1000)
boot.ci(bci, type="basic",index=2)

bci <- boot::boot(data=boom[sex=="female"], statistic=foo, R=1000)
boot.ci(bci, type="basic",index=2)


Given the variability of sleep within our wild-type population (fig XXX?), we also wondered whether the sleep amount of untreated animals could predict their lifespan.
We performed a linear regression (see methods), and found no overall effect: +0.41 days of life per hours of sleep a day  (CI= [-0.51,  1.55]).
There were also no interaction between time spent sleeping and sex.
This result indicates that short-sleeper do not die faster.


