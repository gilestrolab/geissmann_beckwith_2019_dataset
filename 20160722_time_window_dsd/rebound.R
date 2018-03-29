rm(list=ls());gc()
options(nwarnings = 1000)
library(scopr)
library(ggetho)
library(sleepr)
library(gtools)

source("../ggplot_themes.R")

FEMALE_MALE_PALETTE <- c("#be2828ff", "#282896ff")
CONTROL_SD_PALETTE <- c( "#969696ff", "#3caa3cff")
METADATA <- "metadata.csv"
CACHE <- "./cache/"
#~ RESULT_DIR <- "./raw_results/"
RESULT_DIR <- "/data/ethoscope_results"

REMOTE_DIR <- "ftp://nas.lab.gilest.ro/auto_generated_data/ethoscope_results/"

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
  data <- data[is_interpolated == F]
	# first we remove animals that do not have enought data points
	valid_animals <- data[,.(t_range = max(t) - min(t)), by=id][t_range >= days(5)]$id
	data <- data[t > days(-3) & 
				 t < days(+2) &
				 id %in% valid_animals]
	
	data[, treatment := as.factor(ifelse(sdi == 0, "Control", "SD")), meta=T]
	data[, interval := round(((11-sdi) ^ 1.7)) * 20, meta=T]	
    data[, interval := plyr::revalue(as.factor(interval),c("1180"="Control")), meta=T]
	data
	}

# then we apply this function to our data
dt <- curate_data(dt)
# we have a look at our resulting data
print(dt)
summary(dt)

all_pl_objs <- list()





####### population ethogrames here

# a set of layers or our next big plots
layers <- function(palette = CONTROL_SD_PALETTE, annotate=TRUE){
  out <- list(
	stat_pop_etho(method= mean_cl_boot),
    facet_grid( sex ~ .),
    stat_ld_annotations(),
    coord_cartesian(xlim = c(days(-1),days(2))),
    scale_y_continuous(limits = c(NA,1)),
    scale_fill_manual(values=palette),
    scale_colour_manual(values=palette), ethogram_theme
    )
  if(annotate)
    out <- c(out, list(
        annotate("segment",y = .9, yend = .9,   x = hours(20), xend = days(1),   colour = "black",alpha=0.5,size=3),
        annotate("text",y=0.95,x=hours(22), label="treatment")
    ))
    out
}



all_pl_objs$etho_sleep <- ggetho(dt[xmv(sdi) %in% c(0,10)],
                                    aes(y = asleep, fill=treatment)) +
                                layers()
all_pl_objs$etho_sleep_zoom <- all_pl_objs$etho_sleep +
                                  coord_cartesian(xlim = c(days(0.5),days(1.5)))

all_pl_objs$etho_quiet <- ggetho(dt[xmv(sdi) %in% c(0,10)],
                                    aes(y = !moving, fill=treatment)) +
                                layers()
all_pl_objs$etho_quiet_zoom <- all_pl_objs$etho_quiet +
                                    coord_cartesian(xlim = c(days(0.5),days(1.5)))

all_pl_objs$etho_stimuli <- ggetho(dt[xmv(sdi) %in% c(0,10)],
                                    aes(y = interactions, fill=treatment),
                                    summary_FUN = sum) +
                                  layers() + scale_y_continuous(limits = c(NA,NA))

all_pl_objs$etho_stimuli_zoom <- all_pl_objs$etho_stimuli +
                                        coord_cartesian(xlim = c(days(0.5),days(1.5)))

dt[, interval := as.numeric(as.character(xmv(interval)))]

all_pl_objs$etho_stimuli_rel_ovrw <- ggetho(dt[xmv(treatment) == "SD"],
                                    aes(z = (interactions / 10) / (1/as.numeric(interval))),
                                    summary_FUN = mean) +
                                    stat_tile_etho()
all_pl_objs$etho_stimuli_rel <- ggetho(dt[xmv(treatment) == "SD"],
                                    aes(y = (interactions / 10) / (1/as.numeric(interval)), colour= interval),
                                    summary_FUN = mean) +
                                  layers(annotate=F, palette = scales::hue_pal()(11)[1:10]) + scale_y_continuous(name="N_interactions / N_maximum" ,limits = c(NA,NA))  +
                                  coord_cartesian(xlim = c(days(0.45),days(1.05)))
                                  
#########=========================================================#################

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
	

stat_rebound_dt <- rejoin(
						dt[,
						.(
						sleep_baseline_day = mean(asleep[t %between% hours(c(0, 12))]),
						quiet_baseline_day = mean(!moving[t %between% hours(c(0, 12))]),
						sleep_baseline_day3h = mean(asleep[t %between% hours(c(0, 3))]),
						quiet_baseline_day3h = mean(!moving[t %between% hours(c(0, 3))]),
						sleep_baseline_day6h = mean(asleep[t %between% hours(c(0, 6))]),
						sleep_baseline_night = mean(asleep[t %between% hours(c(-12, 0))]),
						quiet_baseline_night = mean(!moving[t %between% hours(c(-12, 0))]),
						sleep_baseline_night4h = mean(asleep[t %between% hours(c(-4, 0))]),
						quiet_baseline_night4h = mean(!moving[t %between% hours(c(-4, 0))]),
						sleep_baseline_all = mean(asleep[t %between% hours(c(-12, 12))]),
						sleep_sd_night4h = mean(asleep[t %between% hours(c(20, 24))]),
						quiet_sd_night4h = mean(!moving[t %between% hours(c(20, 24))]),
						sleep_rebound_day3h = mean(asleep[t %between% hours(c(24, 24 +3))]),
						quiet_rebound_day3h = mean(!moving[t %between% hours(c(24, 24 +3))]),
						sleep_rebound_day6h = mean(asleep[t %between% hours(c(24, 24 +6))]),
						quiet_rebound_day6h = mean(!moving[t %between% hours(c(24, 24 +6))]),
						interactions = sum(interactions)
						)
						,by = id]
						)



	
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
all_pl_objs$bar_quiet_reb_day3h_diff <- all_pl_objs$bar_quiet_reb_day3h_diff + geom_label(data= summ_stat, aes(x=interval, label=text),  y=-0.25 * 180, colour="black", alpha=.5)

all_pl_objs$bar_quiet_has_reb <- ggplot(stat_rebound_dt, aes(interval, as.numeric(quiet_rebound_day3h_pred_sign > 0), colour=treatment)) + layer_barpl() + 
						scale_y_continuous(labels = scales::percent, name="Animals with positive rebound")	 +
						geom_hline(yintercept=0.5, colour="red", linetype=2)

summ_stat <- stat_rebound_dt[,{
				w = binom.test(sum(quiet_rebound_day3h_pred_sign), length(quiet_rebound_day3h_pred_sign), alternative="greater")
				list(pval=w$p.value, n=.N)},
				keyby="sex,interval,treatment"]
				
summ_stat[, text:=sprintf("%s\nN=%03d", stars.pval(pval), n)]

all_pl_objs$bar_quiet_has_reb <- all_pl_objs$bar_quiet_has_reb  + geom_label(data= summ_stat, aes(x=interval, label=text),  y=0.25, colour="black", alpha=.5)



ggplot(stat_rebound_dt[sdi %in% c(0, 10)], aes(x=quiet_baseline_day3h, y=quiet_rebound_day3h, colour=treatment)) + geom_point(alpha=1) + facet_grid(sex ~ .) + 
			#geom_smooth(data=stat_rebound_dt[treatment=="Control"])
			geom_smooth(method = "lm")



ggplot(stat_rebound_dt[sdi %in% c(0)], aes(x=sleep_baseline_night4h, y=sleep_sd_night4h, colour=treatment)) + geom_point(alpha=1) + facet_grid(sex ~ .) + 
			#geom_smooth(data=stat_rebound_dt[treatment=="Control"])
			geom_smooth(method = "lm")
			
ggplot(stat_rebound_dt[sdi %in% c(0, 10)], aes(x=sleep_baseline_night4h * 4 * 60, y=quiet_rebound_day3h_diff * 3 * 60, colour=treatment)) + geom_point(alpha=1) + facet_grid(sex ~ .) + 
			#geom_smooth(data=stat_rebound_dt[treatment=="Control"])
			geom_smooth(method = "lm")
			
			
ggplot(stat_rebound_dt[sdi %in% c(0)], aes(x=sleep_baseline_day6h, y=sleep_rebound_day6h, colour=treatment)) + geom_point(alpha=1) + facet_grid(sex ~ .) + 
			#geom_smooth(data=stat_rebound_dt[treatment=="Control"])
			geom_smooth(method = "lm")


ggplot(stat_rebound_dt[sdi %in% c(0)], aes(x=quiet_baseline_day3h, y=quiet_rebound_day3h, colour=treatment)) + geom_point(alpha=1) + facet_grid(sex ~ .) + 
			#geom_smooth(data=stat_rebound_dt[treatment=="Control"])
			geom_smooth(method = "lm")



pdf("time_window_dsd_rebound.pdf", w=12,h=6)
for(p_name in names(all_pl_objs)){
    pl <- all_pl_objs[[which(names(all_pl_objs) == p_name)]]
    print(pl + ggtitle(p_name))
    p <- plotly::ggplotly(pl)
    htmlwidgets::saveWidget(plotly::as_widget(p),
                sprintf("./overnight_dsd_rebound-%s.html", p_name))
}
dev.off()
