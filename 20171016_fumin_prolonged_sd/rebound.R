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
RESULT_DIR <- "/data/ethoscope_results/"
REMOTE_DIR <- "ftp://nas.lab.gilest.ro/auto_generated_data/ethoscope_results/"

met <- fread(METADATA)
met <- met[status == "OK"]
met <- link_ethoscope_metadata(met,
                                      result_dir = RESULT_DIR)
                                      
dt <- load_ethoscope(met,
					   max_time=days(20),
					   reference_hour=9.0,
					   cache = CACHE,
					   FUN = sleep_annotation,
					   ncores=1)

summary(dt)
                                      

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

pl <- ggetho(dt[!is.infinite(xmv(death_date))], aes(y=paste(as.Date(datetime),  id), z=asleep)) + 
			stat_tile_etho() 



dates <- as.character(sort(unique(as.Date(dt[, meta=T]$datetime))))
#dt[, id2 := interaction(region_id,machine_name, date), meta=T]
pdf("overview_all_experiments.pdf", width=9, h=16)
for (d in dates){
  print(d)
  sdt <- dt[as.Date(xmv(datetime)) == d]
  pl <- ggetho(sdt, aes(y=paste(sex, id), z=moving)) + 
        stat_tile_etho() + 
        ggtitle(d) +
        stat_ld_annotations()
        
  print(pl)
  pl <- ggetho(sdt, aes(y=paste(sex, id), z=interactions)) + 
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
all_pl_objs$etho_stimuli_rel


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




pdf("prolonged_sd_fumin_rebound.pdf", w=12,h=6)
for(p_name in names(all_pl_objs)){
    pl <- all_pl_objs[[which(names(all_pl_objs) == p_name)]]
    print(pl + ggtitle(p_name))
    p <- plotly::ggplotly(pl)
    htmlwidgets::saveWidget(plotly::as_widget(p),
                sprintf("./prolonged_sd-%s.html", p_name))
}
dev.off()




