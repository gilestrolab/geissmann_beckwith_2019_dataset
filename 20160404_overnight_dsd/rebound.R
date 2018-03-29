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
	
	# We currate furter the data by removing individuals that do not have a matching pair 
	# with a different treatment in the same experiment.
	# this means we reduce polution of the data by a large number of controls not necessarily done 
	# at the same time/ same flies..
	males_to_keep <- meta(data)[sex == "M",
						.(
						  n_conditions = length(unique(sdi)),
						  sex = "M"
						  ), 
						by = .(datetime, machine_id)]
	           
	females_to_keep <- meta(data)[sex == "F",
						.(
						  n_conditions = length(unique(sdi)),
						  sex = "F"
						  ), 
						by = .(datetime, machine_id)]
						           
	experiments_to_keep <- rbind(males_to_keep, females_to_keep)
	experiments_to_keep <- experiments_to_keep[n_conditions > 1, -"n_conditions", with=FALSE]
	id_to_keep <- meta(data)[experiments_to_keep, on=c(names(experiments_to_keep))]$id
	
	out <- data[ id %in% id_to_keep, verbose=T]
	out
}

# then we apply this function to our data
dt <- curate_data(dt)
# we have a look at our resulting data
print(dt)
summary(dt)



dates <- as.character(sort(unique(as.Date(dt[, meta=T]$datetime))))
#dt[, id2 := interaction(region_id,machine_name, date), meta=T]
pdf("overview_all_experiments.pdf", width=9, h=16)
for (d in dates){
  print(d)
  sdt <- dt[as.Date(xmv(datetime)) == d]
  pl <- ggetho(sdt, aes(y=paste(sex, id), z=asleep)) + 
        stat_tile_etho() + 
        ggtitle(d)
  print(pl)
}

dev.off()




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
        annotate("segment",y = .9, yend = .9,   x = days(.5), xend = days(1),   colour = "black",alpha=0.5,size=3),
        annotate("text",y=0.95,x=days(.75), label="treatment")
    ))
    out
}



all_pl_objs$etho_sleep <- ggetho(dt[xmv(sdi) %in% c(0,10)],
                                    aes(y = asleep, fill=treatment)) +
                                layers()
all_pl_objs$etho_sleep_zoom <- all_pl_objs$etho_sleep +
                                  coord_cartesian(xlim = c(days(0.5),days(1.5)))

all_pl_objs$etho_sleep_full <-  ggetho(dt,
                                    aes(y = asleep, fill=interval)) +
                                layers(annotate=F, palette = scales::hue_pal()(11))


all_pl_objs$etho_quiet <- ggetho(dt[xmv(sdi) %in% c(0,10)],
                                    aes(y = !moving, fill=treatment)) +
                                layers()
all_pl_objs$etho_quiet_zoom <- all_pl_objs$etho_quiet +
                                    coord_cartesian(xlim = c(days(0.5),days(1.5)))

all_pl_objs$etho_quiet_full <-  ggetho(dt,
                                    aes(y = !moving, fill=interval)) +
                                layers(annotate=F, palette = scales::hue_pal()(11))


all_pl_objs$etho_stimuli <- ggetho(dt[xmv(sdi) %in% c(0,10)],
                                    aes(y = interactions, fill=treatment),
                                    summary_FUN = sum) +
                                  layers() + scale_y_continuous(limits = c(NA,NA))

all_pl_objs$etho_stimuli_zoom <- all_pl_objs$etho_stimuli +
                                        coord_cartesian(xlim = c(days(0.5),days(1.5)))

all_pl_objs$etho_stimuli_full <-    ggetho(dt,
                                        aes(y = interactions, fill=interval),
                                        summary_FUN = sum) +
                                    layers(annotate=F, palette = scales::hue_pal()(11))+
                                    scale_y_continuous(limits = c(NA,NA))


dt[, interval := as.numeric(as.character(xmv(interval)))]

#~ all_pl_objs$etho_stimuli_rel_ovrw <- ggetho(dt[xmv(treatment) == "SD"],
#~                                     aes(z = (interactions / 10) / (1/as.numeric(interval))),
#~                                     summary_FUN = mean) +
#~                                     stat_tile_etho()

all_pl_objs$etho_stimuli_rel <- ggetho(dt[xmv(sdi) == 10],
                                    aes(y = (interactions / 10) / (1/as.numeric(interval)), colour= treatment),
                                    summary_FUN = mean) +
                                  layers(annotate=F, palette = scales::hue_pal()(11)[1:10]) + scale_y_continuous(name="N_interactions / N_maximum" ,limits = c(NA,NA))  +
                                  coord_cartesian(xlim = c(days(0.45),days(1.05)))

#~ all_pl_objs$etho_stimuli_rel_full <- ggetho(dt[xmv(treatment) == "SD"],

all_pl_objs$etho_stimuli_rel_ovrw <- ggetho(dt[xmv(treatment) == "SD"],
                                    aes(z = (interactions / 10) / (1/as.numeric(interval))),
                                    summary_FUN = mean) +
                                    stat_tile_etho()
                                    
all_pl_objs$etho_stimuli_rel <- ggetho(dt[xmv(treatment) == "SD"],
                                    aes(y = (interactions / 10) / (1/as.numeric(interval)), colour= interval),
                                    summary_FUN = mean) +
                                  layers(annotate=F, palette = scales::hue_pal()(11)[1:10]) + scale_y_continuous(name="N_interactions / N_maximum" ,limits = c(NA,NA))  +
                                  coord_cartesian(xlim = c(days(0.45),days(1.05)))


all_pl_objs$dummy_plot <- ggplot() + ggtitle("dummy plot to keep page number consistent")


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
						sleep_baseline_all = mean(asleep[t %between% hours(c(-12, 12))]),
						sleep_sd_night = mean(asleep[t %between% hours(c(12, 24))]),
						quiet_sd_night = mean(!moving[t %between% hours(c(12, 24))]),
						sleep_rebound_day3h = mean(asleep[t %between% hours(c(24, 24 +3))]),
						quiet_rebound_day3h = mean(!moving[t %between% hours(c(24, 24 +3))]),
						sleep_rebound_day6h = mean(asleep[t %between% hours(c(24, 24 +6))]),
						quiet_rebound_day6h = mean(!moving[t %between% hours(c(24, 24 +6))]),
						sleep_rebound_day = mean(asleep[t %between% hours(c(24, 24 +12))]),
						quiet_rebound_day = mean(!moving[t %between% hours(c(24, 24 +12))]),
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

#ggplot(stat_rebound_dt, aes(x=quiet_baseline_day, y=quiet_rebound_day)) + geom_point(alpha=.3) + facet_grid(sex ~ .) + scale_x_sqrt()+ scale_y_sqrt()
summary(lm(quiet_rebound_day3h ~ quiet_baseline_day3h *  sex, stat_rebound_dt[treatment == "Control"]))
summary(lm(sleep_rebound_day3h ~ sleep_baseline_day3h *  sex, stat_rebound_dt[treatment == "Control"]))

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



ggplot(stat_rebound_dt[sdi ==10], aes(x=quiet_baseline_night, y=quiet_rebound_day3h_diff * 3 * 60)) + geom_point(alpha=.3) + facet_grid(sex ~ .) 
# quiecence loss -> rebound
ggplot(stat_rebound_dt[sdi ==10], aes(x= (quiet_baseline_night - quiet_sd_night) * 12 * 60 , y=quiet_rebound_day3h_diff * 3 * 60)) + geom_point(alpha=.3) + facet_grid(sex ~ .)  + geom_smooth(method = "lm")
#?
#ggplot(stat_rebound_dt[sdi ==10], aes(x= (sleep_baseline_night - sleep_sd_night) * 12 * 60 , y=quiet_rebound_day3h_diff * 3 * 60)) + geom_point(alpha=.3) + facet_grid(sex ~ .)  + geom_smooth(method = "lm")


all_pl_objs$sleep_sd_night <- ggplot(stat_rebound_dt, aes(interval, sleep_sd_night, colour=treatment)) + layer_barpl() + scale_y_continuous(limits=c(0,1))
all_pl_objs$quiet_sd_night <- ggplot(stat_rebound_dt, aes(interval, quiet_sd_night, colour=treatment)) + layer_barpl() + scale_y_continuous(limits=c(0,1))

pdf("rebound_quantif.pdf", w=8,h=8)
all_pl_objs$bar_quiet_reb_day3h 
all_pl_objs$bar_quiet_reb_day3h_min_baseline
all_pl_objs$bar_quiet_reb_day3h_diff 
all_pl_objs$bar_quiet_has_reb
dev.off()


# who rebounds the best


ggplot(stat_rebound_dt[sdi!=0], aes(x=as.numeric(as.character(interval)), y=as.numeric(quiet_rebound_day3h_pred_sign))) +  facet_grid(sex ~ .) + geom_smooth() + geom_jitter(alpha=.3, height=.1)


#low sleepers do not rebound better or worse
ggplot(stat_rebound_dt[sdi==10], aes(x=quiet_baseline_night, y=as.numeric(quiet_rebound_day3h_pred_sign))) +  facet_grid(sex ~ .) + geom_smooth() + geom_jitter(alpha=.3, height=.1)
ggplot(stat_rebound_dt[sdi!=0], aes(x=quiet_baseline_night, y=as.numeric(quiet_rebound_day3h_pred_sign))) +  facet_grid(interval ~ sex) + geom_smooth() + geom_jitter(alpha=.3, height=.1)

# ibid w day sleep
ggplot(stat_rebound_dt[sdi==10], aes(x=quiet_baseline_day, y=as.numeric(quiet_rebound_day3h_pred_sign))) +  facet_grid(sex ~ .) + geom_smooth() + geom_jitter(alpha=.3, height=.1)
ggplot(stat_rebound_dt[sdi!=0], aes(x=quiet_baseline_day, y=as.numeric(quiet_rebound_day3h_pred_sign))) +  facet_grid(interval ~ sex) + geom_smooth() + geom_jitter(alpha=.3, height=.1)

#more interaction != more rebound @ 20s
ggplot(stat_rebound_dt[sdi==10], aes(x=interactions, y=as.numeric(quiet_rebound_day3h_pred_sign))) +  facet_grid(sex ~ .) + geom_smooth() + geom_jitter(alpha=.3, height=.1) + scale_x_sqrt()
#but true for longer interval
ggplot(stat_rebound_dt[sdi!=0], aes(x=interactions, y=as.numeric(quiet_rebound_day3h_pred_sign))) +  facet_grid(interval ~ sex) + geom_smooth() + geom_jitter(alpha=.3, height=.1) + scale_x_sqrt()



pdf("overnight_dsd_rebound.pdf", w=12,h=6)
for(p_name in names(all_pl_objs)){
    pl <- all_pl_objs[[which(names(all_pl_objs) == p_name)]]
    print(pl + ggtitle(p_name))
    p <- plotly::ggplotly(pl)
    htmlwidgets::saveWidget(plotly::as_widget(p),
                sprintf("./overnight_dsd_rebound-%s.html", p_name))
}
dev.off()


#############################################
pdf("rebound_quantif_expl.pdf", h=4,w=4)
exple_dt <- data.table(group= c(c("A","B","C"), c("A","B","C")),
				t = c(0,0,0,1,1,1),
				N_eggs=c(100,200,100, 300,400,200),
				eggs_survival=c(.1,.4,.9, .3,.6,.9999))
				
ggplot(exple_dt, aes(t, N_eggs, colour=group, shape=group), size=2) + geom_line() + geom_point()
ggplot(exple_dt, aes(t, eggs_survival, colour=group, shape=group), size=2) + geom_line() + geom_point() + scale_y_continuous(labels = scales::percent)
dev.off()
pdf("rebond_quantif.pdf", h=8,w=8)
pl <- ggetho(dt[xmv(sdi) %in% c(0,10) & xmv(sex) =="M"],
                                    aes(y = asleep, fill=treatment)) +
                                stat_pop_etho() +
								stat_ld_annotations()+
								scale_y_continuous(limits = c(NA,1))+
								scale_fill_manual(values=CONTROL_SD_PALETTE)+
								scale_colour_manual(values=CONTROL_SD_PALETTE)+ ethogram_theme+
                                coord_cartesian(xlim = c(days(0),days(1.5)))
pl


dtm <- dt[xmv(sex)=="M" & xmv(sdi) == 0]
setmeta(dtm, stat_rebound_dt[sex=="M" & sdi == 0, .(id,sleep_baseline_all)])
ggetho(dtm,  aes(z = asleep, y= interaction(id, sleep_baseline_all * 1000)), time_wrap=hours(24)) +
                                    stat_tile_etho() + theme(
				axis.text.y=element_blank(),
				axis.ticks.y=element_blank()) + scale_y_discrete(name="Individual") + stat_ld_annotations()


d <- data.frame(xmin = days(1), xmax=days(1) + hours(3), ymin=-Inf, ymax=Inf)
pl + geom_rect(data=d, mapping=aes(xmin = xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=.4, colour=NA ,inherit.aes=F )

ggplot(stat_rebound_dt[sdi %in% c(0,10) & sex=="M"], aes(treatment, sleep_rebound_day3h, colour=treatment)) + 
		layer_barpl() + scale_y_continuous(limits = c(0,1), name = "Sleep during rebound day (ZT0-3)")

control_data <- stat_rebound_dt[sdi %in% c(0) & sex=="M"]
sd_data <- stat_rebound_dt[sdi %in% c(10) & sex=="M"]

ggplot(control_data, aes(sleep_baseline_day3h, sleep_rebound_day3h, colour=treatment, shape = treatment)) + 
				geom_point(alpha=.75,size=2) +
				scale_y_continuous(limits = c(0,1), name = "Sleep during rebound day (ZT0-3)") +
				scale_x_continuous(limits = c(0,1), name = "Sleep during baseline day (ZT0-3)") +
				geom_smooth(method="lm")
				

pl <- ggplot(stat_rebound_dt[sdi %in% c(0, 10) & sex=="M"], aes(sleep_baseline_day3h, sleep_rebound_day3h, colour=treatment, shape = treatment)) + 
				geom_point(alpha=.75,size=2) +
				scale_y_continuous(limits = c(0,1), name = "Sleep during rebound day (ZT0-3)") +
				scale_x_continuous(limits = c(0,1), name = "Sleep during baseline day (ZT0-3)") +
				geom_smooth(data = control_data, method="lm")
pl

pl_hist <- ggplot(stat_rebound_dt[sdi %in% c(0, 10) & sex=="M"], aes(sleep_rebound_day3h, fill = treatment)) + 
				geom_density(alpha=.5) +
				scale_x_continuous( limits = c(0,1)) + coord_flip() +
				theme(axis.title.y=element_blank(),
				axis.text.y=element_blank(),
				axis.ticks.y=element_blank())


gridExtra::grid.arrange(pl + theme(legend.position="none"), pl_hist, ncol=2, nrow=1, widths=c(3, 1))


mod <- lm(sleep_rebound_day3h ~ sleep_baseline_day3h * sex, stat_rebound_dt[sdi == 0])

stat_rebound_dt[, sleep_rebound_day3h_pred := predict(mod, stat_rebound_dt)]
stat_rebound_dt[, sleep_rebound_day3h_diff := sleep_rebound_day3h - sleep_rebound_day3h_pred]
stat_rebound_dt[, sleep_rebound_day3h_pred_sign := sleep_rebound_day3h_diff > 0]


pl <- ggplot(sd_data, aes(sleep_baseline_day3h, sleep_rebound_day3h, colour=treatment, shape = treatment)) + 
				geom_point(alpha=.75,size=2) +
				scale_y_continuous(limits = c(0,1), name = "Sleep during rebound day (ZT0-3)") +
				scale_x_continuous(limits = c(0,1), name = "Sleep during baseline day (ZT0-3)") +
				geom_smooth(data = control_data, method="lm")
pl + geom_segment(aes(xend = sleep_baseline_day3h, yend= sleep_rebound_day3h_pred), linetype=2)



ggplot(stat_rebound_dt[sdi %in% c(0, 10) & sex=="M"], aes(sleep_rebound_day3h_diff * 180, fill = treatment)) + 
				geom_density(alpha=.5) + geom_vline(xintercept=0, linetype=2)+ 
				scale_x_continuous(name="Extra sleep in 3h (min)")

wilcox.test(sleep_rebound_day3h_diff ~ treatment, stat_rebound_dt[sdi %in% c(0, 10) & sex=="M"])


pl <- ggplot(stat_rebound_dt[sex=="M"], aes(interval, sleep_rebound_day3h, colour=treatment)) + layer_barpl() +
		scale_y_continuous(limits = c(0,1), name = "Sleep during rebound day (ZT0-3)") 
pl

summ_stat <- stat_rebound_dt[,{
				w = wilcox.test(sleep_rebound_day3h_diff, mu=0, alternative="greater")
				list(pval=w$p.value, n=.N)},
				keyby="sex,interval,treatment"]

summ_stat[, text:=sprintf("%s\nN=%03d", stars.pval(pval), n)]

ggplot(stat_rebound_dt[sex=="M"], aes(interval, sleep_rebound_day3h_diff * 180, colour=treatment)) + layer_barpl() +
	geom_label(data= summ_stat[sex=="M"], aes(x=interval, label=text),  y=-0.25 * 180, colour="black", alpha=.5) +
	geom_hline(yintercept=0, colour="red", linetype=2) + 
	scale_y_continuous(name="Extra sleep in 3h (min)")


#~ summary(lm(sleep_rebound_day3h ~ interval * sex , stat_rebound_dt))
#~ summary(lm(sleep_rebound_day3h_diff ~ interval  * sex, stat_rebound_dt))



summ_stat <- stat_rebound_dt[,{
				w = wilcox.test(sleep_rebound_day3h_diff, mu=0, alternative="greater")
				list(pval=w$p.value, n=.N)},
				keyby="sex,interval,treatment"]
summ_stat[, text:=sprintf("%s\nN=%03d", stars.pval(pval), n)]

ggplot(stat_rebound_dt[sex=="M"], aes(interval, as.numeric(sleep_rebound_day3h_pred_sign > 0), colour=treatment)) + layer_barpl() + 
						scale_y_continuous(labels = scales::percent, name="Animals with positive rebound")	 +
						geom_hline(yintercept=0.5, colour="red", linetype=2) + 
						geom_label(data= summ_stat[sex=="M"], aes(x=interval, label=text),  y=0.25 , colour="black", alpha=.5)
						


ggplot(stat_rebound_dt[sex=="M" & sdi==10], aes(sleep_baseline_day, as.numeric(sleep_rebound_day3h_pred_sign > 0)))  + geom_point(alpha=.3) + geom_smooth()+
			scale_y_continuous(labels = scales::percent, name="Animals with positive rebound")	+
			scale_x_continuous(name="Sleep during baseline day")	 
ggplot(stat_rebound_dt[sex=="M" & sdi != 0], aes(interactions, as.numeric(sleep_rebound_day3h_pred_sign > 0)))  + geom_point(alpha=.3) + 
			geom_smooth(method="lm") + facet_grid(interval ~ .) + scale_x_sqrt() +
			scale_y_continuous(labels = scales::percent, name="Animals with positive rebound")	
			
dev.off()





bdt <- bout_analysis(moving, dt)[moving == F]
bdt 


pdf("five_min_rule_exple.pdf", h=4,w=4)

t = 1:1000
response1 = (1 / (1+ exp(t/50 - 4)));
response2 = (1 / (1+ exp(t/50 - 8))); 
response3 = (1 / (1+ exp(t/25 - 2))); 
dd <- data.table(t = c(t,t), response = c(response1, response2, response3), context = rep(c("A", "B", "C"), each = length(t)))
pl <- ggplot(dd[context=="A"], aes(t, response, colour=context)) + geom_line() + scale_x_continuous(name="Bout length (s)")
pl
pl + geom_vline( xintercept =300,col="red", size=2, alpha=.8)

pl <- ggplot(dd, aes(t, response, colour=context)) + geom_line() + geom_vline( xintercept =300,col="red", size=2, alpha=.8)
pl + geom_vline( xintercept =300,col="red", size=2, alpha=.8) + scale_x_continuous(name="Bout length (s)")


dd <- data.table(length = c(rexp(10000, 1/300),rexp(10000, 1/150)), context = rep(c("A", "B"), each = 10000))

pl <- ggplot(dd, aes(length, fill=context)) + geom_density( kernel = "epanechnikov", alpha=.5) + scale_x_continuous(limits = c(0, 1500), name="Bout length (s)")
pl + geom_vline( xintercept =300,col="red", size=2, alpha=.8)
dev.off()


pdf("five_min_rule.pdf", h=8,w=8)


ggplot(stat_rebound_dt[sex=="F"], aes(sleep_baseline_day3h)) + geom_histogram(bins=50) + scale_y_sqrt()  + scale_x_continuous(limits = c(-0.1,1), name = "Sleep during baseline day (ZT0-3)")
ggplot(stat_rebound_dt[sex=="F"], aes(quiet_baseline_day3h)) + geom_histogram(bins=50) + scale_y_sqrt()  + scale_x_continuous(limits = c(-0.1,1), name = "Quiescence during baseline day (ZT0-3)")


ggplot(stat_rebound_dt[sex=="F"], aes(quiet_baseline_day3h, sleep_baseline_day3h)) + geom_point(alpha=.3) +
				scale_x_continuous(limits = c(-0.1,1), name = "Quiescence during baseline day (ZT0-3)") +
				scale_y_continuous(limits = c(-0.1,1), name = "Sleep during baseline day (ZT0-3)") +
				geom_rug(alpha=.15)


ggplot(stat_rebound_dt[sex=="F"], aes(quiet_baseline_day, sleep_baseline_day)) + geom_point(alpha=.3) +
				scale_x_continuous(limits = c(-0.1,1), name = "Quiescence during baseline day (ZT0-12)") +
				scale_y_continuous(limits = c(-0.1,1), name = "Sleep during baseline day (ZT0-12)") +
				geom_rug(alpha=.15)




stat_bdt <- bdt[, 
				.(bout_q_med_day = median(duration[t %between% hours(c(0, 12))]),
				  bout_q_max_day = max(duration[t %between% hours(c(0, 12))]),
				  bout_q_max_night_min1 = max(duration[t %between% hours(c(-12, -0))]),
				  bout_q_max_day_min1 = max(duration[t %between% hours(c(-24, -12))]))
				  
			,by=id]
			
stat_all <- stat_rebound_dt[stat_bdt]

ggplot(stat_all[sex=="F"], aes(bout_q_max_day)) + geom_histogram(bins=50) + 
			geom_vline( xintercept =300,col="red", size=2, alpha=.8)+
			scale_x_continuous(name = "Longest quiescence bout during baseline day (ZT0-12)")
			
ggplot(stat_all[sex=="F"], aes(bout_q_max_day, bout_q_max_day_min1)) + geom_point(alpha=.3) +
				geom_smooth() +
				geom_vline( xintercept =300,col="red", size=2, alpha=.8)+
				geom_hline( yintercept =300,col="red", size=2, alpha=.8)

ggplot(stat_all, aes(x= bout_q_max_night_min1 , y=as.numeric(quiet_rebound_day3h_pred_sign))) +  facet_grid(interval ~ sex) + geom_smooth() + geom_point(alpha=.3) +
			scale_y_continuous(limits = c(-0.1,1.2), labels = scales::percent, name="Animals with positive rebound")	+ scale_x_sqrt(limit=c(0,5e3), name="Longest bout in previous night") +
			geom_vline( xintercept =300,col="red", size=2, alpha=.8)
			
ggplot(stat_rebound_dt[sdi==10], aes(x= sleep_baseline_night , y=as.numeric(quiet_rebound_day3h_pred_sign))) +  facet_grid(sex ~ .) + geom_smooth() + geom_point(alpha=.3) +
			scale_y_continuous(limits = c(-0.1,1.2), labels = scales::percent, name="Animals with positive rebound")	
			
#ggplot(stat_rebound_dt[sdi==10], aes(x=quiet_sd_night, y=as.numeric(quiet_rebound_day3h_pred_sign))) +  facet_grid(sex ~ .) + geom_smooth() + geom_jitter(alpha=.3, height=.1)
#ggetho(dt[xmv(sdi) %in% c(0,10)], aes(y=!moving, fill=treatment)) + stat_pop_etho()+ facet_grid(sex ~ .) 

ggetho(bdt[xmv(sdi) %in% c(0,10)], aes(y=duration, fill=treatment),summary_FUN=max) + stat_pop_etho()+ facet_grid(sex ~ .)  +
				scale_y_continuous(name = "Longest quiescence bout")

dev.off()

