rm(list=ls());gc()
options(nwarnings = 1000)
library(scopr)
library(ggetho)
library(sleepr)

FEMALE_MALE_PALETTE <- c("#be2828ff", "#282896ff")
CONTROL_SD_PALETTE <- c( "#969696ff", "#3caa3cff")
METADATA <- "metadata.csv"
CACHE <- "./cache/"
RESULT_DIR <- "./raw_results/"
REMOTE_DIR <- "ftp://nas.lab.gilest.ro/auto_generated_data/ethoscope_results/"

met <- fread(METADATA)
met <- met[status == "OK"]
met <- link_ethoscope_metadata_remote(met,
                                      remote_dir =  REMOTE_DIR,
                                      result_dir = RESULT_DIR,
                                      verbose = TRUE)
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
    scale_colour_manual(values=palette)
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
all_pl_objs$etho_quiet_zoom <- all_pl_objs$quiet_sleep +
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


################### scalar stats here and barplots (1 value/animal)

# shared layers
layer_barpl <- function(){
	list(
		facet_grid(sex ~ .),
		stat_summary(fun.y = "mean", geom="bar"),
		stat_summary(fun.data = "mean_cl_boot", geom="errorbar"),
		geom_jitter(alpha=.3),
		scale_fill_manual(values=CONTROL_SD_PALETTE),
		scale_colour_manual(values=CONTROL_SD_PALETTE)
		)
	}
	
stat_rebound_dt <- rejoin(
						dt[,
						.(
						sleep_baseline_day = mean(asleep[t %between% hours(c(0, 12))]),
						sleep_baseline_night = mean(asleep[t %between% hours(c(-12, 0))]),
						sleep_baseline_all = mean(asleep[t %between% hours(c(-12, 12))]),
						sleep_rebound_day3h = mean(asleep[t %between% hours(c(24, 24 +3))]),
						quiet_rebound_day3h = mean(!moving[t %between% hours(c(24, 24 +3))]),
						sleep_rebound_day6h = mean(asleep[t %between% hours(c(24, 24 +6))]),
						quiet_rebound_day6h = mean(!moving[t %between% hours(c(24, 24 +6))]),
						interactions = sum(interactions)
						)
						,by = id]
						)

	
all_pl_objs$bar_sleep_reb_day3h <- ggplot(stat_rebound_dt, aes(interval, sleep_rebound_day3h, fill=treatment)) + layer_barpl()
all_pl_objs$bar_sleep_reb_day6h <- ggplot(stat_rebound_dt, aes(interval, sleep_rebound_day6h, fill=treatment)) + layer_barpl()
all_pl_objs$bar_quiet_reb_day3h <- ggplot(stat_rebound_dt, aes(interval, quiet_rebound_day3h, fill=treatment)) + layer_barpl()
all_pl_objs$bar_quiet_reb_day6h <- ggplot(stat_rebound_dt, aes(interval, quiet_rebound_day6h, fill=treatment)) + layer_barpl()
all_pl_objs$bar_interactions <- ggplot(stat_rebound_dt, aes(interval, interactions, fill=treatment)) + layer_barpl() + scale_y_sqrt()

pdf("movement_control_rebound.pdf", w=12,h=6)
for(p_name in names(all_pl_objs)){
    pl <- all_pl_objs[[which(names(all_pl_objs) == p_name)]]
    print(pl + ggtitle(p_name))
    p <- plotly::ggplotly(pl)
    htmlwidgets::saveWidget(plotly::as_widget(p),
                sprintf("./movement_control_rebound-%s.html", p_name))
}
dev.off()



#save(all_pl_objs, file="rebound_plots.RData", compress="gzip")

make_rebound_stats <- function(data){	
	stat_rebound_dt <- data[,
					   .(
						 sleep_baseline_night = mean(asleep[between(t,days(-0.5), days(-0.0))]),
						 sleep_baseline_all = mean(asleep[between(t,days(-0.5), days(0.5))]),
						 sleep_baseline_day = mean(asleep[between(t,days(0.0), days(0.5))]),
						 sleep_baseline_day3h_min1 = mean(asleep[between(t,days(-1.0), days(-1)+hours(3))]),
						 sleep_baseline_day3h = mean(asleep[between(t,days(0.0), days(0)+hours(3))]),
#~ 						 quiet_baseline_day3h = mean(quiet[between(t,days(0.0), days(0)+hours(3))]),
						 sleep_baseline_day6h = mean(asleep[between(t,days(0.0), days(0)+hours(6))]),
#~ 						 quiet_baseline_day6h = mean(quiet[between(t,days(0.0), days(0)+hours(6))]),
#~ 						 quiet_baseline_day1h = mean(quiet[between(t,days(0.0), days(0)+hours(1))]),
#~ 						 quiet_baseline_day2h = mean(quiet[between(t,days(0.0), days(0)+hours(2))]),
						 sleep_sd_night = mean(asleep[between(t,days(.55), days(.95))]),
						 sleep_rebound_night = mean(asleep[between(t,days(1.5), days(2.0))]),
						 sleep_rebound_all = mean(asleep[between(t,days(1.0), days(2))]),
						 sleep_rebound_all_day2 = mean(asleep[between(t,days(2), days(3))]),
						 sleep_rebound_day = mean(asleep[between(t,days(1.0), days(1.5))]),
						 sleep_rebound_day3h = mean(asleep[between(t,days(1.0), days(1)+hours(3))]),
#~ 						 quiet_rebound_day3h = mean(quiet[between(t,days(1.0), days(1)+hours(3))]),
						 sleep_rebound_day6h = mean(asleep[between(t,days(1.0), days(1)+hours(6))]),
						 sleep_rebound_night3h = mean(asleep[between(t,days(1.0), days(1.5)+hours(3))]),
#~ 						 quiet_rebound_day6h = mean(quiet[between(t,days(1.0), days(1)+hours(6))]),
#~ 						 quiet_rebound_day1h = mean(quiet[between(t,days(1.0), days(1)+hours(1))]),
#~ 						 quiet_rebound_day2h = mean(quiet[between(t,days(1.0), days(1)+hours(2))]),
						 sleep_latency = min(t[t> days(1) & asleep == T]) - days(1),
						 sleep_latency_baseline = min(t[t> days(0) & asleep == T]),
						 interaction_n = sum(interactions)
									 ),
					   by=key(data)
					   ]
	stat_rebound_dt <- rejoin(stat_rebound_dt)
					   
#~ stat_rebound_dt[ , treatment := as.factor(ifelse(interval<=mins(5),"SD", "Control"))]
#	stat_rebound_dt[,interval := plyr::revalue(as.factor(interval),c("1180"="Control"))]
	stat_rebound_dt
	}

