rm(list=ls());gc()
options(nwarnings = 1000)
library(scopr)
library(ggetho)
library(sleepr)
library(gtools)
source("../ggplot_themes.R")


FEMALE_MALE_PALETTE <- c("#be2828ff", "#282896ff")
CONTROL_SD_PALETTE <- c( "#969696ff", "#3caa3cff")
METADATA <- "metadata_complete.csv"
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
					   max_time=days(20),
					   reference_hour=9.0,
					   cache = CACHE,
					   FUN = sleep_annotation,
					   ncores=1)

summary(dt)
                                      
dt <- stitch_on(dt, on="uid")   

curate_data <- function(data){
  
	data <- data[is_interpolated == F]
	data <- data[id %in% dt[,.(max(t), max(t)/.N),by=id][V1 > days(13) & V2 < 11][,id]]
	data <- data[t %between% c(days(1), days(13))]
	data
}

# then we apply this function to our data
dt <- curate_data(dt)

pl <- ggetho(dt, aes(y=paste(as.Date(datetime),  id), z=asleep)) + 
			stat_tile_etho() 


dt[, LD := as.factor(ifelse(t %% hours(24) > hours(12), "D", "L"))]

stat_dt <- dt[, 
				.(sleep_days_02_07 = mean(asleep[t>days(2) & t < days(7)]),
				  sleep_days_08_13 = mean(asleep[t>days(8) & t < days(13)]),
				  sleep_days_11_13 = mean(asleep[t>days(11) & t < days(13)]),
				  sleep_days_08_10 = mean(asleep[t>days(8) & t < days(10)]),
				  sleep_days_05_07 = mean(asleep[t>days(5) & t < days(7)]),
				  sleep_days_02_04 = mean(asleep[t>days(2) & t < days(4)]),
				  quiet_days_02_07 = mean(!moving[t>days(2) & t < days(7)]),
				  quiet_days_08_13 = mean(!moving[t>days(8) & t < days(13)]),
				  quiet_days_11_13 = mean(!moving[t>days(11) & t < days(13)]),
				  quiet_days_08_10 = mean(!moving[t>days(8) & t < days(10)]),
				  quiet_days_05_07 = mean(!moving[t>days(5) & t < days(7)]),
				  quiet_days_02_04 = mean(!moving[t>days(2) & t < days(4)])
				  ),
				  by = c("id")]
				  
stat_dt <- rejoin(stat_dt)


pdf("tube_change_correlations.pdf", w=12,h=6)
# female correlation mat

as.data.table(select(stat_dt[sex=="F"], starts_with("sleep_")) %>% correlate(method = "spearman"))
as.data.table(select(stat_dt[sex=="M"], starts_with("sleep_")) %>% correlate(method = "spearman"))
as.data.table(select(stat_dt[sex=="F"], starts_with("quiet_")) %>% correlate(method = "spearman"))
as.data.table(select(stat_dt[sex=="M"], starts_with("quiet_")) %>% correlate(method = "spearman"))



		 
ggplot(stat_dt, aes(sleep_days_02_07, sleep_days_08_13, colour = sex, shape=sex, fill=sex)) +
			geom_smooth(method="lm")+
			geom_point(size=2, alpha=.5) + labs(x="Sleep before tube change", y="Sleep after tube change") +
			generic_theme
			
		 
ggplot(stat_dt, aes(quiet_days_02_07, quiet_days_08_13, colour = sex, shape=sex, fill=sex)) +
			geom_smooth(method="lm")+
			geom_point(size=2, alpha=.5) + labs(x="Quiescence before tube change", y="Quiescence after tube change") +
			generic_theme
			
#~ 			scale_fill_manual(values=FEMALE_MALE_PALETTE) +
#~ 			scale_colour_manual(values=FEMALE_MALE_PALETTE)			
			
dev.off()

