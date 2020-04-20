# Figure layout and reference for the manuscript


## Fig 1, baseline sleep/quiescence


* summary : Undisturbed baseline behaviour for four full days males vs females.
* experiment -- `20160404_overnight_dsd`
* script -- `baseline.R`
* Ns: N_M =  485, N_F = 881
* figure material: 
	1. `overnight_dsd_baseline.pdf`, page 1 -- **sleep** fraction.
	2. `overnight_dsd_baseline.pdf`, page 2 -- **quiescence** (i.e. not moving) fraction.
	3. `overnight_dsd_baseline.pdf`, page 3 --  like 1., but wrapped over 24h
	4. `overnight_dsd_baseline.pdf`, page 4 --  like 2., but wrapped over 24h
	5. `sorted_baseline_overview.pdf`, pages 1 & 2 -- distribution of individual **sleep** in **females**
	6. `sorted_baseline_overview.pdf`, pages 3 & 4 -- distribution of individual **sleep** in **males**
	7. `sorted_baseline_overview.pdf`, pages 5 & 6 -- distribution of individual **quiescence** in **females**
	8. `sorted_baseline_overview.pdf`, pages 7 & 8 -- distribution of individual **quiescence** in **males**
	9. `sorted_baseline_overview.svg` example of how to put figures marginal distribution and overview together
	10. `sorted_baseline_overview.pdf`, page 9 -- like page 2. but showing the 19 females for which we have a video
	11. `sorted_baseline_overview.pdf`, page 10 -- the 19 picked females, as an overview plot. The names on the y axis indicates where to find the video. It reads `<experiment_id>|<region_id> -> <video_id>|<region_id>`
	
	
	
	
## Fig 2, baseline ternary  plots


* summary : same data as fig 1. Introducing the triangle.
* experiment -- `20160404_overnight_dsd`
* script -- `baseline.R`
* Ns are as above
* figure material: 
	1. `overnight_dsd_baseline.pdf`, page 5 --  individual fine grain behaviour for the 19 females (see fig 1), over 48h day, 1min resolution (same data as used in triangle, linking figure 1 and 2).	
	2. `ternary_plot_video.pdf`, all pages -- location in behavioural space for each animal (average across days to wrap on circadian time) 15min /page. Suggested conversion to gif: `convert -delay 20 -density 200x200  -loop 0  ternary_plot_video.pdf  -background white  -alpha remove -alpha off  -coalesce -layers optimize  ternary_plot.gif`
	3. `ternary_plot.pdf`, page 10 -- summary of 1., wraped in one figure.
	4. `ternary_plot.pdf`, page 1 and `ternary_female_path.pdf`, page 1 -- suggested figure legend for 2.
	5. `ternary_plot.pdf`, page 2-9 -- like 2. but for different population quantiles (based on sleep amount)
	6. `ternary_plot.pdf`, page 11 -- Overall relative position of animals given their behaviour and sex. For females, micro-mov. is on the food!
	7. `sleep_dam_overestimate.pdf` page 1 -- relationship between dam-scored and ethoscope-scored sleep (global average, L+D). We also show overall proportion of micromovement as dot size

## Fig 3, virgin vs mated females

* summary : Effect of mating on female sleep/quiescence
* experiment -- `20170814_virgin_vs_mated`
* script -- `script.R`
* N_mated > 85, N_virgin > 150
* figure material: 
	1. `virgin_vs_mated.pdf`, page 1 -- **sleep** fraction. 
	2. `virgin_vs_mated.pdf`, page 2 -- **quiescence** fraction.
	3. `virgin_vs_mated.pdf`, page 3 -- **position** 0 <=> food, 1 <=> cotton wool.
	4. `virgin_vs_mated.pdf`, page 4 --  like 1., but wrapped over 24h (post-mating data only)
	5. `virgin_vs_mated.pdf`, page 5 --  like 2., but wrapped over 24h (post-mating data only)
	6. `virgin_vs_mated-ternary_plots.pdf`, page 2 --  ternary plot post-mating. `FALSE` and `TRUE` (i.e. left and right) facets are for virgin and mated, respectively.

## Fig 4, Sleep deprivation, 12h overnight.

* summary : Effect of sleep deprivation with 10 different (20 - 1000s) intervals.
* experiment -- `20160404_overnight_dsd`
* script -- `rebound.R`
* N > 45 / treatment * sex (see last page)
* figure material: 
	1. `overnight_dsd_rebound.pdf`, page 1 -- **sleep** fraction **20s vs control**.
	2. `overnight_dsd_rebound.pdf`, page 2 -- **sleep** fraction **20s vs control**, zoomed in.
	3. `overnight_dsd_rebound.pdf`, page 3 -- **sleep** fraction **all vs control**
	4. `overnight_dsd_rebound.pdf`, page 4 -- **quiescence** fraction **20s vs control**.
	5. `overnight_dsd_rebound.pdf`, page 5 -- **quiescence** fraction **20s vs control**, zoomed in.
	6. `overnight_dsd_rebound.pdf`, page 6 -- **quiescence** fraction **all vs control**
	7. `overnight_dsd_rebound.pdf`, page 7 -- **absolute number of stimuli** **20s vs control**.
	8. `overnight_dsd_rebound.pdf`, page 8 -- **absolute number of stimuli** **20s vs control**, zoomed in.
	9. `overnight_dsd_rebound.pdf`, page 9 -- **absolute number of stimuli** **all vs control**
	10. `overnight_dsd_rebound.pdf`, page 10 -- same as 7., but **relative** to theoritical max num of stimuli
	11. `overnight_dsd_rebound.pdf`, page 11 -- same as 8., but **relative** to theoritical max num of stimuli
	12. `overnight_dsd_rebound.pdf`, page 12 -- same as 9., but **relative** to theoritical max num of stimuli
	13. `overnight_dsd_rebound.pdf`, page 15 & 16 -- sleep amount in the 3 (page 15) or 6 hours following SD (ZT 0-[3|6]). Bootstrap error bars
	14. `overnight_dsd_rebound.pdf`, page 21 -- observed quiescence -  predicted quiescence in 3h (in min). Groups are to be compared to red dotted line (see lab meeting 20171102) the expectation. Stats are uncorrected Wilcoxon (unpaired) tests. 
	15. `overnight_dsd_rebound.pdf`, page 22 -- Probability of having a rebound (as defined above) > 0 in 3h (in min). Groups are to be compared to red line (50%), the expectation. Stats are uncorrected Binomial tests.

## Fig 5, prolonged SD.

* summary : Effect of 9.5 days sleep deprivation 20s intervals.
* experiment -- `20170209_prolonged_sd`
* script -- `rebound.R`
* N > 90 / treatment * sex (see last page)
* figure material: 
	1. `prolonged_sd_rebound.pdf`, page 1 -- **sleep** fraction **20s vs control**.
	2. `prolonged_sd_rebound.pdf`, page 2 -- **sleep** fraction **20s vs control**, zoomed in.
	3. `prolonged_sd_rebound.pdf`, page 3 -- **quiescence** fraction **20s vs control**.
	4. `prolonged_sd_rebound.pdf`, page 4 -- **quiescence** fraction **20s vs control**, zoomed in.
	5. `prolonged_sd_rebound.pdf`, page 5 -- **absolute number of stimuli** **20s vs control**.
	6. `prolonged_sd_rebound.pdf`, page 6 -- **absolute number of stimuli** **20s vs control**, zoomed in.
	7. `prolonged_sd_rebound.pdf`, page 8 -- **relative number of stimuli** (max = one each 20s) for the 20s interval group. wrapped over circadiand day, from `t = days(1)`.
	8. `prolonged_sd_rebound.pdf`, page 9 & 10 -- sleep amount in the 3 (page 15) or 6 hours following SD (ZT 0-[3|6]). Bootstrap error bars.
	9. `prolonged_sd_rebound.pdf`, page 15 -- observed quiescence -  predicted quiescence in 3h (in min). Groups are to be compared to red line, the expectation. Stats are uncorrected Wilcoxon (unpaired) tests.  #TODO  express in min #TODO  add some alpha
	10. `prolonged_sd_rebound.pdf`, page 16 -- Probability of having a rebound (as defined above) > 0 in 3h (in min). Groups are to be compared to red line (50%), the expectation. Stats are uncorrected Binomial tests.
	11. `prolonged_sd_surv.pdf`, page 1 -- Survival curve (includes animals that died before rebound, which are no in sleep trace data)
	

# Supplementary figures

## Sup fig A: DSD for 4 hours only

* summary : Effect of dynamic sleep deprivation from zt 20 to 24.
* experiment -- `20160722_time_window_dsd`
* script -- `rebound.R`
* N > 50 / treatment * sex (see last page)
* figure material: 
	1. `time_window_dsd_rebound.pdf`, page 1 -- **sleep** fraction **20s vs control**.
	2. `time_window_dsd_rebound.pdf`, page 2 -- **sleep** fraction **20s vs control**, zoomed in.
	3. `time_window_dsd_rebound.pdf`, page 3 -- **quiescence** fraction **20s vs control**.
	4. `time_window_dsd_rebound.pdf`, page 4 -- **quiescence** fraction **20s vs control**, zoomed in.
	5. `time_window_dsd_rebound.pdf`, page 5 -- **absolute number of stimuli** **20s vs control**.
	6. `time_window_dsd_rebound.pdf`, page 6 -- **absolute number of stimuli** **20s vs control**, zoomed in.
	8. `time_window_dsd_rebound.pdf`, page 9 & 10 -- sleep amount in the 3 (page 15) or 6 hours following SD (ZT 0-[3|6]). Bootstrap error bars.
	9. `time_window_dsd_rebound.pdf`, page 15 -- observed quiescence -  predicted quiescence in 3h (in min). Groups are to be compared to red line, the expectation. Stats are uncorrected Wilcoxon (unpaired) tests.  #TODO  express in min #TODO  add some alpha
	10. `time_window_dsd_rebound.pdf`, page 16 -- Probability of having a rebound (as defined above) > 0 in 3h (in min). Groups are to be compared to red line (50%), the expectation. Stats are uncorrected Binomial tests.
	

	
## Sup fig B: Static prolonged SD


* summary : Effect of 9.5 days sleep deprivation 20s intervals.
* experiment -- `20170209_prolonged_sd`
* script -- `rebound.R`
* N > 70 / treatment * sex (see last page)
* figure material: 
	1. `prolonged_sd_rebound.pdf`, page 1 -- **sleep** fraction **20s vs control**.
	2. `prolonged_sd_rebound.pdf`, page 2 -- **sleep** fraction **20s vs control**, zoomed in.
	3. `prolonged_sd_rebound.pdf`, page 3 -- **quiescence** fraction **20s vs control**.
	4. `prolonged_sd_rebound.pdf`, page 4 -- **quiescence** fraction **20s vs control**, zoomed in.
	5. `prolonged_sd_rebound.pdf`, page 5 -- **absolute number of stimuli** **20s vs control**.
	6. `prolonged_sd_rebound.pdf`, page 6 -- **absolute number of stimuli** **20s vs control**, zoomed in.
	7. `prolonged_sd_rebound.pdf`, page 8 -- **relative number of stimuli** (max = one each 20s) for the 20s interval group. wrapped over circadiand day, from `t = days(1)`.
	8. `prolonged_sd_rebound.pdf`, page 9 & 10 -- sleep amount in the 3 (page 15) or 6 hours following SD (ZT 0-[3|6]). Bootstrap error bars.
	9. `prolonged_sd_rebound.pdf`, page 15 -- observed quiescence -  predicted quiescence in 3h (in min). Groups are to be compared to red line, the expectation. Stats are uncorrected Wilcoxon (unpaired) tests.  #TODO  express in min #TODO  add some alpha
	10. `prolonged_sd_rebound.pdf`, page 16 -- Probability of having a rebound (as defined above) > 0 in 3h (in min). Groups are to be compared to red line (50%), the expectation. Stats are uncorrected Binomial tests.
	11. `prolonged_sd_surv.pdf`, page 1 -- Survival curve (incldes animals that died before rebound)
	
	
	
## Sup fig C, effect of changing the tube on behaviour conservation

* summary : Changing tube and shuffeling location => sleep pattern conserved!
* experiment -- `20170109_tube_change_and_var`
* script -- `script.R`
* N > 200 /  sex
* figure material: 
	1. `tube_change_correlations.pdf`, page 1 -- **sleep** fraction after vs before tube shcnage/shuffeling.
	2. `tube_change_correlations.pdf`, page same as 1, but for *quiescence*.
	



# Undiscussed figures

## Fig X, Sleep deprivation, 12h in L phase.


* summary : Effect of dynamic sleep deprivation from zt 00 to 12.
* experiment -- `20171017_dsd_l_phase`
* script -- `rebound.R`
* N > 60 / treatment * sex (see last page)
* figure material: 
	1. `l_phase_dsd_rebound.pdf`, page 1 -- **sleep** fraction **20s vs control**.
	2. `l_phase_dsd_rebound.pdf`, page 2 -- **sleep** fraction **20s vs control**, zoomed in.
	3. `l_phase_dsd_rebound.pdf`, page 3 -- **quiescence** fraction **20s vs control**.
	4. `l_phase_dsd_rebound.pdf`, page 4 -- **quiescence** fraction **20s vs control**, zoomed in.
	5. `l_phase_dsd_rebound.pdf`, page 5 -- **absolute number of stimuli** **20s vs control**.
	6. `l_phase_dsd_rebound.pdf`, page 6 -- **absolute number of stimuli** **20s vs control**, zoomed in.
	8. `l_phase_dsd_rebound.pdf`, page 9 & 10 -- TODO
	9. `l_phase_dsd_rebound.pdf`, page 15 -- TODO
	10. `l_phase_dsd_rebound.pdf`, page 16 -- TODO
	


## Fig Y, Sleep deprivation, 8h in L phase.


* summary : Effect of dynamic sleep deprivation from zt 00 to 8.
* experiment -- `20171017_dsd_zt0_8`
* script -- `rebound.R`
* N > 60 / treatment * sex (see last page)
* figure material: 
	1. `dsd_zt0_8_rebound.pdf`, page 1 -- **sleep** fraction **20s vs control**.
	2. `dsd_zt0_8_rebound.pdf`, page 2 -- **sleep** fraction **20s vs control**, zoomed in.
	3. `dsd_zt0_8_rebound.pdf`, page 3 -- **quiescence** fraction **20s vs control**.
	4. `dsd_zt0_8_rebound.pdf`, page 4 -- **quiescence** fraction **20s vs control**, zoomed in.
	5. `dsd_zt0_8_rebound.pdf`, page 5 -- **absolute number of stimuli** **20s vs control**.
	6. `dsd_zt0_8_rebound.pdf`, page 6 -- **absolute number of stimuli** **20s vs control**, zoomed in.
	8. `dsd_zt0_8_rebound.pdf`, page 9 & 10 -- TODO
	9. `dsd_zt0_8_rebound.pdf`, page 15 -- TODO
	10. `dsd_zt0_8_rebound.pdf`, page 16 -- TODO
