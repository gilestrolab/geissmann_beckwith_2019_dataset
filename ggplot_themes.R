# This file contains our conventions to represent ggplot graphs in the lab.
# Ideally, you will save it in your workspace, and source it (`source(ggplot_themes.R)`)
# then, you can simply add themes to plots. For instance:
## make the plot:
# `pl <- ethogramPlot(...)`
# # change theme and show:
# `pl + ethogram_theme`

# This is designed to for pdf of size 8x6 inches. 
# So, unless you have a good reason to use a different size, you can do:
# `pdf("my_file_name.pdf", w=8,h=6)`
# `do stuf...`
# `dev.off`

ethogram_theme <- theme_bw() + theme(	 axis.text = element_text(size = 12),
					 panel.grid.major = element_line(colour = "grey80"),
					 panel.grid.major.x =element_blank(),
					 panel.grid.minor = element_blank(),
					 legend.position = "bottom",
					 legend.title =element_blank())


# this theme is for generic scatterplots, boxplot, barplot...
generic_theme <- theme_bw()+ theme( 	axis.text = element_text(size = 12),
					axis.title=element_text(size=14),
					axis.text.x = element_text(angle=60, vjust=1.1, hjust=1),
					panel.grid.major = element_line(colour = "grey80"),
					#panel.grid.major.x =element_blank(),
					panel.grid.minor = element_blank(),
					legend.position = "none")


FEMALE_MALE_PALETTE <- c("#be2828ff", "#282896ff")
CONTROL_SD_PALETTE <- c( "#969696ff", "#3caa3cff")

annotateEthoPlot <- function(plot, title = "", palette = FEMALE_MALE_PALETTE, y
									=expression(frac(sleeping~time,total~time))){
	makeLDAnnotation(plot + labs(x="time (day)", 
					y = y,
					title = title)  +
					scale_y_continuous(limits=c(NA,1))+
					scale_fill_manual(values=palette) + scale_colour_manual(values=palette) +
					ethogram_theme 
			)
			
	}
