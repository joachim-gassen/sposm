install.packages("wbstats")
###or require(installr)
###install.ImageMagick()
###install legacy packages: convert!!
###seems to work only under windows?

#####GET DATA FROM WORLD BANK 
###usage of wbstats: https://cengel.github.io/gearup2016/worldbank.html
dir.create("assignment1")
setwd("assignment1")
library(wbstats)

new_wb_cache <- wbcache()
wbsearch("sex ratio at birth", cache = new_wb_cache)
##wbsearch("suicide mortality rate", cache = new_wb_cache)

wb_dat <- wb(indicator = c("SP.POP.BRTH.MF")) 

####CLEANING AND DATA TRANSFORMATION
library(dplyr)
keepc <- c("China", "East Asia & Pacific", "European Union", "North America", "Latin America & Caribbean", "Sub-Saharan Africa")
keepy <- c(seq(from=1962, to=2007, by=5), 2010:2017)

wb_dat <- filter(wb_dat, country %in% keepc)
wb_dat <- mutate(wb_dat, p_female = 1/(1+value))
  
####MAKING BARPLOTS ACROSS YEARS
###writing if loop: https://www.datamentor.io/r-programming/if-else-statement/
###writing loop and save iteration results in ggplot: https://stackoverflow.com/questions/26034177/save-multiple-ggplots-using-a-for-loop
###changing y axis scale: https://www.datanovia.com/en/blog/ggplot-axis-limits-and-scales/
###changing axis labels: https://www.datanovia.com/en/blog/ggplot-axis-labels/
library(ggplot2)
for (year in keepy) {
  temp_title = if (year <= 2007) {
    paste("Percentage of Female Birth", as.numeric(year)-2,"-", as.numeric(year)+3)
  } else {
    paste("Percentage of Female Birth", year)
  }
    
  temp_plot = ggplot(subset(wb_dat, date == year)) + 
    geom_bar(mapping = aes(x = country, fill = country, y = p_female), stat="identity") +
    coord_cartesian(ylim = c(0.46, 0.50)) +
    theme(plot.title = element_text(color = "black", size = 18, face = "bold"), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggtitle(temp_title) +
    labs(caption="Data source: World Bank Gender Statistics") +
    guides(fill=guide_legend(title="Country/Region")) +
    scale_fill_manual("legend", values = c("China" = "red", "East Asia & Pacific" = "orange", "European Union" = "yellow", "Latin America & Caribbean" = "green", "North America" = "blue", "Sub-Saharan Africa" = "purple"))
  
  ggsave(temp_plot, file = paste0(year,".png"), dpi = 80, units = "in")
}

rm(temp_plot, temp_title)

###Gif making: https://ryouready.wordpress.com/2010/11/21/animate-gif-images-in-r-imagemagick/
###Gif loop pause: https://stackoverflow.com/questions/40191000/imagemagick-convert-command-set-delay-time-for-the-last-frame
system("E:/imageMagick-7.0.8-Q16/convert -delay 80 1962.png 1967.png 1972.png 1977.png 1982.png 1987.png 1992.png 1997.png 2002.png 2007.png 2010.png 2011.png 2012.png 2013.png 2014.png 2015.png 2016.png -delay 500 2017.png Percentag_Fem_Birth.gif")

file.remove(list.files(pattern=".png"))
