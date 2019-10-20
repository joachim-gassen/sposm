install.packages("wbstats")
install.packages("magick")

#####GET DATA FROM WORLD BANK 
###usage of wbstats: https://cengel.github.io/gearup2016/worldbank.html
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
  
####MAKING GIF
###writing if loop: https://www.datamentor.io/r-programming/if-else-statement/
###making gif using magick package: https://cran.r-project.org/web/packages/magick/vignettes/intro.html#animated_graphics
###changing y axis scale: https://www.datanovia.com/en/blog/ggplot-axis-limits-and-scales/
###changing axis labels: https://www.datanovia.com/en/blog/ggplot-axis-labels/
library(magick)
library(ggplot2)

img <- image_graph(600, 340, res = 96)
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
  
  print(temp_plot)
}

dev.off()
animation <- image_animate(img, fps = 2, loop = 1)
print(animation)

image_write(animation, "Percent_fem_birth.gif")

rm(temp_plot, temp_title)
