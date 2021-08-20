# Blood Drive Location Map
# Source: https://www.city-data.com/income/income-Charlottesville-Virginia.html

library(tidyverse)
library(readxl)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(viridis)

data3<-read_excel("/Users/yixintang/Redcross/blood drive/blood_drive_locations.xlsx",3)
# sheet 3

mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(15)
# color: https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf

# Map 
mapper <- get_stamenmap(bbox = c(left = -78.525, 
                                 bottom = 38.01,
                                 right = -78.45,
                                 top = 38.07), 
                        zoom=15,
                        maptype = "toner")

# Add blood drive points
ggmap(mapper) + geom_point(data = data3, 
                           aes(x = longitude, y = latitude,
                               color=factor(drive_location),
                               shape=factor(type)),
                           size=3)+ 
  xlab("Longitude") + ylab("Latitude")+
  scale_color_manual(values=mycolors,name='Possible Blood Drive Location')+
  scale_shape_manual(values=c(8,16),name='Type')




# google map: requires API key (not free)
# Couldn't outline the area
# https://rdrr.io/cran/ggmap/man/register_google.html
# https://stackoverflow.com/questions/30270011/ggmap-route-finding-doesnt-stay-on-roads




