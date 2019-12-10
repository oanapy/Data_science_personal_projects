setwd("C:/Users/Gins - SI/Desktop/NL_stats/")
library(readr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(sf)
library(sp)
library(tidyverse)
library(plot3D)
library(plotly)
library(rayshader)
library(av)

###TODO 
#1. approach to get a map of the real NL map in the background - https://www.jessesadler.com/post/geocoding-with-r/
#2. second approach if #1 fails https://www.littlemissdata.com/blog/maps
#3.get the data from cbs/statline - https://opendata.cbs.nl/statline/portal.html?_la=nl&_catalog=CBS&tableId=70904ned&_theme=734
#4. join the wijk geolocation from the shapefile to the real income data
#5. test the 3d plot below
#6. find a way to show growth in several years/maybe as a timseries approach as to how the incme increases per neighborhood


#cbs_buurten_2018 <- read_csv("cbs_buurten_2018.csv", 
#                             na = "-99999999")

test <- st_read("wijkenbuurten2018/Uitvoer_shape/wijk_2018.shp")
test <- test[test$WATER == 'NEE',]
test <- st_transform(test, CRS("+proj=longlat"))
#test$WK_CODE <- as.character(test$WK_CODE)
#plot(test)

stat_2009 <- read_delim("2009_statline.csv", 
                        ";", 
                        escape_double = FALSE, 
                        trim_ws = TRUE)
                        
stat_2010 <- read_delim("2010_statline.csv", 
                        ";", 
                        escape_double = FALSE, 
                        trim_ws = TRUE)
#stat_2011 <- 
stat_2009 <- stat_2009 %>% filter(`Regiocodering/Soort regio (omschrijving)` == 'Wijk')
stat_2010 <- stat_2010 %>% filter(`Regiocodering/Soort regio (omschrijving)` == 'Wijk')

stat_09_10 <- rbind.data.frame(stat_2009, stat_2010)


#test <- left_join(stat_09_10, test,  by = c('Regiocodering/Codering ()' = 'WK_CODE'))

#test <- test %>% 
#    left_join(stat_09_10, by = c('WK_CODE' = 'Regiocodering/Codering ()'))
#%>% 
#    filter(as.numeric("Inkomen/Persoonlijk inkomen/Gemiddeld inkomen per inwoner  (1 000 euro)") > 0 & 
#               !is.na(Perioden))

mtplot <- ggplot() +
    geom_sf(data = test, aes(fill = AANT_MAN)) +
    scale_color_gradientn(colours = terrain.colors(10))
mtplot

#+
 #   scale_color_gradientn(colours = terrain.colors(10))
#+
    facet_wrap(~ Perioden) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          legend.position = 'none',
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())

mtplot

# using sf
#sf_cent <- st_point_on_surface(test)
#sf_cent <- st_transform(sf_cent, CRS("+proj=longlat"))


#separated_coord <- sf_cent %>%
#    mutate(lat = unlist(map(sf_cent$geometry,2)),
#           long = unlist(map(sf_cent$geometry,1)))

#write.csv(separated_coord, 'separated_coord.csv')


#df <- separated_coord %>% st_drop_geometry() %>% filter(AANT_MAN > 0)


#p <- plot_ly(data = df , x = df$long, y = df$lat, z = df$AANT_INW, type = "mesh3d",
            # intensity=df$AANT_INW,
             #colorscale="Viridis")
#p


#mtplot = ggplot(df) + 
#    geom_point(aes(x = long, y = lat, color = AANT_MAN)) + 
#    scale_colour_gradientn(colours = terrain.colors(10)) +
#    theme(axis.line=element_blank(),
#          axis.text.x=element_blank(),
#          axis.text.y=element_blank(),
#          axis.ticks=element_blank(),
#          axis.title.x=element_blank(),
#          axis.title.y=element_blank(),
#          panel.background=element_blank(),
#          panel.border=element_blank(),
#          panel.grid.major=element_blank(),
#          panel.grid.minor=element_blank(),
#          plot.background=element_blank())
#mtplot

#par(mfrow = c(1, 2))
plot_gg(mtplot, height = 5, width = 7, 
        scale = 250, zoom = 0.6, 
        windowsize = c(1400, 800),
        multicore = TRUE)
render_snapshot("hex_plot.png")
render_depth(focus = 0.5, focallength = 15, filename = "hex_plot")
render_movie(filename = "hex_plot_orbit", type = "orbit",
             phi = 45, theta = 60)