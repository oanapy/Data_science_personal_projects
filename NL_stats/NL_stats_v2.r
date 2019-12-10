@@ -0,0 +1,203 @@
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

stat_2009 <- read_delim("2009_statline.csv", 
                        ";", escape_double = FALSE, 
                        col_types = cols(Perioden = col_integer(), 
                                         `Wonen/Percentage gestapelde bouw (%)` = col_skip(), 
                                         `Wonen/Percentage leegstand (%)` = col_skip(), 
                                         `Wonen/Woningvoorraad (aantal)` = col_integer()), 
                        trim_ws = TRUE)
stat_2010 <- read_delim("2010_statline.csv", 
                        ";", escape_double = FALSE, col_types = cols(Perioden = col_integer(), 
                                                                     `Wonen/Percentage gestapelde bouw (%)` = col_skip(), 
                                                                     `Wonen/Percentage leegstand (%)` = col_skip()), 
                        trim_ws = TRUE)
stat_2011 <- read_delim("2011_statline.csv", 
                        ";", escape_double = FALSE, col_types = cols(Perioden = col_integer(), 
                                                                     `Wonen/Percentage gestapelde bouw (%)` = col_skip(), 
                                                                     `Wonen/Percentage leegstand (%)` = col_skip()), 
                        trim_ws = TRUE)
stat_2012 <- read_delim("2012_statline.csv", 
                        ";", escape_double = FALSE, col_types = cols(Perioden = col_integer(), 
                                                                     `Wonen/Percentage gestapelde bouw (%)` = col_skip(), 
                                                                     `Wonen/Percentage leegstand (%)` = col_skip()), 
                        trim_ws = TRUE)
stat_2013 <- read_delim("2013_statline.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
stat_2013 <- stat_2013 %>% mutate(Perioden = 2013)
stat_2014 <- read_delim("2014_statline.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
stat_2014 <- stat_2014 %>% mutate(Perioden = 2014)
stat_2015 <- read_delim("2015_statline.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
stat_2015 <- stat_2015 %>% mutate(Perioden = 2015)



stat_2009 <- stat_2009 %>% filter(`Regiocodering/Soort regio (omschrijving)` == 'Gemeente')
stat_2010 <- stat_2010 %>% filter(`Regiocodering/Soort regio (omschrijving)` == 'Gemeente')
stat_2011 <- stat_2011 %>% filter(`Regiocodering/Soort regio (omschrijving)` == 'Gemeente')
stat_2012 <- stat_2012 %>% filter(`Regiocodering/Soort regio (omschrijving)` == 'Gemeente')

stat_2013 <- stat_2013 %>% filter(`Regioaanduiding/Soort regio (omschrijving)` == 'Gemeente')
stat_2014 <- stat_2014 %>% filter(`Regioaanduiding/Soort regio (omschrijving)` == 'Gemeente')
stat_2015 <- stat_2015 %>% filter(`Regioaanduiding/Soort regio (omschrijving)` == 'Gemeente')




#select top 5 gem with highest avg income
stat_2009 <- stat_2009 %>% mutate(min_inc = min(`Inkomen/Persoonlijk inkomen/Gemiddeld inkomen per inwoner� (1 000 euro)`),
                                  norm_inc = `Inkomen/Persoonlijk inkomen/Gemiddeld inkomen per inwoner� (1 000 euro)` / min_inc,
                                  min_won = min(`Wonen/Gemiddelde woningwaarde (1 000 euro)`),
                                  norm_won = `Wonen/Gemiddelde woningwaarde (1 000 euro)`/min_won) %>%
    dplyr::arrange(norm_inc) %>% distinct(norm_inc, .keep_all = T)
stat_2010 <- stat_2010 %>% mutate(min_inc = min(`Inkomen/Persoonlijk inkomen/Gemiddeld inkomen per inwoner� (1 000 euro)`),
                                  norm_inc = `Inkomen/Persoonlijk inkomen/Gemiddeld inkomen per inwoner� (1 000 euro)` / min_inc,
                                  min_won = min(`Wonen/Gemiddelde woningwaarde (1 000 euro)`),
                                  norm_won = `Wonen/Gemiddelde woningwaarde (1 000 euro)`/min_won) %>%
    dplyr::arrange(norm_inc) %>% distinct(norm_inc, .keep_all = T)

stat_2011 <- stat_2011 %>% mutate(min_inc = min(`Inkomen/Persoonlijk inkomen/Gemiddeld inkomen per inwoner� (1 000 euro)`),
                                  norm_inc = `Inkomen/Persoonlijk inkomen/Gemiddeld inkomen per inwoner� (1 000 euro)` / min_inc,
                                  min_won = min(`Wonen/Gemiddelde woningwaarde (1 000 euro)`),
                                  norm_won = `Wonen/Gemiddelde woningwaarde (1 000 euro)`/min_won) %>%
    dplyr::arrange(norm_inc) %>% distinct(norm_inc, .keep_all = T)
stat_2012 <- stat_2012 %>% mutate(min_inc = min(`Inkomen/Persoonlijk inkomen/Gemiddeld inkomen per inwoner� (1 000 euro)`),
                                  norm_inc = `Inkomen/Persoonlijk inkomen/Gemiddeld inkomen per inwoner� (1 000 euro)` / min_inc,
                                  min_won = min(`Wonen/Gemiddelde woningwaarde (1 000 euro)`),
                                  norm_won = `Wonen/Gemiddelde woningwaarde (1 000 euro)`/min_won) %>%
    dplyr::arrange(norm_inc) %>% distinct(norm_inc, .keep_all = T)


dfs <- list(stat_2009, stat_2010, stat_2011, stat_2012)

# TOP 5
rm(top_5_all)
top_5_all <- data.frame()
rm(top_5)
for (i in 1:length(dfs)) {
    print(length(dfs))
    print(i)
    #print(dfs[[i]])
    #df[["Inkomen/Persoonlijk inkomen/Gemiddeld inkomen per inwoner  (1 000 euro)"]]
    top_5 <- dfs[[i]][1:5,]
    print('next year')
    top_5_all <- rbind(top_5_all, top_5)
    #, last_5)
}

#LAST 5
rm(last_5_all)
last_5_all <- data.frame()
rm(last_5)
for (i in 1:length(dfs)) {
    print(i)
    last_5 <- tail(dfs[[i]])
    print('second')
    last_5_all <- rbind(last_5_all, last_5)
}

all_dfs <- rbind(top_5_all, last_5_all)

p <- all_dfs %>% 
    plot_ly(
        x = ~jitter(as.numeric(norm_won), 10), 
        y = ~norm_inc, 
        #size = ~norm_inc,
        color = ~`Regiocodering/Gemeentenaam (naam)`, 
        frame = ~Perioden, 
        text = ~paste(`Regiocodering/Gemeentenaam (naam)`,
                      Perioden, sep = " "), 
        #hoverinfo = "text",
        type = 'scatter',
        mode = 'text',
        #mode = 'markers',
        showlegend = F
    ) %>%
    layout(title = 'Primates Brain and Body Weight',
           xaxis = list(title = 'Body Weight (kg)',
                        zeroline = TRUE,
                        range = c(0,5)),
           yaxis = list(title = 'Brain Weight (g)',
                        range = c(0,25)))
p
##alternaive to show gem's that do not appear in all the years of registration
#par(mfrow=c(1,2))

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
          "000000", "#C3D7A4")

p <- ggplot(top_5_all, aes(x = norm_won, 
                         y = norm_inc,
                         color = `Regiocodering/Gemeentenaam (naam)`,
                         size = norm_inc)) +
    geom_point(aes(frame = Perioden)
               #,
               #color = 'black'
               ) + 
    geom_text(label = paste(top_5_all$`Regiocodering/Gemeentenaam (naam)`,
                            top_5_all$Perioden, sep = " "),
                             size = 4,
              position=position_jitter(width = 0.03,
                                       height = 0.02)) +
    scale_color_viridis_d(option = "E") +
    expand_limits(x = 1, y = 1) +
    coord_cartesian(xlim =c(1, 2.5), ylim = c(1, 1.3)) + 
    theme_classic() +
    theme(legend.position = 'none') 

q <- ggplot(last_5_all, aes(x = norm_won, 
                            y = norm_inc,
                            color = `Regiocodering/Gemeentenaam (naam)`,
                            size = norm_inc)) +
    geom_point(aes(frame = Perioden)) + 
    geom_text(label = paste(last_5_all$`Regiocodering/Gemeentenaam (naam)`,
                            last_5_all$Perioden, sep = " "),
              size = 4,
              position=position_jitter(width = 0.03,
                                       height = 0.02)) +
    scale_color_viridis_d(option = "D") +
    #expand_limits(x = 1, y = 1) +
    coord_cartesian(xlim =c(2.5, 5), ylim = c(16.5,21)) + 
    theme_classic() +
    theme(legend.position = 'none') 

ggplotly(p)
ggplotly(q)

s <- subplot(ggplotly(p), ggplotly(q)) %>%
    layout(title = "Top 5 and last 5 gemeente's for income/house value",
           legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center"),
           #width=800,
           #height=600, #or more
           autosize = T)
s
####################################


stat <- rbind.data.frame(stat_2009, stat_2010, stat_2011, stat_2012)

p <- stat_2015 %>%
    plot_ly(
        x = ~`Inkomen/Inkomen van personen/Aantal inkomensontvangers�� (aantal)`,
        y = ~`Inkomen/Inkomen van personen/Gemiddeld inkomen per inwoner� (x 1 000 euro)`,
        frame = ~Perioden,
        type = 'scatter',
        mode = 'markers',
        showlegend = T
    )
p