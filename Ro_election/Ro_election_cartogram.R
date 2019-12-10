@@ -0,0 +1,608 @@
setwd("C:/Users/Gins - SI/Desktop/Ro_election/")
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(pacman)
library(cartogram)    # for the cartogram
library(ggplot2)      # to realize the plots
library(broom)        # from geospatial format to data frame
library(tweenr)       # to create transition dataframe between 2 states
library(gganimate)    # To realize the animation
library(maptools)     # world boundaries coordinates
library(viridis)      # for a nice color palette
library(maptools)
library(raster)
library(rgdal)
library(cowplot)

pv_RO_PRSD_FINAL <- read_csv("C:/Users/Gins - SI/Desktop/Ro_election/pv_RO_PRSD_FINAL.csv")
#View(pv_RO_PRSD_FINAL)

#c = voturi valabile
#b = voturi totale/sectie votare

#names(pv_RO_PRSD_FINAL)

pv_RO_PRSD_FINAL <- pv_RO_PRSD_FINAL %>%
    mutate(prezenta_prc_valabile_din_nr_total_prezenti = (C * 100) / B,
           nule_prc_din_nr_total_prezenti = (D * 100) / B)

#Calcul procent Johannis din total voturi valabile
prc_Johannis <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Localitate, 'Judet', C, G1) %>%
    dplyr::mutate(prc_Johannis = (G1 * 100) / C) %>% filter(Judet != 'Bucharest')

#calcul voturi totale per candidat ca procent
remove(votes_per_candidate_from_total)
votes_per_candidate_from_total = data.frame(candidate_code = rep(0, 14),
                                            total_votes = rep(0,14),
                                            prc_from_total_votes_RO = rep(0,14))
total_valid_votes_RO <- pv_RO_PRSD_FINAL %>% 
    summarise(total_votes = sum(C))
idx <- c(19:32)
j <- 1
for (i in idx) {
    print(colnames(pv_RO_PRSD_FINAL[i]))
    votes_per_candidate_from_total[j, ] = c(colnames(pv_RO_PRSD_FINAL[i]),
                                            sum(pv_RO_PRSD_FINAL[[i]]),
                                            (sum(pv_RO_PRSD_FINAL[[i]]) * 100)/total_valid_votes_RO/100)
    j <- j + 1
}

#download basic ro map with polygones
Ro <-getData("GADM", country="RO", level=1)
# A basic representation
plot(Ro)

############################Johannis##########################
#count votes per Judet
prc_Johannis_judet <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Judet, G1) %>%
    dplyr:: filter(Judet != 'Bucharest' & Judet != 'Ilfov') %>%
    dplyr::group_by(Judet) %>%
    summarise(count_votes = sum(G1))

#make Ilfov and B as one due to plotting issues
prc_Johannis_B_IF <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Judet, G1) %>%
    dplyr::filter(Judet == 'Bucharest' | Judet == 'Ilfov')%>%
    dplyr::group_by(Judet) %>%
summarise(count_votes = sum(G1))

#add the results from Ilfov and B as a sum count_votes to the main dataframe with votes
prc_Johannis_judet<- dplyr::add_row(prc_Johannis_judet,
                                    Judet = 'Ilfov',
                                    count_votes = sum(prc_Johannis_B_IF$count_votes))

#merge count_votes with polygone dataframe
wrld_simpl_Johannis <-  sp::merge(Ro,
                                  prc_Johannis_judet,
                                  by.x="NAME_1",
                                  by.y='Judet')

#create basic cartogram
RO_cartogram_Johannis <- cartogram_cont(wrld_simpl_Johannis,
                                        'count_votes',
                                        itermax=7)

# A basic representation
plot(RO_cartogram_Johannis)

# Transform these 2 objects in dataframe, plotable with ggplot2
RO_cartogram_df_Johannis <-  as.data.frame(RO_cartogram_Johannis)
RO_cartogram_df_Johannis$.id <- as.numeric(rownames(RO_cartogram_df_Johannis)) 
RO_cartogram_df_Johannis_final <- sp::merge(tidy(RO_cartogram_Johannis),
                                            RO_cartogram_df_Johannis,
                                            by.x = "id",
                                            by.y = ".id")

#make cartogram with ggplot
p <- ggplot() +
    geom_polygon(data = RO_cartogram_df_Johannis_final,
                 aes(fill = count_votes/100000,
                     x = long,
                     y = lat, 
                     group = group) ,
                 size=0,
                 alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="Votes \n (x 100.000)") +
    ggtitle("Romania, 1st round elections, 2019",
            subtitle="Klaus Johannis" )+
    theme(text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f4",
                                       color = NA), 
        panel.background = element_rect(fill = "#f5f5f4",
                                        color = NA), 
        legend.background = element_rect(fill = "#f5f5f4",
                                         color = NA),
        plot.title = element_text(size= 22,
                                  hjust=0.5,
                                  color = "#4e4d47",
                                  margin = margin(b = -0.1,
                                                  t = 0.4,
                                                  l = 2,
                                                  unit = "cm")),
        plot.subtitle = element_text(size= 20,
                                     hjust=0.5,
                                     color = "#4e4d47", 
                                     margin = margin(b = -0.1,
                                                     t = 0.4,
                                                     l = 2,
                                                     unit = "cm")),
        legend.justification = 'center',
        legend.position=c(0.075,0.18))


# Barplot
q <- ggplot(votes_per_candidate_from_total %>% filter(prc_from_total_votes_RO > 0.05),
            aes(reorder(candidate_code, prc_from_total_votes_RO),
                prc_from_total_votes_RO,
                fill = ifelse(candidate_code == "G1",
                              "Highlighted",
                              "Normal"))) + 
    geom_col(position = 'dodge') +
    ylab("% of total votes") +
    coord_flip() +
    theme(plot.background = element_rect(fill = "#f5f5f4",
                                         color = NA), 
          panel.background = element_rect(fill = "#f5f5f4",
                                          color = NA)) +
    scale_y_continuous(labels = scales::percent) + 
    theme(legend.position = "none",
          axis.title.y=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
          )

#overlap map with barplot
ggdraw() +
    draw_plot(p, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(q, x = .78, y = .7, width = .22, height = .3) 


#save plot
ggsave("frame_00.png", device = 'png')

############################Dancila##########################
#count votes per Judet
prc_Dancila_judet <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Judet, G5) %>%
    dplyr:: filter(Judet != 'Bucharest' & Judet != 'Ilfov') %>%
    dplyr::group_by(Judet) %>%
    summarise(count_votes = sum(G5))

#make Ilfov and B as one due to plotting issues
prc_Dancila_B_IF <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Judet, G5) %>%
    dplyr::filter(Judet == 'Bucharest' | Judet == 'Ilfov')%>%
    dplyr::group_by(Judet) %>%
    summarise(count_votes = sum(G5))

#add the results from Ilfov and B as a sum count_votes to the main dataframe with votes
prc_Dancila_judet<- dplyr::add_row(prc_Dancila_judet,
                                    Judet = 'Ilfov',
                                    count_votes = sum(prc_Dancila_B_IF$count_votes))

#merge count_votes with polygone dataframe
wrld_simpl_Dancila <-  sp::merge(Ro,
                                  prc_Dancila_judet,
                                  by.x="NAME_1",
                                  by.y='Judet')

#create basic cartogram
RO_cartogram_Dancila <- cartogram_cont(wrld_simpl_Dancila,
                                        'count_votes',
                                        itermax=7)

# A basic representation
plot(RO_cartogram_Dancila)

# Transform these 2 objects in dataframe, plotable with ggplot2
RO_cartogram_df_Dancila <-  as.data.frame(RO_cartogram_Dancila)
RO_cartogram_df_Dancila$.id <- as.numeric(rownames(RO_cartogram_df_Dancila)) 
RO_cartogram_df_Dancila_final <- sp::merge(tidy(RO_cartogram_Dancila),
                                            RO_cartogram_df_Dancila,
                                            by.x = "id",
                                            by.y = ".id")

#make cartogram with ggplot
p <- ggplot() +
    geom_polygon(data = RO_cartogram_df_Dancila_final,
                 aes(fill = count_votes/100000,
                     x = long,
                     y = lat, 
                     group = group) ,
                 size=0,
                 alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="Votes \n (x 100.000)") +
    ggtitle("Romania, 1st round elections, 2019",
            subtitle="Viorica Dancila" )+
    theme(text = element_text(color = "#22211d"), 
          plot.background = element_rect(fill = "#f5f5f4",
                                         color = NA), 
          panel.background = element_rect(fill = "#f5f5f4",
                                          color = NA), 
          legend.background = element_rect(fill = "#f5f5f4",
                                           color = NA),
          plot.title = element_text(size= 22,
                                    hjust=0.5,
                                    color = "#4e4d47",
                                    margin = margin(b = -0.1,
                                                    t = 0.4,
                                                    l = 2,
                                                    unit = "cm")),
          plot.subtitle = element_text(size= 20,
                                       hjust=0.5,
                                       color = "#4e4d47", 
                                       margin = margin(b = -0.1,
                                                       t = 0.4,
                                                       l = 2,
                                                       unit = "cm")),
          legend.justification = 'center',
          legend.position=c(0.075,0.18))


# Barplot
q <- ggplot(votes_per_candidate_from_total %>% filter(prc_from_total_votes_RO > 0.05),
            aes(reorder(candidate_code, prc_from_total_votes_RO),
                prc_from_total_votes_RO,
                fill = ifelse(candidate_code == "G5",
                              "Highlighted",
                              "Normal"))) + 
    geom_col(position = 'dodge') +
    ylab("% of total votes") +
    coord_flip() +
    theme(plot.background = element_rect(fill = "#f5f5f4",
                                         color = NA), 
          panel.background = element_rect(fill = "#f5f5f4",
                                          color = NA)) +
    scale_y_continuous(labels = scales::percent) + 
    theme(legend.position = "none",
          axis.title.y=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
    )

#overlap map with barplot
ggdraw() +
    draw_plot(p, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(q, x = .78, y = .7, width = .22, height = .3) 


#save plot
ggsave("frame_01.png", device = 'png')

###########################Barna#############
#count votes per Judet
prc_Barna_judet <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Judet, G3) %>%
    dplyr:: filter(Judet != 'Bucharest' & Judet != 'Ilfov') %>%
    dplyr::group_by(Judet) %>%
    summarise(count_votes = sum(G3))

#make Ilfov and B as one due to plotting issues
prc_Barna_B_IF <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Judet, G3) %>%
    dplyr::filter(Judet == 'Bucharest' | Judet == 'Ilfov')%>%
    dplyr::group_by(Judet) %>%
    summarise(count_votes = sum(G3))

#add the results from Ilfov and B as a sum count_votes to the main dataframe with votes
prc_Barna_judet<- dplyr::add_row(prc_Barna_judet,
                                   Judet = 'Ilfov',
                                   count_votes = sum(prc_Barna_B_IF$count_votes))

#merge count_votes with polygone dataframe
wrld_simpl_Barna <-  sp::merge(Ro,
                                 prc_Barna_judet,
                                 by.x="NAME_1",
                                 by.y='Judet')

#create basic cartogram
RO_cartogram_Barna <- cartogram_cont(wrld_simpl_Barna,
                                       'count_votes',
                                       itermax=7)

# A basic representation
plot(RO_cartogram_Barna)

# Transform these 2 objects in dataframe, plotable with ggplot2
RO_cartogram_df_Barna <-  as.data.frame(RO_cartogram_Barna)
RO_cartogram_df_Barna$.id <- as.numeric(rownames(RO_cartogram_df_Barna)) 
RO_cartogram_df_Barna_final <- sp::merge(tidy(RO_cartogram_Barna),
                                           RO_cartogram_df_Barna,
                                           by.x = "id",
                                           by.y = ".id")

#make cartogram with ggplot
p <- ggplot() +
    geom_polygon(data = RO_cartogram_df_Barna_final,
                 aes(fill = count_votes/100000,
                     x = long,
                     y = lat, 
                     group = group) ,
                 size=0,
                 alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="Votes \n (x 100.000)") +
    ggtitle("Romania, 1st round elections, 2019",
            subtitle="Dan Barna" )+
    theme(text = element_text(color = "#22211d"), 
          plot.background = element_rect(fill = "#f5f5f4",
                                         color = NA), 
          panel.background = element_rect(fill = "#f5f5f4",
                                          color = NA), 
          legend.background = element_rect(fill = "#f5f5f4",
                                           color = NA),
          plot.title = element_text(size= 22,
                                    hjust=0.5,
                                    color = "#4e4d47",
                                    margin = margin(b = -0.1,
                                                    t = 0.4,
                                                    l = 2,
                                                    unit = "cm")),
          plot.subtitle = element_text(size= 20,
                                       hjust=0.5,
                                       color = "#4e4d47", 
                                       margin = margin(b = -0.1,
                                                       t = 0.4,
                                                       l = 2,
                                                       unit = "cm")),
          legend.justification = 'center',
          legend.position=c(0.075,0.18))


# Barplot
q <- ggplot(votes_per_candidate_from_total %>% filter(prc_from_total_votes_RO > 0.05),
            aes(reorder(candidate_code, prc_from_total_votes_RO),
                prc_from_total_votes_RO,
                fill = ifelse(candidate_code == "G3",
                              "Highlighted",
                              "Normal"))) + 
    geom_col(position = 'dodge') +
    ylab("% of total votes") +
    coord_flip() +
    theme(plot.background = element_rect(fill = "#f5f5f4",
                                         color = NA), 
          panel.background = element_rect(fill = "#f5f5f4",
                                          color = NA)) +
    scale_y_continuous(labels = scales::percent) + 
    theme(legend.position = "none",
          axis.title.y=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
    )

#overlap map with barplot
ggdraw() +
    draw_plot(p, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(q, x = .78, y = .7, width = .22, height = .3) 


#save plot
ggsave("frame_02.png", device = 'png')

###########################Diaconu#############
#count votes per Judet
prc_Diaconu_judet <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Judet, G10) %>%
    dplyr:: filter(Judet != 'Bucharest' & Judet != 'Ilfov') %>%
    dplyr::group_by(Judet) %>%
    summarise(count_votes = sum(G10))

#make Ilfov and B as one due to plotting issues
prc_Diaconu_B_IF <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Judet, G10) %>%
    dplyr::filter(Judet == 'Bucharest' | Judet == 'Ilfov')%>%
    dplyr::group_by(Judet) %>%
    summarise(count_votes = sum(G10))

#add the results from Ilfov and B as a sum count_votes to the main dataframe with votes
prc_Diaconu_judet<- dplyr::add_row(prc_Diaconu_judet,
                                 Judet = 'Ilfov',
                                 count_votes = sum(prc_Diaconu_B_IF$count_votes))

#merge count_votes with polygone dataframe
wrld_simpl_Diaconu <-  sp::merge(Ro,
                               prc_Diaconu_judet,
                               by.x="NAME_1",
                               by.y='Judet')

#create basic cartogram
RO_cartogram_Diaconu <- cartogram_cont(wrld_simpl_Diaconu,
                                     'count_votes',
                                     itermax=7)

# A basic representation
plot(RO_cartogram_Diaconu)

# Transform these 2 objects in dataframe, plotable with ggplot2
RO_cartogram_df_Diaconu <-  as.data.frame(RO_cartogram_Diaconu)
RO_cartogram_df_Diaconu$.id <- as.numeric(rownames(RO_cartogram_df_Diaconu)) 
RO_cartogram_df_Diaconu_final <- sp::merge(tidy(RO_cartogram_Diaconu),
                                         RO_cartogram_df_Diaconu,
                                         by.x = "id",
                                         by.y = ".id")

#make cartogram with ggplot
p <- ggplot() +
    geom_polygon(data = RO_cartogram_df_Diaconu_final,
                 aes(fill = count_votes/10000,
                     x = long,
                     y = lat, 
                     group = group) ,
                 size=0,
                 alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="Votes \n (x 10.000)") +
    ggtitle("Romania, 1st round elections, 2019",
            subtitle="Mircea Diaconu" )+
    theme(text = element_text(color = "#22211d"), 
          plot.background = element_rect(fill = "#f5f5f4",
                                         color = NA), 
          panel.background = element_rect(fill = "#f5f5f4",
                                          color = NA), 
          legend.background = element_rect(fill = "#f5f5f4",
                                           color = NA),
          plot.title = element_text(size= 22,
                                    hjust=0.5,
                                    color = "#4e4d47",
                                    margin = margin(b = -0.1,
                                                    t = 0.4,
                                                    l = 2,
                                                    unit = "cm")),
          plot.subtitle = element_text(size= 20,
                                       hjust=0.5,
                                       color = "#4e4d47", 
                                       margin = margin(b = -0.1,
                                                       t = 0.4,
                                                       l = 2,
                                                       unit = "cm")),
          legend.justification = 'center',
          legend.position=c(0.075,0.18))


# Barplot
q <- ggplot(votes_per_candidate_from_total %>% filter(prc_from_total_votes_RO > 0.05),
            aes(reorder(candidate_code, prc_from_total_votes_RO),
                prc_from_total_votes_RO,
                fill = ifelse(candidate_code == "G10",
                              "Highlighted",
                              "Normal"))) + 
    geom_col(position = 'dodge') +
    ylab("% of total votes") +
    coord_flip() +
    theme(plot.background = element_rect(fill = "#f5f5f4",
                                         color = NA), 
          panel.background = element_rect(fill = "#f5f5f4",
                                          color = NA)) +
    scale_y_continuous(labels = scales::percent) + 
    theme(legend.position = "none",
          axis.title.y=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
    )

#overlap map with barplot
ggdraw() +
    draw_plot(p, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(q, x = .78, y = .7, width = .22, height = .3) 


#save plot
ggsave("frame_03.png", device = 'png')

###########################Paleologu#############
#count votes per Judet
prc_Paleologu_judet <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Judet, G2) %>%
    dplyr:: filter(Judet != 'Bucharest' & Judet != 'Ilfov') %>%
    dplyr::group_by(Judet) %>%
    summarise(count_votes = sum(G2))

#make Ilfov and B as one due to plotting issues
prc_Paleologu_B_IF <- pv_RO_PRSD_FINAL %>% 
    dplyr::select(Judet, G2) %>%
    dplyr::filter(Judet == 'Bucharest' | Judet == 'Ilfov')%>%
    dplyr::group_by(Judet) %>%
    summarise(count_votes = sum(G2))

#add the results from Ilfov and B as a sum count_votes to the main dataframe with votes
prc_Paleologu_judet<- dplyr::add_row(prc_Paleologu_judet,
                                   Judet = 'Ilfov',
                                   count_votes = sum(prc_Paleologu_B_IF$count_votes))

#merge count_votes with polygone dataframe
wrld_simpl_Paleologu <-  sp::merge(Ro,
                                 prc_Paleologu_judet,
                                 by.x="NAME_1",
                                 by.y='Judet')

#create basic cartogram
RO_cartogram_Paleologu <- cartogram_cont(wrld_simpl_Paleologu,
                                       'count_votes',
                                       itermax=7)

# A basic representation
plot(RO_cartogram_Diaconu)

# Transform these 2 objects in dataframe, plotable with ggplot2
RO_cartogram_df_Paleologu <-  as.data.frame(RO_cartogram_Paleologu)
RO_cartogram_df_Paleologu$.id <- as.numeric(rownames(RO_cartogram_df_Paleologu)) 
RO_cartogram_df_Paleologu_final <- sp::merge(tidy(RO_cartogram_Paleologu),
                                           RO_cartogram_df_Paleologu,
                                           by.x = "id",
                                           by.y = ".id")

#make cartogram with ggplot
p <- ggplot() +
    geom_polygon(data = RO_cartogram_df_Paleologu_final,
                 aes(fill = count_votes/10000,
                     x = long,
                     y = lat, 
                     group = group) ,
                 size=0,
                 alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="Votes \n (x 10.000)") +
    ggtitle("Romania, 1st round elections, 2019",
            subtitle="Theodor Paleologu" )+
    theme(text = element_text(color = "#22211d"), 
          plot.background = element_rect(fill = "#f5f5f4",
                                         color = NA), 
          panel.background = element_rect(fill = "#f5f5f4",
                                          color = NA), 
          legend.background = element_rect(fill = "#f5f5f4",
                                           color = NA),
          plot.title = element_text(size= 22,
                                    hjust=0.5,
                                    color = "#4e4d47",
                                    margin = margin(b = -0.1,
                                                    t = 0.4,
                                                    l = 2,
                                                    unit = "cm")),
          plot.subtitle = element_text(size= 20,
                                       hjust=0.5,
                                       color = "#4e4d47", 
                                       margin = margin(b = -0.1,
                                                       t = 0.4,
                                                       l = 2,
                                                       unit = "cm")),
          legend.justification = 'center',
          legend.position=c(0.075,0.18))


# Barplot
q <- ggplot(votes_per_candidate_from_total %>% filter(prc_from_total_votes_RO > 0.05),
            aes(reorder(candidate_code, prc_from_total_votes_RO),
                prc_from_total_votes_RO,
                fill = ifelse(candidate_code == "G2",
                              "Highlighted",
                              "Normal"))) + 
    geom_col(position = 'dodge') +
    ylab("% of total votes") +
    coord_flip() +
    theme(plot.background = element_rect(fill = "#f5f5f4",
                                         color = NA), 
          panel.background = element_rect(fill = "#f5f5f4",
                                          color = NA)) +
    scale_y_continuous(labels = scales::percent) + 
    theme(legend.position = "none",
          axis.title.y=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
    )

#overlap map with barplot
ggdraw() +
    draw_plot(p, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(q, x = .78, y = .7, width = .22, height = .3) 


#save plot
ggsave("frame_04.png", device = 'png')

#animate in cmd -> ffmpeg -r 0.2 -i frame_%02d.png -c:v libx264 -vf fps=25 -crf 17 -pix_fmt yuv420p ex.mp4