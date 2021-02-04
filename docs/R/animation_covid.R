library(gganimate)
library(ggplot2)
library(tidycovid19)
library(tidyverse)
library(gifski)

jh<-download_jhu_csse_covid19_data()

confirmed_rank<-jh %>% 
    group_by(date)%>%      
    mutate(rank = rank(-confirmed),
           lab.confirm = paste0(" ", confirmed)) %>%
    group_by(country) %>%
    filter(rank <= 10)%>%
      ungroup()




anima_confirmed<-ggplot(confirmed_rank, aes(rank, group = country, 
                                            fill = as.factor(country), 
                                            color = as.factor(country)))+
    geom_tile(aes(y = confirmed/2,
                  height = confirmed,
                  width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1, size = 5) + 
    geom_text(aes(y=confirmed,label = lab.confirm, hjust=0),size = 5 ) +  
    coord_flip(clip = "off", expand = TRUE) +
    scale_x_reverse() +
    theme_minimal() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black"),
          plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"),
          plot.caption =element_text(size=13, hjust=1, color="black"),
          plot.margin = margin(1,3, 1, 3, "cm")) +
    transition_states(date, transition_length = 3, state_length = 1) +
    ease_aes('sine-in-out')+
    labs(title = '{closest_state}',  
         subtitle  =  "",
         caption  = "Unidad de M\u00e9todos y Acceso a Datos (UMAD)")

animate(anima_confirmed, nframes = 1000, duration=20,  width = 670, height = 600, end_pause = 80, renderer = gifski_renderer()) 
gganimate::anim_save(here::here('animations','bar_race.gif'))

##############################################################################
##############################################################################

deaths_rank<-jh %>% 
  group_by(date)%>%      
  mutate(rank = rank(-deaths),
         lab.deaths = paste0(" ", deaths)) %>%
  group_by(country) %>%
  filter(rank <= 10)%>%
  ungroup()


anima_deaths<-ggplot(deaths_rank, aes(rank, group = country, 
                                            fill = as.factor(country), 
                                            color = as.factor(country)))+
  geom_tile(aes(y = deaths/2,
                height = deaths,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1, size = 5) + 
  geom_text(aes(y=deaths,label = lab.deaths, hjust=0),size = 5 ) +  
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=13, hjust=1, color="black"),
        plot.margin = margin(1,3, 1, 3, "cm")) +
  transition_states(date, transition_length = 3, state_length = 1) +
  ease_aes('sine-in-out')+
  labs(title = '{closest_state}',  
       subtitle  =  "",
       caption  = "Unidad de M\u00e9todos y Acceso a Datos (UMAD)")

animate(anima_deaths, nframes = 1000, duration=20,  width = 670, height = 600, end_pause = 80, renderer = gifski_renderer()) 

gganimate::anim_save(here::here('animations','bar_race2.gif'))


