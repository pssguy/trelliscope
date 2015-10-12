

library(datadr)
library(trelliscope)

# some packages for manipulation and display
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)

# and the data
library(Lahman)

conn <- vdbConn("vdbPremier", name = "soccer")

data <- read_csv("data/ageRank.csv")



byPlayer<- divide(data,
                  by = c("PLAYERID","name","team"))


# panel_ggplot <- function(x)
#   x %>% 
#   mutate(IP=IPouts/3) %>% 
#   ggplot(aes(x=yearID,y=WHIP,color=teamID,size=IP))+
#   geom_point() +
#   ylab("WHIP") +
#   xlab("") +
#   theme_bw()


panel_ggplot <- function(x)
  x %>% 
   ggplot(aes(x=gameDate,y=ageOrder))+
  geom_point(size=2,colour="green") +
  ylab("Age Rank of Starters") +
  xlab("") +
  theme_bw()
  

## test
panel_ggplot(byPlayer[[1]]$value)
  


makeDisplay(byPlayer,
            panelFn = panel_ggplot,
           # cogFn   = whipCog,
            name    = "Players Age Rank over Time",
            desc    = "Starters age rank in team by game")

view()