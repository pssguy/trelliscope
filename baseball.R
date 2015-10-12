
## standard libraries

library(dplyr)
library(ggplot2)
library(stringr)


library(datadr)
library(trelliscope)


#library(shinyapps)

## Lahman data to 2014
library(Lahman)


conn <- vdbConn("vdbLahman", name = "baseball")


pitching <- tbl_df(Pitching %>% 
                     filter(yearID >= 1901 & lgID %in% c("AL", "NL")) %>% 
                     select(-c(SH,SF,GIDP)))


pitching <- pitching %>% 
  left_join(Master) %>% 
  mutate(
         WHIP = round((H + BB) * 3/IPouts, 2),
         name=paste(nameFirst,nameLast, sep=" "),
         bbrefURL=paste0("http://www.baseball-reference.com/players/",str_sub(bbrefID,1,1),"/",bbrefID,".shtml")) %>% 
  select(playerID,teamID,name,WHIP,yearID,IPouts,bbrefURL)
## lets just look at pitcher (no split by team played for)

byPlayer<- divide(pitching,
                  by = c("playerID","name"))

panel_ggplot <- function(x)
  x %>% 
  mutate(IP=IPouts/3) %>% 
  ggplot(aes(x=yearID,y=WHIP,color=teamID,size=IP))+
  geom_point() +
  ylab("WHIP") +
  xlab("") +
  theme_bw()


## test
panel_ggplot(byPlayer[[1]]$value)

#cognostics function to be applied to each plot

  whipCog <- function(x) {
    list(
      totInns=sum(x$IPouts)/3,
      minYear=min(x$yearID),
      maxYear=as.integer(max(x$yearID)),
      bbhRef = cogHref(unique(x$bbrefURL),desc="Baseball Reference link")
    )
  }

  makeDisplay(byPlayer,
              panelFn = panel_ggplot,
              cogFn   = whipCog,
              name    = "WHIP by Team by Year",
              desc    = "Pitcher Average Walks and Hits per Innings")
  
  view()
  
  deployToShinyApps(vdbConn = getOption("vdbConn"), appName = "trelliscope_baseball",
                    account = "mytinyshinys", redeploy = TRUE, size = NULL, instances = NULL,
                    quiet = FALSE)