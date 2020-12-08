library(tidyverse)
library(plotly)
library(ggforce)
library(gridExtra)
library(jsonlite)

setwd("C:/GitHub/shinyNYTAnalysis")

  # cvt <- fromJSON("https://covidtracking.com/api/v1/states/daily.json")
  # cvt$date <- as.Date(as.character(cvt$date), format="%Y%m%d")
  # 
  # counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  #   arrange(state, county, date) %>% #reorder to make reading the data file more intuitive
  #   mutate(cs = paste(county, " ", state), fips = NULL, newcase = (cases - lag(cases)), newdeath = (deaths - lag(deaths))) # create new variables ...
  # 
  # countystats <- counties %>% group_by(cs) %>% 
  #   summarize(casemax = max(cases), deathmax = max(deaths), firstday = min(date))

  lastday <- max(counties$date) # find the last day of the dataset
  
  cleancounty <- counties %>%
    left_join(countystats, by="cs") %>%
    filter(cases > 10) %>% 
    filter(casemax > 8000) %>% 
    filter(county != "Unknown") %>% 
    mutate(daysin = as.numeric(date - firstday)) %>% 
    arrange(state, county) %>% 
    filter(state == 'Georgia')
  
    cslist <- cleancounty %>% distinct(cs)

    pdf(file="multiplotnc.pdf")
    for (i in seq(from=1, to=nrow(cslist), by=6)) {
      
      cssub <- cslist %>% slice(i:(i+5))

      sub <- cleancounty %>% inner_join(cssub, by="cs")
      
      cp <-  ggplot(sub, aes(x=date, y=newcase )) + geom_point() + geom_smooth() + 
        ylim(0,600) + ylab("New Cases Per Day") + xlab("Date") +
        facet_wrap(~cs, ncol=2, nrow=3)
      plot(cp)
      
    }
    dev.off()
    
