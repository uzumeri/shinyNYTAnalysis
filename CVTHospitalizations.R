library(tidyverse)
library(plotly)
library(ggforce)
library(gridExtra)
library(jsonlite)

setwd("C:/GitHub/shinyNYTAnalysis")

  cvt <- fromJSON("https://covidtracking.com/api/v1/states/daily.json")
  cvt$date <- as.Date(as.character(cvt$date), format="%Y%m%d")

  slist <- cvt %>% distinct(state)

    pdf(file="hospitalizations.pdf")
    for (i in seq(from=1, to=nrow(slist), by=6)) {
      
      ssub <- slist %>% slice(i:(i+5))

      sub <- cvt %>% inner_join(ssub, by="state")
      
      cp <-  ggplot(sub, aes(x=date, y=totalTestResultsIncrease )) + geom_point() + geom_smooth() + facet_wrap(~state, ncol=2, nrow=3)
      plot(cp)
      
    }
    dev.off()
    
