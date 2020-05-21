library(tidyverse)

countyaction <- read.csv("countiesSAH.csv", stringsAsFactors = FALSE) %>% 
  filter(county != "" & county != "State") %>% mutate(cs = paste(county, " ", state)) %>% rename(chard = hard) %>% select(cs, chard)
stateaction <- read.csv("countiesSAH.csv", stringsAsFactors = FALSE) %>% 
  filter(county == "" | county == "State") %>% select(state, hard)
