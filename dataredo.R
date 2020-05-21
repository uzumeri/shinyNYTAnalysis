library(shiny)
library(tidyverse)
library(broom)

options(scipen = 999)

counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  arrange(state, county, date) %>% #reorder to make reading the data file more intuitive
  mutate(log10cases = log10(cases), cs = paste(county, " ", state), fips = NULL) # create 2 new variables ...

countygov <- read.csv("countiesSAH.csv", stringsAsFactors = FALSE) %>% 
  filter(county != "State") %>% mutate(cs = paste(county, " ", state)) %>% 
  rename(chard = hard) %>% select(cs, chard) %>% 
  mutate(chard = as.Date(chard, format="%m/%d/%Y"))

countystats <- counties %>% group_by(cs) %>%
  summarize(casemax = max(cases), deathmax = max(deaths))

lastday <- max(counties$date) # find the last day of the dataset

states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
  arrange(state, date) %>% #reorder to make reading the data file more intuitive
  mutate(log10cases = log10(cases), fips=NULL) # create a new variable ...

stategov <- read.csv("countiesSAH.csv", stringsAsFactors = FALSE) %>% 
  filter(county == "State") %>% 
  select(state, hard) %>% 
  mutate(hard = as.Date(hard, format="%m/%d/%Y")) %>% 
  replace_na(list(hard=Sys.Date()))

statestats <- states %>% group_by(state) %>% 
  summarize(casemax = max(cases), deathmax = max(deaths))

cutoff <- 500         # input value

cleancounty <- counties %>%
  filter(cases > 10) %>% 
  left_join(countystats, by="cs") %>%
  left_join(countygov, by="cs") %>%
  left_join(stategov, by="state") %>% 
  filter(casemax >= cutoff) 
  
cleanstate <- states %>%
  left_join(statestats, by="state") %>%
  left_join(stategov, by="state")

delay <- 2 # input value

fit <- cleancounty %>% 
  filter(date <= hard + delay | date <= chard + delay) %>% 
  group_by(cs) %>% 
  do(lm = lm(log10(cases) ~ date, data =.)) %>% 
  mutate(lm_b0 = summary(lm)$coeff[1], lm_b1 = summary(lm)$coeff[2]) %>% 
  mutate( dt = log10(2)/lm_b1) %>% 
  select(cs, lm_b0, lm_b1, dt)  

firstday <- as.Date("01/03/2020", format="%d/%m/%Y") 

instate <- "Georgia"  # input value

subset <- cleancounty %>% left_join(fit, by="cs") %>% filter(state==instate)

ytop <- max(subset$casemax)

hardpoly <- data.frame(x = c(subset$hard[1],subset$hard[1],lastday,lastday), y = c(0,ytop,ytop,0))

p <- ggplot(subset) +
  scale_y_log10("Cases to Date") +
  geom_polygon(data=hardpoly, mapping=aes(x=x, y=y), fill="red", alpha=0.1) +
  geom_point(aes(x = date, y = cases, shape = county, stroke=1.5)) +
  geom_abline(mapping=aes(intercept = lm_b0, slope = lm_b1, color='Gray'), show.legend = FALSE) +
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) +
  xlim(firstday,lastday) +
  theme(legend.position = "bottom") +
  labs(x="Date", title="Cases vs Time with Fitted Regression Line") 

p
