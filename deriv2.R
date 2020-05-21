library(tidyverse)
library(sfsmisc)

counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  arrange(state, county, date) %>% #reorder to make reading the data file more intuitive
  mutate(log10cases = log10(cases), cs = paste(county, " ", state), fips = NULL) # create 2 new variables ...

subset <- counties %>% filter(state == "California", cases >= 2) %>% 
  arrange(county, date) %>% 
  group_by(county) %>% 
  select (county, date, cases)  

subset$ndate <- as.numeric(subset$date)

deriv2 <- as.data.frame(D2ss(as.numeric(subset$ndate),subset$cases),spar.offset=0)
deriv2$x2 <- as.Date(deriv2$x,origin = "1970-01-01") 

p <- ggplot(deriv2, aes(x2, y)) + geom_point() +geom_smooth(method="loess")
p
