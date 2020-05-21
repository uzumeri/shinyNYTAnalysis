
counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  arrange(state, county, date) %>% #reorder to make reading the data file more intuitive
  mutate(log10cases = log10(cases), cs = paste(county, " ", state), fips = NULL) # create 2 new variables ...

countygov <- read.csv("countiesSAH.csv", stringsAsFactors = FALSE) %>% 
  filter(county != "" & county != "State") %>% mutate(cs = paste(county, " ", state)) %>% 
  rename(chard = hard) %>% select(cs, chard) %>% 
  mutate(chard = as.Date(chard, format="%m/%d/%Y"))
stategov <- read.csv("countiesSAH.csv", stringsAsFactors = FALSE) %>% 
  filter(county == "" | county == "State") %>% 
  select(state, hard) %>% 
  mutate(hard = as.Date(hard, format="%m/%d/%Y"))

stats <- counties %>% group_by(cs) %>% 
  summarize(casemax = max(cases), maxdeaths = max(deaths))
