library(shiny)
library(tidyverse)
#library(broom)
library(plotly)
library(sfsmisc)


  counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
    arrange(state, county, date) %>% #reorder to make reading the data file more intuitive
    mutate(cs = paste(county, " ", state), fips = NULL) # create 2 new variables ...
  
  # write.csv(counties, "counties.csv")
  
  countygov <- read.csv("countiesSAH.csv", stringsAsFactors = FALSE) %>% 
    filter(county != "State") %>% mutate(cs = paste(county, " ", state)) %>% 
    rename(chard = hard) %>% select(cs, chard) %>% 
    mutate(chard = as.Date(chard, format="%m/%d/%Y"))
  
  countystats <- counties %>% group_by(cs) %>% 
    summarize(casemax = max(cases), deathmax = max(deaths), n = n(), firstday = min(date))

  lastday <- max(counties$date) # find the last day of the dataset
  
  states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
    arrange(state, date) %>% #reorder to make reading the data file more intuitive
    mutate(fips=NULL) # create a new variable ...
  
  stategov <- read.csv("countiesSAH.csv", stringsAsFactors = FALSE) %>% 
    filter(county == "State") %>% 
    select(state, hard) %>% 
    mutate(hard = as.Date(hard, format="%m/%d/%Y")) %>% 
    replace_na(list(hard=Sys.Date()))

  cleancounty <- counties %>%
    left_join(countystats, by="cs") %>%
    left_join(countygov, by="cs") %>%
    left_join(stategov, by="state") %>% 
    filter(cases > 10) %>% 
    filter(casemax > 500) %>% 
    filter(county != "Unknown") %>% 
    filter(n > 5) %>% 
    mutate(ndate = as.numeric(date))
  
    cslist <- cleancounty %>% distinct(cs)
  
    for (i in 1:nrow(cslist)) {
      
        dcs <- cleancounty %>% filter(cs == cslist$cs[i])
        subset <- dcs %>% select (ndate, cases, date) 
        
        d <- D2ss(subset$ndate,subset$cases) 
        dy <- map_dfr(d, ~as.data.frame(d$y), .id="kx") %>% filter(kx == "x") 
        dx <- map_dfr(d, ~as.data.frame(.x), .id="ky") %>% filter(ky == "x")
        der <- cbind(dx, dy) 
        colnames(der) <- c("keyy","ndate","keyx","dcases")
        deriv2 <- der %>% select(ndate, dcases) %>% add_column(cs = as.character(cslist$cs[i]))

        if (i < 2) {
          deriv <- deriv2
        } else {
          deriv <- bind_rows(deriv, deriv2)
        }
        p <- ggplot(data=deriv2, aes(x=ndate, y=dcases)) + geom_point() + geom_smooth(method="loess") + labs(title=as.character(cslist$cs[i]))
        print(p)
    }
    
    cleancounty <- cleancounty %>% left_join(deriv, by = c("ndate" = "ndate","cs" = "cs")) 


