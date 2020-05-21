library(tidyverse)

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

cleandata <- counties %>% 
    left_join(stats, by="cs") %>% 
    filter(casemax > 300) %>% 
    filter(cases > 5) %>% 
    left_join(countygov, by="cs") %>% 
    left_join(stategov, by="state") 
  
cleancounties <- cleandata %>% count(county)
cleanstates <- cleandata %>% count(state)

firstday <- min(cleandata$date)
lastday <- max(cleandata$date)

fitdata <- cleandata %>% filter(date <= hard +5 | date <= chard +5 | is.na(hard))

fit <- fitdata %>% 
    group_by(cs) %>% 
    do(lm = lm(log10(cases) ~ date, data =.)) %>% 
    mutate(lm_b0 = summary(lm)$coeff[1], lm_b1 = summary(lm)$coeff[2]) %>% 
    mutate( dt = log10(2)/lm_b1) %>% 
    select(cs, lm_b0, lm_b1) %>% 
    ungroup()

cleandata <- cleandata %>% left_join(fit, by="cs")

pdf("plots.pdf", onefile = TRUE, paper = "USr")

for (s in stategov$state){
  subset <- cleandata %>% filter(state==s)
  
  hardpoly <- data.frame(x = c(subset$hard[1],subset$hard[1],lastday,lastday), y = c(0,5,5,0))

  if (nrow(subset)>0){
    p <- ggplot(subset, aes(x = date, y= log10(cases))) +
    scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) +
    scale_colour_manual(values=c('#999999','#E69F00','#56B4E9','#999999','#E69F00','#56B4E9','#999999','#E69F00', '#56B4E9','#999999','#E69F00', '#56B4E9','#999999','#E69F00'), name=NULL, labels = NULL) +
    geom_polygon(data=hardpoly, mapping=aes(x=x, y=y), fill="red", alpha=0.1) +
    geom_point(aes(shape = cs, color=cs,  stroke=1.5)) +
    geom_abline(mapping=aes(intercept = lm_b0, slope = lm_b1, color='Gray'), show.legend = FALSE) +
    xlim(firstday,lastday) +
    theme(legend.position = "bottom") +
    labs(x="Date", y="Log10 of Cases to Date", title="Cases vs Time with Fitted Regression Line")
plot(p)
}
}
dev.off()
