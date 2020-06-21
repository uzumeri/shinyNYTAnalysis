library(shiny)
library(tidyverse)
library(broom)
library(plotly)
library(sfsmisc)
library(pspline)

# Define UI for random distribution app ----
ui <- fillPage(
  
  # App title ----
  titlePanel("COVID-19 Status Models"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      title = "How to play",
      strong("COVID-19 Analysis"),
      p("Statistical Analysis of Cases by State and County ... Showcasing Counties with over 1000 cases,"),
      p("Date of Most Recent Data: "), 
      strong(uiOutput("lday")),
      br(),

      # Input: Slider for the state that is of interest 
      uiOutput("stateselector"),
      
      # Input: Slider for the state that is of interest 
      uiOutput("countyselector"),

      strong("Credit:"),
      p("The data for this site was sourced from the New York Times, who are kindly sharing their daily data collection: "),
      a(href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html", " COVID-19 case data"),
      br(),
      p("Vic Uzumeri, PhD - April 2020")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Impacted Counties", 
                           plotOutput("countyraw"), style = "height:600px"),
                  tabPanel("Impacted Counties (Log)", 
                           sliderInput("delayl", "Delay in Lockdown Effect", min = 0, max = 7, value = 5),
                           plotOutput("county"), style = "height:600px"),
                  tabPanel("New Cases Per Day", plotlyOutput("d1st", height = "100%")),
                  tabPanel("Latest Change", dataTableOutput("cdelta")),
                  tabPanel("Case Acceleration", plotlyOutput("deriv", height = "100%")),
                  tabPanel("Deaths", plotlyOutput("deaths", height = "100%")),
                  tabPanel("Case Fatality Rate", plotlyOutput("deathrate", height = "100%"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {
  
  # General code to set up variables
  
  options(scipen = 999)
  
  counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
    arrange(state, county, date) %>% #reorder to make reading the data file more intuitive
    mutate(cs = paste(county, " ", state), fips = NULL) # create 2 new variables ...
  
   write.csv(counties, "counties.csv")
  
  countygov <- read.csv("countiesSAH.csv", stringsAsFactors = FALSE) %>% 
    filter(county != "State") %>% mutate(cs = paste(county, " ", state)) %>% 
    rename(chard = hard) %>% select(cs, chard) %>% 
    mutate(chard = as.Date(chard, format="%m/%d/%Y"))
  
  countystats <- counties %>% group_by(cs) %>% 
    summarize(casemax = max(cases), deathmax = max(deaths), n = n(), firstday = min(date))
  
  # write.csv(countystats, "countystats.csv")
  
  lastday <- max(counties$date) # find the last day of the dataset
  
  states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
    arrange(state, date) %>% #reorder to make reading the data file more intuitive
    mutate(fips=NULL) # create a new variable ...
  
  stategov <- read.csv("countiesSAH.csv", stringsAsFactors = FALSE) %>% 
    filter(county == "State") %>% 
    select(state, hard) %>% 
    mutate(hard = as.Date(hard, format="%m/%d/%Y")) %>% 
    replace_na(list(hard=Sys.Date()))
  
  statestats <- states %>% group_by(state) %>% 
    summarize(casemax = max(cases), deathmax = max(deaths))

  cleancounty <- counties %>%
    left_join(countystats, by="cs") %>%
    left_join(countygov, by="cs") %>%
    left_join(stategov, by="state") %>% 
    filter(cases > 10) %>% 
    filter(casemax > 1000) %>% 
    filter(county != "Unknown") %>% 
    filter(n > 5) %>% 
    mutate(ndate = as.numeric(date)) 
    
  cleancounty <- cleancounty %>% group_by(cs) %>% mutate(CasePctIncrease = ((cases - lag(cases))/cases)*100)
  
  cslist <- cleancounty %>% distinct(cs)
  
  for (i in 1:nrow(cslist)) {
    
    dcs <- cleancounty %>% filter(cs == cslist$cs[i])
    subset <- dcs %>% select (ndate, cases, date) 

    d2 <- D2ss(subset$ndate,subset$cases, spar.offset = 0) 
    d2y <- map_dfr(d2, ~as.data.frame(d2$y), .id="kx") %>% filter(kx == "x") 
    d2x <- map_dfr(d2, ~as.data.frame(.x), .id="ky") %>% filter(ky == "x")
    der2 <- cbind(d2x, d2y) 
    colnames(der2) <- c("keyy","ndate","keyx","dcases")
    deriv2 <- der2 %>% select(ndate, dcases) %>% add_column(cs = as.character(cslist$cs[i]))
    
    if (i < 2) {
      derv2 <- deriv2
    } else {
      derv2 <- bind_rows(derv2, deriv2)
    }
  }
  
  cleancounty <- cleancounty %>% left_join(derv2, by = c("ndate" = "ndate","cs" = "cs")) 
  
  write.csv(cleancounty,"cleancounty.csv")

  #----------------------------------
  # Generate the last day of the dataset
  output$lday <- renderText({
    format(lastday, format="%b %d")
    # browser()
  })
  
  # Generate a vector with a list of counties for the chosen state
  output$countyselector <- renderUI({
    counties <- cleancounty %>% filter(state == input$state) %>% distinct(county) 
    selectInput("county", "County", as.list(counties$county))
  })
  
  # Generate a vector with a list of states that are relevant to analysis
  output$stateselector <- renderUI({
    states <- cleancounty %>% distinct(state) 
    selectInput("state", "State", as.list(states$state))
  })
  
  output$countyraw <- renderPlot({
    
    subset <- cleancounty %>% group_by(cs) %>% filter(state == input$state & county == input$county) 
    # write.csv(subset, "subset.csv")
    firstday <- as.Date("15/03/2020", format="%d/%m/%Y") 
    
    ytop <- max(subset$casemax)
    
    hardpoly <- data.frame(x = c(subset$hard[1],subset$hard[1],lastday,lastday), y = c(0,ytop,ytop,0))
    
    
    ggplot(subset) + 
      geom_polygon(data=hardpoly, mapping=aes(x=x, y=y), fill="red", alpha=0.1) +
      geom_point(aes(x = date, y = cases)) +
      xlim(firstday,lastday) +
      theme(legend.position = "bottom") +
      labs(x="Date", title="Cases vs Time")
    
  })

    # Generate a log plot of the data by state and county ----
    output$county <- renderPlot({

      subset <- cleancounty %>% group_by(cs) %>% filter(state == input$state & county == input$county) %>% mutate(udate = hard + input$delayl)
  
      firstday <- as.Date("15/03/2020", format="%d/%m/%Y") 
      
      ytop <- max(subset$casemax)
  
      hardpoly <- data.frame(x = c(subset$hard[1],subset$hard[1],lastday,lastday), y = c(0,ytop,ytop,0))

      ggplot(subset) + 
      scale_y_log10("Cases to Date - Log Scale") +
      geom_polygon(data=hardpoly, mapping=aes(x=x, y=y), fill="red", alpha=0.1) +
      geom_point(aes(x = date, y = cases)) +
      geom_smooth(data = subset(subset, date <= udate), aes(x = date, y = cases), method = 'lm', formula = y ~ x, color='Gray', show.legend = FALSE, se = FALSE, fullrange = TRUE) +
      xlim(firstday,lastday) + 
      theme(legend.position = "bottom") +
      labs(x="Date", title="Log10(Cases) vs Time with Fitted Regression Line")
    
  })
  
  # Generate a log plot of the data by state and county ----
  output$deriv <- renderPlotly({
    
    subset <- cleancounty %>% filter(state == input$state)
    ytop <- max(subset$dcases)
    firstday <- as.Date("15/03/2020", format="%d/%m/%Y") 
    
    accel <- data.frame(x = c(firstday, firstday, lastday, lastday),y = c(0,ytop,ytop,0) )
    decel <- data.frame(x = c(firstday, firstday, lastday, lastday),y = c(0,-ytop,-ytop,0) )
    
  ggplotly(
    ggplot(data=subset, aes(x=date, y=dcases)) + 
      geom_point() + 
      geom_smooth(method="loess", se = FALSE) + 
      scale_y_continuous("Acceleration of Cases") +
      geom_polygon(data=accel, mapping=aes(x=x, y=y), fill="Red", alpha=0.1) +
      geom_polygon(data=decel, mapping=aes(x=x, y=y), fill="Green", alpha=0.1) +
      labs(x="Date", title="Acceleration of Case Change") +
      facet_wrap(vars(county))
  )

  })
  
  output$cdelta <- renderDataTable({
    
    subset <- cleancounty %>% filter(date == lastday) %>% select(cases, deaths, CasePctIncrease) %>% arrange(desc(CasePctIncrease))
    subset

  })

    output$deaths <- renderPlotly({
    
    subset <- cleancounty %>% filter(state == input$state & county == input$county)
    
    ggplotly(
      ggplot(data=subset, aes(x=date, y=deaths)) + 
      geom_point() + 
      geom_smooth(method="loess", se = FALSE) +
      labs(x="Date", title="Cumulative Deaths") +
      scale_y_continuous("Cumulative Deaths") 
    )
    
  })

  output$d1st <- renderPlotly({
    
    subset <- cleancounty %>% filter(state == input$state & county == input$county)
    subset$d1 <- predict(sm.spline(subset$ndate, subset$cases), subset$ndate, 1)
    
    write.csv(subset, "subsetd1.csv")
    
    ggplotly(
      ggplot(data=subset, aes(x=date, y=d1)) + 
      geom_point() + 
      geom_smooth(method="loess", span = 0.5, se = FALSE) +
      labs(x="Date", title="New Cases Per Day") +
      scale_y_continuous("New Cases Per Day") 
    )
    
  })

  output$deathrate <- renderPlotly({
    
    subset <- cleancounty %>% filter(state == input$state & county == input$county)
    
    ggplotly(
      ggplot(data=subset, aes(x=date, y=deaths/cases)) + 
      geom_point() + 
      geom_smooth(method="loess", se = FALSE) +
      labs(x="Date", title="Mortality Rate in Confirmed Cases") +
      scale_y_continuous("Fraction Fatal Cases") +
      ylim(0,.10)
    )
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)

