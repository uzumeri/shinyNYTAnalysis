library(shiny)
library(tidyverse)
library(broom)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("COVID-19 Status Models"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      title = "How to play",
      strong("Introduction"),
      p("Explanation about the data and the analysis"),
      p("Date of Most Recent Data: ",strong(uiOutput("lday"))),

      # Input: Selector for county case number cutoff 
      selectInput("countycutoff", "County Case Cutoff", c(300,500,1000,1500)),

      # Input: Slider for the state that is of interest 
      uiOutput("stateselector"),
      
      strong("Followup"),
      p("Random text about stuff")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("High Case Volume Counties", plotOutput("county")),
                  tabPanel("States", plotOutput("state"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {
  
  # General code to set up variables
  
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
    summarize(casemax = max(cases), maxdeaths = max(deaths))
  
  cleancounty <- counties %>%
    filter(cases > 5) %>%
    left_join(countystats, by="cs") %>%
    left_join(countygov, by="cs") %>%
    left_join(stategov, by="state")
  
  cleanstate <- states %>%
    left_join(statestats, by="state") %>%
    left_join(stategov, by="state")  # Generate printable string showing last day of data ----

    output$lday <- renderText({
    format(lastday, format="%b %d")
  })
  
  # Generate a vector with a list of states that are relevant to analysis
  
  output$stateselector <- renderUI({
    selectInput("state", "State", as.list(cleanstates$state))
  })
  
  # Generate a plot of the data ----

  output$county <- renderPlot({

    fit <- cleancounty %>% 
      filter(date <= hard +5 | date <= chard +5 | is.na(hard)) %>% 
      group_by(cs) %>% 
      do(lm = lm(log10(cases) ~ date, data =.)) %>% 
      mutate(lm_b0 = summary(lm)$coeff[1], lm_b1 = summary(lm)$coeff[2]) %>% 
      mutate( dt = log10(2)/lm_b1) %>% 
      select(cs, lm_b0, lm_b1, dt)  

    firstday <- min(cleancounty$date)

    subset <- cleancounty %>% left_join(fit, by="cs") %>% filter(state==input$state)
    
    hardpoly <- data.frame(x = c(subset$hard[1],subset$hard[1],lastday,lastday), y = c(0,5,5,0))

      ggplot(subset, aes(x = date, y= log10(cases))) +
        scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) +
        scale_colour_manual(values=c('#999999','#E69F00','#56B4E9','#999999','#E69F00','#56B4E9','#999999','#E69F00', '#56B4E9','#999999','#E69F00', '#56B4E9','#999999','#E69F00'), name=NULL, labels = NULL) +
        geom_polygon(data=hardpoly, mapping=aes(x=x, y=y), fill="red", alpha=0.1) +
        geom_point(aes(shape = county, stroke=1.5)) +
        geom_abline(mapping=aes(intercept = lm_b0, slope = lm_b1, color='Gray'), show.legend = FALSE) +
        xlim(firstday,lastday) +
        theme(legend.position = "bottom") +
        labs(x="Date", y="Log10 of Cases to Date", title="Cases vs Time with Fitted Regression Line") 

  })

  output$state <- renderPlot({

    subset <- cleanstate %>% filter(state==input$state)
    hardpoly <- data.frame(x = c(subset$hard[1],subset$hard[1],lastday,lastday), y = c(0,5,5,0))

    ggplot(subset, aes(x = date, y= log10(cases))) +
      scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) +
      scale_colour_manual(values=c('#999999','#E69F00','#56B4E9','#999999','#E69F00','#56B4E9','#999999','#E69F00', '#56B4E9','#999999','#E69F00', '#56B4E9','#999999','#E69F00'), name=NULL, labels = NULL) +
      geom_polygon(data=hardpoly, mapping=aes(x=x, y=y), fill="red", alpha=0.1) +
      geom_point() +
      xlim(firstday,lastday) +
      theme(legend.position = "bottom") +
      labs(x="Date", y="Log10 of Cases to Date", title="Cases vs Time with Fitted Regression Line") 

  })
  
  # # Generate a summary of the data ----
  # output$summary <- renderPrint({
  #   summary(d())
  # })
  # 
  # # Generate an HTML table view of the data ----
  # output$table <- renderTable({
  #   d()
  # })
  
}

# Create Shiny app ----
shinyApp(ui, server)

