library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)
library(plotly)

ds <- function(s, i, beta){
  (-1 * beta * s * i)
}

di <- function(s, i, beta, gamma){
  (beta * s * i)  - (gamma * i)
}

dr <- function(i, gamma){
  gamma * i
}

simulate_day <- function(t, s, i, r, beta, gamma){
  
  next_day <- list(
    s = s + ds(s, i, beta),
    i = i + di(s, i, beta, gamma),
    r = r + dr(i, gamma)
    )
  
  next_day <- lapply(next_day[c('s', 'i', 'r')], function(x){ifelse(x < 0, 0, ifelse(x > 1, 1, x))})
  
  next_day$t <- t
  
  return(next_day)
}

ui <- fluidPage(
  
  tags$head(
    tags$style(type="text/css", "label{ text-align: center; vertical-align: middle; } .form-control { display: table-row; text-align: center;}")
  ),
  
  titlePanel('An Compartmental Model of Infectious Disease'),
  
  sidebarLayout(
    sidebarPanel(align = 'center',
      div(align = 'center', 
        numericInput(inputId = 'N', "Total Population", value = 331000000, min = 1000, max = NA, width = '50%'),
        numericInput(inputId = 'I_0', "Initial Number of Cases",  value = 1629, min = 0, max = NA, width = '50%'),
        dateInput(inputId = 'start_date', "Date of Above Caseload", value = Sys.Date(), width = '70%')
      ),
      sliderInput(inputId = 'c', label = "Avg. Infected Person's Contacts per Day", min = 0, max = 20, value = 10),
      sliderInput(inputId = 'p', label = "Probability of Infecting a Contact", min = 0.01, max = 1, value = 0.07),
      sliderInput(inputId = 'd', "Infectious Period (Days)", min = 1, max = 14, value = 7),
      sliderInput(inputId = 'sim_duration', "Days to Simulate", min = 1, max = 360,  value = 90),
      actionButton(inputId = 'simulate', label = "Run Simulation")
    ),
    mainPanel(
      plotlyOutput('progression')
    )
  )
)
  
server <- function(input, output, session){
  
  gamma <- reactive({1/input$d})
  
  beta <- reactive({input$c * input$p})
  
  sim <- eventReactive(
    { # any of these events will trigger the simulation:
      input$simulate
      input$I_0
      input$c
      input$p
      input$d
      input$sim_duration
    },
    { # begin simulation:
    N <- input$N
    i <- input$I_0/N
    s <- 1 - i
    r <- 0
    t <- 0
    
    results <- list(t, s, i, r) %>% set_names("t", "s", "i", "r")
    for(t in 1:input$sim_duration){
      cat(paste0("t: ", t, "\ts: ", s, "\ni: ", i, "\nr: ", r, "\n"))
      new <- simulate_day(t, s, i, r, beta(), gamma()) 
      results <- bind_rows(results, new)
      s <- new$s
      i <- new$i
      r <- new$r
    }
    
    results
    
  })
  
  output$progression <- renderPlotly({
    df <- sim() %>% 
      as_tibble() %>% 
      mutate(date = input$start_date + days(t)) %>% 
      mutate(Susceptible = s*input$N,
             Infected = i*input$N,
             Removed = r*input$N) %>% 
      pivot_longer(Susceptible:Removed, names_to = "Status") %>% 
      mutate(Status = factor(Status, levels = c("Susceptible", "Infected", "Removed")))
    
    ggplotly({
      ggplot(df, aes(x = date, y = value)) + 
      geom_point(aes(color = Status), size = 2, alpha = 0.5) + 
      geom_line(aes(color = Status)) + 
      scale_color_manual(values = c('darkgreen', 'darkred', 'gray')) + 
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) + 
      scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +
      ylab("Population") + 
      xlab("Date") +
      theme(axis.text.y = element_text(angle = 45))
    })
  })
  
}
  
shinyApp(ui, server)