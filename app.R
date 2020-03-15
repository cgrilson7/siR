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

simulate_day <- function(s, i, r, beta, gamma){
  
  next_day <- list(
    s = s + ds(s, i, beta),
    i = i + di(s, i, beta, gamma),
    r = r + dr(i, gamma)
    )
  
  lapply(next_day, function(x){ifelse(x < 0, 0, ifelse(x > 1, 1, x))})
  
}

ui <- fluidPage(
  
  tags$head(
    tags$style(type="text/css", "
    label{ text-align: center; vertical-align: middle; }
    .form-control { display: table-row; text-align: center;}
    img {max-width: 100%;max-height: 100%};
    ")
  ),
  
  titlePanel('A Compartmental Model of Infectious Disease'),
  
  sidebarLayout(
    sidebarPanel(align = 'center',
      div(align = 'center', 
        numericInput(inputId = 'N', HTML("Total Population (<em> N </em>)"), value = 331000000, min = 1000, max = NA, width = '50%'),
        numericInput(inputId = 'I_0', "Initial Number of Cases",  value = 1629, min = 0, max = NA, width = '50%'),
        dateInput(inputId = 'start_date', "Date of Above Caseload", value = Sys.Date(), width = '70%')
      ),
      sliderInput(inputId = 'c', label = HTML("Avg. Infected Person's Contacts per Day (<em> c </em>)"), min = 0, max = 20, value = 10),
      sliderInput(inputId = 'p', label = HTML("Probability of Infecting a Contact (<em> p </em>)"), min = 0.01, max = 1, value = 0.07),
      sliderInput(inputId = 'd', HTML("Infectious Period (Days, <em> d </em>)"), min = 1, max = 14, value = 7),
      sliderInput(inputId = 'm', HTML("Mortality Rate (<em> m </em>)"), min = 0, max = 1, value = 0.02),
      sliderInput(inputId = 'sim_duration', "Days to Simulate", min = 1, max = 720,  value = 180),
      actionButton(inputId = 'simulate', label = "Run Simulation")
    ),
    mainPanel(
      plotlyOutput('progression'),
      uiOutput('markdown_explanation')
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
      input$m
      input$sim_duration
    },
    { # begin simulation:
    t <- 0
    N <- input$N
    i <- input$I_0/N
    s <- 1 - i
    r <- 0
    
    results <- list(t, s, i, r) %>% set_names("t", "s", "i", "r")
    for(t in 1:input$sim_duration){
      # cat(paste0("t: ", t, "\ts: ", s, "\ni: ", i, "\nr: ", r, "\n"))
      new <- simulate_day(s, i, r, beta(), gamma())
      new$t <- t
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
      mutate(Recovered = (1-input$m)*Removed,
             Dead = input$m*Removed) %>% 
      pivot_longer(cols = c(Susceptible, Infected, Recovered, Dead), names_to = "Status") %>% 
      mutate(Status = factor(Status, levels = c("Susceptible", "Infected", "Recovered", "Dead")))
    
    ggplotly({
      ggplot(df, aes(x = date, y = value)) + 
      geom_point(aes(color = Status), size = 2, alpha = 0.5) + 
      geom_line(aes(color = Status)) + 
      scale_color_manual(values = c('gold', 'darkred', 'lightgreen', 'gray')) + 
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) + 
      scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +
      ylab("Population") + 
      xlab("Date") +
      theme(axis.text.y = element_text(angle = 45)) 
    })
  })
  
  output$markdown_explanation <- renderUI({
    includeHTML('www/markdown_explanation.html')
  })
  
}
  
shinyApp(ui, server)