library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)
library(plotly)
library(deSolve)

# ds <- function(s, i, beta){
#   (-1 * beta * s * i)
# }
# 
# di <- function(s, i, beta, gamma){
#   (beta * s * i)  - (gamma * i)
# }
# 
# dr <- function(i, gamma){
#   gamma * i
# }

equations <- function(t, state = c(s = 0.99, i = 0.01, r = 0), parameters = c(beta = 0.69, gamma = 0.69)){
  with(as.list(c(state, parameters)), {
    
    ds <- -1 * beta * s * i
    di <- beta*s*i - gamma*i
    dr <- gamma*i
    
    list(c(ds, di, dr))
  })
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
    sidebarPanel(align = 'center', width = 3,
      div(align = 'center', 
        numericInput(inputId = 'N', HTML("Total Population, <em>N</em>"), value = 331000000, min = 1000, max = NA, width = '70%'),
        numericInput(inputId = 'I_0', HTML("Total Cases Today, <em>I<sub>0</sub></em>"),  value = 4138, min = 0, max = NA, width = '70%'),
        # dateInput(inputId = 'start_date', "Date of Above Caseload", value = Sys.Date(), width = '70%')
      ),
      sliderInput(inputId = 'c', label = HTML("Avg. Contacts per Day, <em>c</em>"), min = 0, max = 20, value = 10),
      sliderInput(inputId = 'p', label = HTML("Probability of Infecting a Contact, <em>p</em>"), min = 0.01, max = 1, value = 0.07),
      sliderInput(inputId = 'd', HTML("Infectious Period, <em>d</em> (days)"), min = 1, max = 14, value = 7),
      sliderInput(inputId = 'm', HTML("Mortality Rate, <em>m</em>"), min = 0, max = 1, value = 0.02),
      sliderInput(inputId = 'sim_duration', "Days to Simulate", min = 1, max = 720,  value = 180),
      actionButton(inputId = 'simulate', label = "Run Simulation")
    ),
    mainPanel(width = 9,
      tabsetPanel(id = 'Tabs',
        tabPanel('Simulation',
          plotlyOutput('progression')
        ),
        tabPanel('Methods',
          uiOutput('markdown_explanation')
        )
      )
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
      
    N <- input$N
    i_0 <- input$I_0/N
    s_0 <- 1 - i_0
    r_0 <- 0
    beta <- input$c * input$p
    gamma <- 1/input$d
    
    initial_state <- c(s = s_0, i = i_0, r = r_0)
    
    times <- 0:input$sim_duration
    
    parameters <- c(beta = beta, gamma = gamma)
    
    deSolve::ode(y = initial_state, times = times, func = equations, parms = parameters) %>%
      as_tibble() %>%
      mutate(time = as.integer(time)) %>% 
      mutate_at(vars(s, i, r), as.numeric) %>% 
      mutate(date = Sys.Date() + days(times)) %>% 
      select(date, s, i, r)

  })
  
  observeEvent(input$simulate, {
    updateTabsetPanel(session = session, inputId = 'Tabs', selected = 'Simulation')
  })
  
  output$progression <- renderPlotly({
    
    df <- sim() %>% 
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