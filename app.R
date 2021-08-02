library(shiny)
library(deSolve)

#read in csv
read <- read.csv("/Users/annah/OneDrive - UCB-O365/Documents/RStudio_Plots/RaceEthnPlots/colorado-race-ethnicity-historical.csv")

# SIRD function definition made into piece wise for 4 different periods
# with different alpha value for each allowing for varying contact rates

sird_eqns <- function(time, variables, parameters) {
  ifelse(time >= 0 & time<15, with(as.list(c(variables, parameters)), {
      dS <- -(alpha1) * S * I
      dI <-  (alpha1) * S * I - (.1005) * I
      dR <-  .1 * I
      dD <-  .0005 * I
      return(list(c(dS, dI, dR, dD)))
  }), 
  ifelse(time >= 15 & time<150, with(as.list(c(variables, parameters)), {
      dS <- -(alpha2) * S * I
      dI <-  (alpha2) * S * I - (.1005) * I
      dR <-  .1 * I
      dD <-  .0005 * I
      return(list(c(dS, dI, dR, dD)))
    }),
  ifelse(time >= 150 & time<290, with(as.list(c(variables, parameters)), {
      dS <- -(alpha3) * S * I
      dI <-  (alpha3) * S * I - (.1005) * I
      dR <-  .1 * I
      dD <-  .0005 * I
      return(list(c(dS, dI, dR, dD)))
    }),
    ifelse(time >= 290 & time<327, with(as.list(c(variables, parameters)), {
      dS <- -(alpha4) * S * I
      dI <-  (alpha4) * S * I - (.1005) * I
      dR <-  .1 * I
      dD <-  .0005 * I
      return(list(c(dS, dI, dR, dD)))
    }), 0
    ))))
}

# USER INTERFACE
ui <- fluidPage(
  titlePanel("Parsimonious SIRD Covid Model"), #Title
  
  sidebarLayout( #sidebar with description and widgets(user adjustable buttons, drop-down menus and more)
    sidebarPanel( 
      helpText("SIRD populations based on Race/Ethnicity with Recovery Rate 0.1, and Death Rate 5e-9. Contact Rate adjusted for different periods"),
      selectInput("var", 
                  label = "Choose a group to display",
                  choices = c("Asian", "American Indian Alaska Native", "Black", "White", "Other", "Native Hawaiian Pacific Islander", "Multiracial", "Latinx", "Ethnically Hispanic", "Ethnically Not Hispanic"),
                  selected = "Asian"), 
      checkboxGroupInput(inputId = "pop", 
                         label = "Choose which population(s) you'd like to display",
                         choices = c("Susceptible", "Infected", "Recovered", "Deaths"),
                         selected = c("Deaths", "Susceptible", "Infected", "Recovered")),
      sliderInput(inputId = "alpha1", 
                  label ="Contact Rate: apr 15 - jul 1", 
                  value = 0.3, min = 0.05, max = 0.4, step = 0.05),
      sliderInput(inputId = "alpha2", 
                  label ="Contact Rate: jul 2 - sep 13", 
                  value = 0.1, min = 0.05, max = 0.4, step = 0.05),
      sliderInput(inputId = "alpha3", 
                  label ="Contact Rate: sep 14 - jan 31", 
                  value = 0.2, min = 0.05, max = 0.4, step = 0.05),
      sliderInput(inputId = "alpha4", 
                  label ="Contact Rate: feb 1 - march 7", 
                  value = 0.4, min = 0.05, max = 0.4, step = 0.05),
      #actionButton(inputId = "layer",label ="Layer data over ODE")
    ),
    
    mainPanel(plotOutput("my_plot"), plotOutput("my_plot2")) # plot outputs
  )
)

#SERVER
server <- function(input, output) {
  
  #327 days (apr 15, 2020 - mar 7, 2021)
  time_values <- seq(0, 327)
  data_time <- seq(0,327, by = 3.516)
  
  output$my_plot <- renderPlot({  # plots under renderPlot will update each time the app is run
    
    cases <- switch(input$var, 
                    "Asian" = read$Cases_Asian, 
                    "American Indian Alaska Native" = read$Cases_AIAN, 
                    "Black" = read$Cases_Black, 
                    "White" = read$Cases_White, 
                    "Other" = read$Cases_Other, 
                    "Native Hawaiian Pacific Islander" = read$Cases_NHPI, 
                    "Multiracial" = read$Cases_Multiracial, 
                    "Latinx" = read$Cases_LatinX, 
                    "Ethnically Not Hispanic" = read$Cases_Ethnicity_NonHispanic,
                    "Ethnically Hispanic" = read$Cases_Ethnicity_Hispanic
    )
    
    deaths <- switch(input$var, 
                     "Asian" = read$Deaths_Asian,  
                     "American Indian Alaska Native" = read$Deaths_AIAN, 
                     "Black" = read$Deaths_Black, 
                     "White" = read$Deaths_White, 
                     "Other" = read$Deaths_Other, 
                     "Native Hawaiian Pacific Islander" = read$Deaths_NHPI, 
                     "Multiracial" = read$Deaths_Multiracial, 
                     "Latinx" = read$Deaths_LatinX, 
                     "Ethnically Not Hispanic" = read$Deaths_Ethnicity_NonHispanic,
                     "Ethnically Hispanic" = read$Deaths_Ethnicity_Hispanic
    )
    population <- switch(input$var, 
                         "Asian" = 188461, 
                         "American Indian Alaska Native" = 50010, 
                         "Black" = 240538, 
                         "White" = 3880000, 
                         "Other" = 60000, 
                         "Native Hawaiian Pacific Islander" = 7000, 
                         "Multiracial" = 154616, 
                         "Latinx" = 1259520, 
                         "Ethnically Not Hispanic" = 4500000,
                         "Ethnically Hispanic" = 1257000
    )
    
    #inital values
    initial_values <- c(S = population, I = cases[1], R = 0, D = deaths[1])
    #xtick <- c(april 15, )
    
    #ode solver
    sird_vals <- reactive({ # make solution reactive so it live updates each time parameters are changed in the app by user
      req(input$alpha1, input$alpha2, input$alpha3, input$alpha4)  # parameters to be read in from widgets
      ode(y = initial_values,
          times = time_values,
          func = sird_eqns,
          parms = c(alpha1 = input$alpha1/population, alpha2 = input$alpha2/population, alpha3 = input$alpha3/population, alpha4 = input$alpha4/population))
    })
    
    sird_vals <- as.data.frame(sird_vals()) # retrieve solved ode values as data points stored in S, I, R and D
    
    with(sird_vals, { # plot
      
      ts.plot(time_values, ts(S), type = "l", col = "blue",
              xlab = "Days (apr 15, 2020 - mar 7, 2021)", ylab = "Number of People", main = "SIRD ODE Model", xaxt = 'n')
      lines(time, ts(I), col = "red")
      lines(time, ts(R), col = "green")
      lines(time, ts(D), col = "black")
      lines(data_time, ts(cases), col = "red", lty = 2)
    })
    
    legend("topright", c("Susceptibles", "Infectious", "Recovered", "Deaths", "Cases from Data"),
           col = c("blue", "red", "green", "black", "red"), lty = c(1,1,1,1,2), bty = "n")
    legend("left", c("Total Deaths:", as.character(sum(deaths)), "Which Is:", as.character(sum(deaths)*100/sum(cases)), "% Of Cases"))
    axis(1, at = seq(0, 300, 50), labels = c("apr 15","jun 4","jul 24","sep 12","nov 1","dec 21","feb 9"), las=2)
  })
  
  output$my_plot2 <- renderPlot({ # plot of race and ethnicity data
    
    cases <- switch(input$var, 
                    "Asian" = read$Cases_Asian, 
                    "American Indian Alaska Native" = read$Cases_AIAN, 
                    "Black" = read$Cases_Black, 
                    "White" = read$Cases_White, 
                    "Other" = read$Cases_Other, 
                    "Native Hawaiian Pacific Islander" = read$Cases_NHPI, 
                    "Multiracial" = read$Cases_Multiracial, 
                    "Latinx" = read$Cases_LatinX, 
                    "Ethnically Not Hispanic" = read$Cases_Ethnicity_NonHispanic,
                    "Ethnically Hispanic" = read$Cases_Ethnicity_Hispanic
    )
    
    deaths <- switch(input$var, 
                     "Asian" = read$Deaths_Asian, 
                     "American Indian Alaska Native" = read$Deaths_AIAN, 
                     "Black" = read$Deaths_Black, 
                     "White" = read$Deaths_White, 
                     "Other" = read$Deaths_Other, 
                     "Native Hawaiian Pacific Islander" = read$Deaths_NHPI, 
                     "Multiracial" = read$Deaths_Multiracial, 
                     "Latinx" = read$Deaths_LatinX, 
                     "Ethnically Not Hispanic" = read$Deaths_Ethnicity_NonHispanic,
                     "Ethnically Hispanic" = read$Deaths_Ethnicity_Hispanic
    )
    
    deathSum <- sum(deaths)
    ts.plot(ts(cases), 
            ts(deaths),
            gpars=list(col = c("red", "blue"), 
                       xlab="Time (apr 15, 2020 - mar 7, 2021)", 
                       ylab="Number of People", 
                       lty=c(1:3), main = "Colorado Cases and Deaths Data"), xaxt = 'n')
    legend("topleft", bty="n", lty=c(1,1), col=c("red","blue"),
           legend=c("Cases", "Deaths"))
    legend("left", c("Total Deaths", as.character(sum(deaths)),"Which Is: ", as.character(sum(deaths)*100/sum(cases)), "% Of this group's cases"))
    axis(1, at = c(0,20,40,60,80), labels = c("apr 15","jun 23","aug 31","nov 7","jan 15"), las=2)
  })
}

shinyApp(ui, server) # create app