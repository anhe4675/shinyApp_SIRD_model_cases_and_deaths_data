# ShinyApp SIRD model with Cases and Deaths Data by Race and Ethnicity
This code represents an SIRD(susceptible, infected, recovered, death) modeling of COVID-19 from the paper “A parsimonious model for spatial transmission and heterogeneity in the COVID-19 propagation” by Lionel Roques, Olivier Bonnefon, Virgile Baudrot, Samuel Soubeyrand, Henri Berestycki: https://www.medrxiv.org/content/10.1101/2020.07.15.20154740v3 . Modeling of Covid was taken into account for various ethnic and racial groups and compared to a data set for said group’s cases and deaths from the time period of April 15, 2020 - March 7, 2021. 

### Abstract

The goal of this code was to : (1) Create an interactive Shiny app in Rstudio of an SIRD COVID-19 model in Colorado, taking into account different racial and ethnic groups, (2) Divide the graphed time of interest into different key points in the pandemic, (3) Allow users to play around with reactive widgets in the app (adjusting contact rate of key points in time to see how government restrictions affect cases and selecting different populations and graphs to display) (4) See if our predicted model, when layered over real data, could reflect what actually happened. All of these goals will finally allow us to give people the chance to see and understand dynamics of covid and disparities between racial and ethnic groups throughout the pandemic.

### Building the Shiny App

Starting with building shiny apps, the first key aspect is constructing a User Interface and Server. For the UI one must construct the layout of the app. I utilized the following formatting functions: fluidPage() creates the main page of the app, titlePanel() creates the title, sidebarLayout() and sidebarPanel() create a sidebar for me to fill with key info, helpText() lets us write a description in the panel and mainPanel() for us to spit out our plots onto. The UI is also where we may include all necessary widgets. Widgets are buttons, dropdown menus, sliders, text inputs, ect. that you create for users to be able to use and adjust. I included a selectInput(), checkboxGroupInput(), and sliderInput() options, all of which have an “inputId” which we define and write to in the server in order to create corresponding outputs to that widget. The server includes the two reactive plots functions to be displayed in the mainPanel: output$my_plot <- renderPlot({}) and output$my_plot2 <- renderPlot({}). In the first we read in the data and use its initial values to construct our SIRD model. We then create the model with an ode solver and with the retrieved alpha values that correspond to our adjustable widgets in the UI. We read in those reactive alpha values that have inputId = “alpha1” as such: input$alpha1. We then layer the graphed data over top of the SIRD model as well as displaying it in its own personal plot which is in the my_plot2 function. Notice the format to read in the csv file: input$var corresponds to the inputId of our first widget “var” to specify which population group to display. We preface each column to read with “read$” and organize them by race/ethn into deaths and cases using the switch function. Everything is then developed into our final app using the command: shinyApp(ui, server).

### SIRD Model

The models in this app were retrieved from the mentioned paper, a Covid study on France where the SIRD system of differential equations was defined to be: 

* S’(t) = −α(t)/N * S * I
* I’(t) = α(t)/N * S * I − (β + γ) * I
* R’(t) = β * I
* D’(t) = γ * I

where N is population (specified in code for each race/ethn group), β is time until infectious becomes recovered β = 1/(10 days) = 0.1, gamma is death rate of infectious γ = 5 · 10−4, α is contact rate, dependent on time (α(t) ≈  0.3 at beginning of pandemic, α(t) ≈ 0.1 after the lockdown in model and varies more later on)

### Usage

To start the app, open R inside this folder and run
`shiny::runApp("app.R")`.
