# ui.R
# cpk_app

shinyUI(fluidPage(
  navbarPage("Process Capability Index Calculator",
             
    tabPanel("Cp",
      sidebarLayout(
        sidebarPanel(
          radioButtons("calccp", label = h5("Input"),
                       choices = list("Measured Cp" = 1, "Desired Cp" = 2), selected = 1),
          numericInput("cp", 
                       label = h5("Cp"), 
                       value = 1.33,
                       min = 0.01,
                       step = 0.01),
          numericInput("n.cp", 
                       label = h5("Sample size"), 
                       value = 30,
                       min = 5,
                       max = 250),
          sliderInput("conf.cp", label = h5("Confidence level percentage"),
                      min = 0, max = 1, value = 0.95)
        ),
        mainPanel(
          conditionalPanel(condition = "input.calccp == 1",
            textOutput("text.cp1")
          ),
          conditionalPanel(condition = "input.calccp == 2",
            tabsetPanel(
              tabPanel("Summary", textOutput("text.cp2"), plotOutput("plot.cp2")),
              tabPanel("Table", dataTableOutput("table.cp2"))
            )
          )
        )
      )
    ),
    
    tabPanel("Cpk",
      sidebarLayout(
        sidebarPanel(
          radioButtons("calccpk", label = h5("Input"),
                      choices = list("Measured Cpk" = 1, "Desired Cpk" = 2), selected = 1),
          numericInput("cpk",
                      label = h5("Cpk"),
                      value = 1.33,
                      min = 0.01,
                      step = 0.01),
          numericInput("n.cpk",
                      label = h5("Sample size"),
                      value = 30,
                      min = 5,
                      max = 250),
          sliderInput("conf.cpk", label = h5("Confidence level percentage"),
                      min = 0, max = 1, value = 0.95)
        ),
        mainPanel(
          conditionalPanel(condition = "input.calccpk == 1",
            textOutput("text.cpk1")
          ),
          conditionalPanel(condition = "input.calccpk == 2",
            tabsetPanel(
              tabPanel("Summary", textOutput("text.cpk2"), plotOutput("plot.cpk2")),
              tabPanel("Table", dataTableOutput("table.cpk2"))
            )
          )
        )
      )
    )
  )
))