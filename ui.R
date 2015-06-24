# ui.R
# cpk_app

shinyUI(fluidPage(
  withMathJax(),
  titlePanel("Cpk Calculator"),
  sidebarLayout(
    sidebarPanel(
      helpText("Choose the following parameters to approximate a lower confidence bound for Cpk."),
      numericInput("cpk", 
                   label = h5("Desired Cpk"), 
                   value = 1.33,
                   min = 0,
                   step = 0.01),
      numericInput("n", 
                   label = h5("Sample size"), 
                   value = 30,
                   min = 5,
                   max = 250),
      sliderInput("conf", label = h5("Confidence level percentage"),
                  min = 0, max = 1, value = 0.95),
      helpText('The formula used to approxiate the lower bound is from Kushler and Hurley (1992): $$LCL = \\hat{C}_{pk}\\left(1 - \\frac{Z_{\\alpha/2}}{\\sqrt{2(n-1)}}\\right)$$')
      ),
    mainPanel(
      textOutput("text1"),
      plotOutput("plot1")
      )
  )
))