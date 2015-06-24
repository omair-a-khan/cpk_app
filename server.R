# server.R
# cpk_app

library(ggplot2)

shinyServer(
  function(input, output) {
    output$text1 <- renderText({ 
      alpha <- 1 - input$conf
      lcb <- input$cpk/(1 - qnorm(1 - alpha/2)/sqrt(2*input$n - 2))
      lcbround <- round(lcb, 3)
      conf2 <- 100*input$conf
      paste("The minimum value of Cpk for which the process is considered capable ", conf2, "% of the time is ", lcbround, ".", sep = "")
    })
    output$plot1 <- renderPlot({
      n.vec <- c(5:250)
      alpha <- 1 - input$conf
      lcb <- input$cpk/(1 - qnorm(1 - alpha/2)/sqrt(2*input$n - 2))
      lcb.vec <- input$cpk/(1 - qnorm(1 - alpha/2)/sqrt(2*n.vec - 2))
      
      ylowerlim <- floor(lcb.vec[246])
      
      qplot(n.vec, lcb.vec, geom = "line") +
        xlab('Sample Size (n)') +
        ylab(expression(Minimum~C[p*k])) +
        ylim(ylowerlim, lcb.vec[1]) +
        geom_point(aes(x = input$n, y = lcb), color = "red", size = 2)
    })
  }
)