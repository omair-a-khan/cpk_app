# server.R
# cpk_app

library(ggplot2)

shinyServer(
  function(input, output) {

### Cp ###
    
    output$text.cp1 <- renderText({
      alpha <- 1 - input$conf.cp
      conf2 <- 100*input$conf.cp
      cp.lcl <- input$cp * sqrt(qchisq(alpha/2, input$n.cp - 1)/(input$n.cp - 1))
      cp.lcl <- sprintf("%.2f", round(cp.lcl, 2))
      cp.ucl <- input$cp * sqrt(qchisq(1 - alpha/2, input$n.cp - 1)/(input$n.cp - 1))
      cp.ucl <- sprintf("%.2f", round(cp.ucl, 2))
      paste("The ", conf2, "% confidence interval for a Cp of ", input$cp, " is (",
            cp.lcl, ", ", cp.ucl, ").", sep = "")
    })
    
    output$text.cp2 <- renderText({
      alpha <- 1 - input$conf.cp
      conf2 <- 100*input$conf.cp
      lcb <- input$cp / sqrt(qchisq(alpha/2, input$n.cp - 1)/(input$n.cp - 1))
      lcb <- sprintf("%.2f", round(lcb, 2))
      paste("The minimum value of Cp for which the process is considered capable ", 
            conf2, "% of the time is ", lcb, ".", sep = "")
    })
    
    output$plot.cp2 <- renderPlot({
      n.vec <- c(5:250)
      alpha <- 1 - input$conf.cp
      lcb.cp <- input$cp / sqrt(qchisq(alpha/2, input$n.cp - 1)/(input$n.cp - 1))
      lcb.cp.vec <- input$cp / sqrt(qchisq(alpha/2, n.vec - 1)/(n.vec - 1))

      qplot(n.vec, lcb.cp.vec, geom = "line") +
        xlab('Sample Size (n)') +
        ylab('Minimum Cp') +
        ggtitle('Lower Confidence Bound for Cp') + 
        geom_point(aes(x = input$n.cp, y = lcb.cp), color = "red", size = 2)
    })
    
    output$table.cp2 <- renderDataTable({
      n.vec <- c(5:250)
      alpha <- 1 - input$conf.cp
      lcb.cp <- input$cp / sqrt(qchisq(alpha/2, input$n.cp - 1)/(input$n.cp - 1))
      lcb.cp.vec <- input$cp / sqrt(qchisq(alpha/2, n.vec - 1)/(n.vec - 1))
      lcb.cp.vec <- sprintf("%.2f", round(lcb.cp.vec, 2))
      cptable <- data.frame(n.vec, lcb.cp.vec)
      names(cptable) <- c("Sample Size", "Lower Confidence Bound")
      cptable
    })
    
### Cpk ###
    
    output$text.cpk1 <- renderText({
      alpha <- 1 - input$conf.cpk
      conf2 <- 100*input$conf.cpk
      cpk.lcl <- input$cpk * (1 - qnorm(1 - alpha/2)/sqrt(2*input$n.cpk - 2))
      cpk.lcl <- sprintf("%.2f", round(cpk.lcl, 2))
      cpk.ucl <- input$cpk * (1 + qnorm(1 - alpha/2)/sqrt(2*input$n.cpk - 2))
      cpk.ucl <- sprintf("%.2f", round(cpk.ucl, 2))
      paste("The ", conf2, "% confidence interval for a Cpk of ", input$cpk, " is (",
            cpk.lcl, ", ", cpk.ucl, ").", sep = "")
    })
    
    output$text.cpk2 <- renderText({
      alpha <- 1 - input$conf.cpk
      conf2 <- 100*input$conf.cpk
      lcb.cpk <- input$cpk/(1 - qnorm(1 - alpha/2)/sqrt(2*input$n.cpk - 2))
      lcb.cpk <- sprintf("%.2f", round(lcb.cpk, 2))
      paste("The minimum value of Cpk for which the process is considered capable ", 
            conf2, "% of the time is ", lcb.cpk, ".", sep = "")
    })
    
    output$plot.cpk2 <- renderPlot({
      n.vec <- c(5:250)
      alpha <- 1 - input$conf.cpk
      lcb.cpk <- input$cpk/(1 - qnorm(1 - alpha/2)/sqrt(2*input$n.cpk - 2))
      lcb.cpk.vec <- input$cpk/(1 - qnorm(1 - alpha/2)/sqrt(2*n.vec - 2))

      qplot(n.vec, lcb.cpk.vec, geom = "line") +
        xlab('Sample Size (n)') +
        ylab('Minimum Cpk') +
        ggtitle('Lower Confidence Bound for Cpk') +
        geom_point(aes(x = input$n.cpk, y = lcb.cpk), color = "red", size = 2)
    })
    
    output$table.cpk2 <- renderDataTable({
      n.vec <- c(5:250)
      alpha <- 1 - input$conf.cpk
      lcb.cpk <- input$cpk/(1 - qnorm(1 - alpha/2)/sqrt(2*input$n.cpk - 2))
      lcb.cpk.vec <- input$cpk/(1 - qnorm(1 - alpha/2)/sqrt(2*n.vec - 2))
      lcb.cpk.vec <- sprintf("%.2f", round(lcb.cpk.vec, 2))
      cpktable <- data.frame(n.vec, lcb.cpk.vec)
      names(cpktable) <- c("Sample Size", "Lower Confidence Bound")
      cpktable
    })
  }
)