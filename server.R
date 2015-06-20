library(shiny)
library(ggplot2)

set.seed(12345)

shinyServer(function(input, output) {
  
  pop  <-  function(pdf){
    mean  <- sum(pdf[1] * pdf[2])
    ex2  <- sum(pdf[1]^2 * pdf[2])
    var  <- ex2 - mean^2
    sd  <- sqrt(var)
    stats  <- list(mean=mean, var=var, sd=sd)
    stats
  }
  
  
  
  output$pdf <- renderPlot({
    prob <- c(input$on, input$tw, input$th, input$fo, input$fi, input$si)
    prob <- prob / sum(prob)
    die  <- data.frame(side = 1:6, prob)
    
    dieMu <- pop(die)$mean
    dieSigma  <- pop(die)$sd
    
    
    ggplot(die, aes(side, prob)) + 
      geom_bar(stat = "identity", fill="#FF9999") +
      scale_x_continuous(breaks=1:6) +
      #     scale_y_continuous(limits=c(0, 0.4)) +
      geom_vline(xintercept = dieMu, color="blue", size=1) +
      #     annotate("segment", x = dufmu, xend = dfmu+dfsigma, y = .35, yend = .35,  
      #              colour = "blue", size=1,
      #              arrow = arrow(angle = 90, length = unit(0.06, "inches"), ends = "both")) +
      #     annotate("text", x = 3.3, y = 0.22, hjust=0, label = "mu == 3.5", angle = 90, parse = TRUE) +
      #     annotate("text", x = 3.8, y = 0.38, hjust=0, label = "sigma == 2.06", parse = TRUE) +
      xlab("") +
      theme_bw()
  })
  
  output$text1 <- renderText({
    prob <- c(input$on, input$tw, input$th, input$fo, input$fi, input$si)
    prob <- prob / sum(prob)
    die  <- data.frame(side = 1:6, prob)
    
    dieMu <- round(pop(die)$mean, 3)
    dieSigma  <- round(pop(die)$sd, 3)
    
    paste0("The population mean is: ", dieMu, "\n",
           "The population standard distribution is: ", dieSigma)
  })
  
  
  output$sampleMeans <- renderPlot({
    
    prob <- c(input$on, input$tw, input$th, input$fo, input$fi, input$si)
    prob <- prob / sum(prob)
    
    iid <- matrix(sample(1:6, input$n * as.integer(input$rep), replace = TRUE, 
                         prob = prob),
                  ncol = input$n)
    iid  <- apply(iid, 1, mean)
    iid  <- data.frame(iid)
    mosm  <- mean(iid[,1])
    
    
    ggplot(iid, aes(iid)) + 
      geom_histogram(fill="#FF9999", binwidth = 0.05) +
      scale_x_continuous(limits=c(0.5, 6.5), breaks = c(1:6)) +
      geom_vline(xintercept = mosm, color="blue", size=1) +
      labs(x = "", y = "") +
      theme_bw()

  })

  output$text2 <- renderText({
    
    prob <- c(input$on, input$tw, input$th, input$fo, input$fi, input$si)
    prob <- prob / sum(prob)
    
    iid <- matrix(sample(1:6, input$n * as.integer(input$rep), replace = TRUE, 
                         prob = prob),
                  ncol = input$n)
    iid  <- apply(iid, 1, mean)
    mosm  <- round(mean(iid), 3)
    sdosm  <- round(sd(iid), 3)
    sigma  <- round(sdosm * sqrt(input$n), 3)
    
    paste0("The mean of sample means is: ", mosm, "\n",
           "The standard distribution of sample means is: ", sdosm, "\n",
           "s * sqrt{n} = ", sigma)
  })

})
