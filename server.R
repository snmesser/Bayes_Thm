library(shiny)

bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

shinyServer(function(input, output, session) {
  
  t_neg = 0
  t_pos = 0
  f_neg = 0
  f_pos = 0
  
  observeEvent({
    input$spec
    input$sens
    input$infect
    input$new
    input$go
    input$ques
  },
  {
    observeEvent(input$go, {
      updateTabItems(session, "tabs", "explore")
    })
    
    
    output$plot1 <- renderPlot({
  
      plot(x = NULL, y = NULL, 
           xlim = c(0, 43),
           ylim = c(-5, 25),
           xaxt="n",
           yaxt="n",
           ann=FALSE,
           bty = "n")
      
      k = 1
      
      pick <- rnorm(1000)
      test<- rnorm(1000)
      
      for (i in c(1:40)) {
        for (j in c(1:25)) {
          if (pick[k] > qnorm(1 - (input$infect / 1000))) {
            if (test[k] > qnorm(1 - input$sens)) {
              points(i, j, pch = 21, col = "blue", bg = "green", cex = 1.75)
              t_pos <<- t_pos + 1
            } 
            else {
              points(i, j, pch = 19, col = "red", cex = 1.75)
              f_neg <<- f_neg + 1
            }
          }
          else {
            if (test[k] > qnorm(input$spec)) {
              points(i, j, pch = 19, col = "blue", cex = 1.75)
              f_pos <<- f_pos + 1
            }
            else {
              points(i, j, pch = 19, col = "black", cex = 0.75)
              t_neg <<- t_neg + 1
            }
          }
          k = k + 1
        }
      }
      
      legend(x = "bottom", legend = c(paste0("True Negative (", t_neg, ")"),
                               paste0("True Positive (", t_pos, ")"),
                               paste0("False Negative (", f_neg, ")"),
                               paste0("False Positive (", f_pos, ")")), 
             horiz = FALSE, bty = "n", pch = 21, col = c("black", "blue", "red", "blue"),
             pt.cex = c(0.75, 2, 2, 2), pt.bg = c("black", "green", "red", "blue"), ncol = 2)
    }, bg = "#F5F5F5")

    numbers <- reactiveValues(question=c())
    
    num_qs <- length(bank$question)
    
    observe({
      numbers$question=sample(1:num_qs,1)
    })
    
    output$question<-renderText(bank[numbers$question, 2])
    
    # output$formular <- renderUI({
    #   withMathJax(
    #     helpText('$$P(Disease|Positive) = \\frac{P(Positive|Disease) * P(Disease)}
    #              {P(Positive|Disease) * P(Disease)+P(Positive|No Disease) * P(No Disease)}
    #              = \\frac{Sensitivity * \\frac{Prevalence}{1000}}
    #              {Sensitivity * \\frac{Prevalence}{1000}
    #              + (1 - Specificity) * (1 - \\frac{Prevalence}{1000})}$$'))
    # })
    
    output$calculation <- renderUI({
      if (!input$pop_result) return()
      withMathJax(
        helpText(sprintf('$$\\huge{P(Disease|Positive) = 
                         \\frac{Sensitivity * \\frac{Prevalence}{1000}}
                         {Sensitivity * \\frac{Prevalence}{1000}
                         + (1 - Specificity) * (1 - \\frac{Prevalence}{1000})} =
                         \\frac{%s * \\frac{%s}{1000}}
                         {%s * \\frac{%s}{1000} + (1 - %s) 
                         * (1 - \\frac{%s}{1000})} = %1.3f}$$', 
                         input$sens, input$infect, input$sens, input$infect, input$spec, input$infect,
                         (input$sens * (input$infect/1000)) / 
                           ((input$sens * (input$infect/1000)) + ((1 - input$spec) * (1 - (input$infect/1000))))))
      )
    })
    
    # output$t_neg <- renderText({
    #   if (t_neg != 1) {
    #     sprintf("%s people don't have the disease and had negative test results.", t_neg)
    #   } else {
    #     sprintf("%s person doesn't have the disease and had a negative test result.", t_neg)
    #   }
    # })
    # 
    # output$t_pos <- renderText({
    #   if (t_pos != 1) {
    #     sprintf("%s people have the disease and had positive test results.", t_pos)
    #   } else {
    #     sprintf("%s person has the disease and had a positive test result.", t_pos)
    #   }
    # })
    # 
    # output$f_pos <- renderText({
    #   if (f_pos != 1) {
    #     sprintf("%s people don't have the disease and had positive test results.", f_pos)
    #   } else {
    #     sprintf("%s person doesn't have the disease and had a positive test result.", f_pos)
    #   }
    # })
    # 
    # output$f_neg <- renderText({
    #   if (f_neg != 1) {
    #     sprintf("%s people have the disease and had negative test results.", f_neg)
    #   } else {
    #     sprintf("%s person has the disease and had a negative test result.", f_neg)
    #   }
    # })
    
    output$result <- renderText({
      pos = t_pos + f_pos
      if (pos != 1) {
        sprintf("There were %s positive results, of which %s actually had the disease.
                This gives a sample estimate of a %2.f %% chance of actually having the disease if one tests positive for it.",
                pos, t_pos, ((t_pos / pos) * 100))
      }
    })
    
    t_neg = 0
    t_pos = 0
    f_neg = 0
    f_pos = 0
  })
})
          
