library(shiny)
library(shinydashboard)
library(plotrix)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  withMathJax(),
  
  dashboardPage(skin = "blue",
                dashboardHeader(title = "Bayes' Theorem"),
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabs",
                    menuItem("Prerequisite", icon  = icon("drupal"), tabName = "prereq"),
                    menuItem("Overview", tabName = "overview", icon = icon("th")),
                    menuItem("Explore", icon = icon("bar-chart"), tabName = "explore")
                  )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "prereq",
                            fluidPage(
                              h1("BLANK"),
                              bsButton("move", "Go to the Overview", size = "large", style = "primary")
                            )),
                    tabItem(tabName = "overview",
                            fluidRow(
                              column(3,
                                     img(src="psu.PNG", width = 100)),
                              column(9,
                                     h1("About:"))),
                            fluidPage(
                              h3("This app is designed to demonstrate Bayes' Theorem using the classic example of disease incidence."),
                              h2("Instructions:"),
                              h3("1. Pick a disease from the drop-down menu."),
                              h3("2. Use the sliders to decide accuracy of tests for those who have and don't have the disease."),
                              h3("3. Observe the overall accuracy of the test."),
                              h2("Acknowledgement:"),
                              h4("This app was developed and coded by Sam Messer."),
                              div(style = "text-align: center",
                                  bsButton("go", "G O !", size = "large", style = "primary"))
                                 )),
                    tabItem(tabName = "explore",
                            tags$head(
                              tags$style(".shiny-notification {position:fixed;bottom:50px;left:25%;width:50%;} ")),
                            div(style="display: inline-block;vertical-align:top;",
                                circleButton("hint",icon = icon("question",class = "glyphicon glyphicon-question-sign"), size = "xs"),
                                bsPopover("hint", "Hint","Think about the way each slider affect the probability",placement = "bottom", trigger = "hover", options = NULL)
                            ),
                            
                            div(style="display: inline-block;vertical-align:top;",
                                circleButton("info",icon = icon("info",class = "glyphicon glyphicon-info-sign"), size = "xs"),
                                bsPopover("info", "Instructions","Adjust sliders and observe the effects",placement = "bottom", trigger = "hover", options = NULL)
                            ),
                            h4(
                              wellPanel(
                                fluidRow(
                                  column(3,
                                         sliderInput("infect", "Prevalence (Per 1000 People)", min = 1, max = 100, step = 1, value = 5),
                                         bsPopover("infect", "Prevalence (Per 1000 People)","The average number of people (per 1000) that get infected by the disease.",
                                                   placement = "bottom", trigger = "hover", options = NULL),
                                         sliderInput("spec", "Specificity", min = 0.5, max = 0.999, value = 0.99, step = 0.001),
                                         bsPopover("spec", "Specificity","The probability of someone who <b>does not have</b> the disease testing positive for it.",
                                                   placement = "bottom", trigger = "hover", options = NULL),
                                         sliderInput("sens", "Sensitivity", min = 0.5, max = 0.999, value = 0.995, step = 0.001),
                                         bsPopover("sens", "Sensitivity","The probability of someone who <b>has</b> the disease testing positive for it.",
                                                   placement = "bottom", trigger = "hover", options = NULL),
                                         bsButton("new", " Generate New Sample", icon("paper-plane"))
                                         
                                  ),
                                  column(6,
                                         plotOutput("plot1"),
                                         bsPopover("plot1", "Sensitivity","The probability of someone who <b>has</b> the disease testing positive for it.",
                                                   placement = "bottom", trigger = "hover", options = NULL)
                                  ),
                                  column(3,
                                         h2("Challenge:"),
                                         textOutput("question"),
                                         bsButton("ques", "New Challenge", icon("amazon"))
                                         )),
                                fluidRow(
                                  # column(12,align="center",
                                  #        uiOutput('formular')),
                                  column(12, align = "center",
                                         textOutput("result"))
                                  
                                ),
                                fluidRow(
                                  column(12, align="center",
                                         uiOutput('calculation')),
                                  br(),
                                  column(3, 
                                         checkboxInput('pop_result', 'Show Theoretical Result', value = FALSE))
                                )
                                )))
                              )
                            )
                            )))