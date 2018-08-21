library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)

shinyUI(fluidPage(
  
  withMathJax(),
  
  dashboardPage(skin = "blue",
                dashboardHeader(title = "Bayes' Theorem"),
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabs",
                    menuItem("Prerequisite", icon  = icon("book"), tabName = "prereq"),
                    menuItem("Overview", tabName = "overview", icon = icon("dashboard"), selected = TRUE),
                    menuItem("Explore", icon = icon("wpexplorer"), tabName = "explore")
                  )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "prereq",
                            fluidPage(
                              h2("Information you should know about Bayes' Theorem:"),
                              h3("For two events D and T, Bayes' Theorem relates P(D|T) to P(T|D) through:"),
                              fluidRow(
                                column(12, align="center",
                                       uiOutput('calculation_part'))),
                              h3("In the screening test example used in this application, we define:"),
                              h3("D = Has the disease"),
                              h3("T = Tests positive for the disease"),
                              h3("P(D) is called the Prevalence"),
                              h3("P(T|D) is called the Sensitivity"),
                              h3("P(Not T|Not D) is called the Specificity")
                              )),
                    tabItem(tabName = "overview",
                            fluidRow(
                              column(3,
                                     tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)))),
                            fluidPage(
                              h2("About:"),
                              h3("This app is designed to demonstrate Bayes' Theorem using the classic example of disease incidence."),
                              h2("Instructions:"),
                              h3("Adjust the sliders to help you solve the challenges."),
                              h2("Acknowledgement:"),
                              h4("This app was developed and coded by Sam Messer."),
                              div(style = "text-align: center",
                                  bsButton("go", "GO!", size = "large", style = "primary", icon = icon("bolt")))
                                 )),
                    tabItem(tabName = "explore",
                            tags$head(
                              tags$style(".shiny-notification {position:fixed;bottom:50px;left:25%;width:50%;} ")),
                            div(style="display: inline-block;vertical-align:top;",
                                circleButton("hint",icon = icon("question",class = "glyphicon glyphicon-question-sign"), size = "xs"),
                                bsPopover("hint", "Hint","Think about the way each slider affects the probability",placement = "bottom", trigger = "hover", options = NULL)
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
                                         bsPopover("infect", "Prevalence (Per 1000 People)","The average number of people (per 1000) that has the disease.",
                                                   placement = "bottom", trigger = "click", options = NULL),
                                         sliderInput("spec", "Specificity", min = 0.5, max = 0.999, value = 0.99, step = 0.001),
                                         bsPopover("spec", "Specificity","The probability of someone who <b>does not have</b> the disease testing positive for it.",
                                                   placement = "bottom", trigger = "click", options = NULL),
                                         sliderInput("sens", "Sensitivity", min = 0.5, max = 0.999, value = 0.995, step = 0.001),
                                         bsPopover("sens", "Sensitivity","The probability of someone who <b>has</b> the disease testing positive for it.",
                                                   placement = "bottom", trigger = "click", options = NULL),
                                         bsButton("new", " Generate New Sample", icon("retweet"))
                                         
                                  ),
                                  column(6,
                                         plotOutput("plot1"),
                                         bsPopover("plot1", "Sample Results","The points show a sample of 1000 people from the population. All are tested for the disease, and the results are displayed by the size and color of dot.",
                                                   placement = "bottom", trigger = "click", options = NULL)
                                  ),
                                  column(3, align = "center",
                                         h2("Challenge:"),
                                         textOutput("question"),
                                         br(),
                                         bsButton("ques", "New Challenge"),
                                         br(),
                                         br(),
                                         textOutput("sample_ans"),
                                         br(),
                                         bsButton("show_ans", "Show Sample Answer")
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
                                         checkboxInput('pop_result', 'Show Theoretical Result', value = TRUE))
                                )
                                )))
                              )
                            )
                            )))