require(shiny)
library(googleVis)



shinyUI(navbarPage("Prediction Star Review from its Text Alone",  
                   tabPanel("Prediction", 
                            
                            
                            sidebarPanel(width=3,
                                         h4("Text Input:", style="color:#4B0082"),
                                         textInput ("entry", 
                                                    " ",
                                                    "This is the best"),
                                         hr(),
                                         h5("The app will predict the star rating based on text review. Please type your review or copy and paste single/multiple reviews", style="color:#4B0082"),
                                         hr(),
                                         h6("This App is built for:"),
                                         a(img(src = "capstone.png", height = 300, width = 270),href="https://www.coursera.org/course/dsscapstone"),
                                         hr(),
                                         h6("For more information about Albion:"),
                                         a(img(src = "git.png", height = 30, width = 30),href="https://github.com/albiondervishi/Data-Science-Capstone"),
                                         a(img(src = "lin.png", height = 30, wdth = 30),href="https://www.linkedin.com/in/albion-dervishi-b0248733"),
                                         a(img(src = "gmail.jpg", height = 30, width = 30),href="mailto: albiondervishi@gmail.com"),
                                         br()
                            )),
                   
                   
                   mainPanel(
                     htmlOutput("view")
                     
                   ),
                   
                   
                   
                   tabPanel("Algorithm",                      
                            mainPanel(            includeMarkdown("Capstone.Rmd")
                                                  
                            ))
))









