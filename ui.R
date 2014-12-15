library(shiny)
library(psych)
load('environment_11_12_14.RData',envir=.GlobalEnv)
shinyUI(fluidPage(
  titlePanel("Predictive Text Analytics"),
  sidebarLayout(
    sidebarPanel(
    tags$textarea(id="userinput",rows="5",cols="40", "Enter Text Here")
    ),
  mainPanel(
	h3("Top 5 Expected Words in Weighted Sorted Order (Most Probable -> Probable)"),
	h4("In case one requires most related single word please selected first word from following"),
    verbatimTextOutput("textarea.out"),
	h4("Please be Patient!! This Works"),
	h5("Five words were given just for better user clarity and understanding"),
	
	h3("----How it works------"),
	h4("As one starts typing in text box in side panel predictions appear in above panel in decreasing weighted order"),
	h4("Code is available at http://github.com/DivyanshBhatia/datasciencecapstone")
	)
  )
  ))

  