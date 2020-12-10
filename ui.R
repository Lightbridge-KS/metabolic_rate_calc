# Metabolic rate calculator v2

library(shiny)
library(dplyr)
library(purrr)
library(pins)

# UI ----------------------------------------------------------------------


ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  withMathJax(),
  
  titlePanel("Metabolic rate & Oxygen Consumption"),
  "By Harvard spirometer :",
  tags$a(href="https://github.com/Lightbridge-AI/metabolic_rate_calc", "Get Code"),
  hr(),
  
  sidebarLayout(
    
    sidebarPanel(
      img(src = "ramp.png",width = 300,height = 300),
      hr(),
      fluidRow(
        column(6,
               numericInput("x","X (mm)",NULL,min = 0)     
        ),
        column(6,
               numericInput("y","Y (mm)",NULL,min = 0)
        )
      ),
      
      fluidRow(
        column(6,
               numericInput("ht","Height (cm)", NULL, min = 0)
        ),
        column(6,
               numericInput("wt", "Weight (kg)", NULL, min = 0)
        )
      ),
      
      fluidRow(
        column(6,
               numericInput("age", "Age (yrs)", NULL, min = 0)
        ),
        column(6,
               radioButtons("sex", "Gender", choices = c("Male","Female"), selected = NULL, inline = F)
        )
      ),
      
      
      
      numericInput("temp","Temperature ( ˚c)", NULL),
      numericInput("baro", "Barometric pressure (mmHg)", NULL, min = 0),
      numericInput("cal_eqi", "Caloric equivalent of Oxygen (Cal/hr)", value = 4.825),
      helpText("( Note : default value at RQ = 0.82 )")
      
      
      
      
      
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Data",
                 br(),
                 dataTableOutput("table"),
                 fluidRow(
                   column(width = 12, downloadButton("download", "Download .xlsx",class = "btn-block"))
                 )
        ),
        tabPanel("Formular",
                 img(src = "formular.png",width = 697,height = 381),
                 h4("STPD correction factors"),
                 helpText("Training data  :  Lab direction-determination of metabolic rate;  Appendix I"),
                 helpText("Model  :  Multiple linear regresssion"),
                 helpText("Predictors  :  Barometric pressure, Temperature"),
                 helpText("Outcome  :  STPD correction factor "),
                 
                 br(),
                 h4("Body surface area"),
                 helpText("Using DuBois formular"),
                 helpText("$$BSA = 0.007184 \\times Height^{0.725} \\times Weight^{0.425}$$"),
                 helpText("(Height in cm,  Weight in kg)", align = "right"),
                 helpText("similar to this",tags$a(href="http://www-users.med.cornell.edu/~spon/picu/calc/bsacalc.htm", 
                                                   "calculator")),
                 
                 br(),
                 h4("BMR standards"),
                 helpText("Training data  :  Lab direction-determination of metabolic rate;  Appendix III : Basal Heat Production Standard"),
                 helpText("Model  :  Smoothing spline (spar = 0.5)"),
                 helpText("Predictors  :  Age , Sex"),
                 helpText("Outcome  :  BMR")
                 
                 
                 
        )
      )
      
      
      
    )
  )
)

