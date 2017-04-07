library(shiny)

fluidPage(
    titlePanel(""),
    fluidRow(
        column(3,
               wellPanel(
                   sliderInput("tension", "Tension", 0.3,min=0,max=1,step = 0.01),
                   sliderInput("fontsize","Font size", 12, min=6, max=24),
                   sliderInput("width","Width and height", 830, min=200, max=1200),
                   sliderInput("padding","Padding", 100, min=0, max=300),
                   uiOutput("cutoffui")
                   
               ),
               wellPanel(
                   icon("warning"),
                   tags$small("Report issues here: "),
                   HTML(paste("<a href=https://github.com/SMAC-Group/pair-vis/issues>")),
                   icon("github"),
                   HTML(paste("</a>"))
               )
        ),
        column(9,
               #verbatimTextOutput("type"),
               uiOutput("circplot")
        )
    )
)
