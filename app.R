#Packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyWidgets)

library(readr)
library(dplyr)
library(lubridate)
library(reshape2)
library(scales)
library(forcats)
library(DT)

library(ggplot2)
library(ggrepel)
library(ggthemes)

library(fontawesome)

library(psych)
library(sjPlot)

library(drlib)

library(tidyverse)
#dir.create('~/.fonts')
#file.copy("www/SpecialElite-Regular.ttf", "~/.fonts")
#system('fc-cache -f ~/.fonts')

`%not_in%` <- purrr::negate(`%in%`)
is_extant <-function(x) any(!is.na(x))
is_numeric<-function(x) any(is.numeric(x))
# Data import
use <- function(name) {
  csv <- ".csv"
  xlsx <- ".xlsx"
  dta <- ".dta"
  sav <- ".sav"
  if (grepl(csv, name)) {
    readr::read_csv(name)
  } else if (grepl(xlsx, name)) {
    readxl::read_xlsx(name)
  } else if (grepl(dta, name)) {
    haven::read_dta(name)
  } else if (grepl(sav, name)) {
    haven::read_spss(name)
  } else {
    stop("unknown data type.")
  }
}

palette1<-c( "#115740",  "#cc5500",  "#00313c", "#e56a54",
           "#83434e",  "#00b388",  "#f0b323", "#5b6770", 
           "#64ccc9",  "#789D4a",  "#789f90",  "#cab64b",
           "#183028",  "#b9975b")























ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "BxDash"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("About", tabName = "grand_about", icon = icon("book")),
      menuItem("Your Data File", tabName="yourdata", icon = icon("file-upload")),
      menuItem("Individual Students" ,tabName = "onestudent", icon = icon("user-graduate")),
      menuItem("Schoolwide", tabName = "schoolwide", icon = icon("school")),
      menuItem("Teacher Data", tabName = "teacherdata", icon = icon("chalkboard-teacher")),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      tags$p("Copyright 2020 Andrew McCartney")
      
      )   # sidebarmenu
  ), #sidebar
  dashboardBody(
    tabItems(
    tabItem(tabName = "grand_about",
            box(tags$p("BxDash is a bespoke behavior management tracking tool for northern Virginia special education schools developed by Andrew McCartney. Contact the author for access or to join in the fun."),
                tags$p(tags$b("confidentiality"), "This app is generated anew every time the server loads and is crashed every time the server times out. That means that uploaded user data are never stored and are refreshed on each use. While that may be annoying (and certainly limits the utility of the application) it means that the app and user data are nearly impossible to hack and cannot be accessed by any other users. Your data are confidential."))),
    
    tabItem(
      tabName = "yourdata",
      box("BxDash is an opinionated data tool. Your dataset must have the correct structure when uploaded or the analysis will fail and the tool will be useless to you. A primer on the proper data structure goes here: along with a potential image describing the needed variables."),
      box(
        title = "Upload Student Data",
        fileInput("FileInput", "Input Your Student Data"),
        helpText("Dataset must be one of: .csv, .sav, .dta, or .xlsx"),
      ),#upload box
    ),
    
    tabItem(tabName = "onestudent", 
            box(tags$p("student graphs go here.")),
            valueBoxOutput("student_points_valuebox"),
            box("points trajectory"),
            box("heatmap"),
            box("by class")),
    tabItem(tabName = "schoolwide",
            box("school-wide data graphs go here",
            "like faceted trajectory charts"),
            box("student groupings based on levels (e.g. who is what level)")),
    tabItem(tabName = "teacherdata",
            "unclear as now what this tab will show.")
    )#tabItems
  ) #body
)#page




































server <- function(input, output, session) {
  # input the data set
  datasetInput <- reactive({
    infile <- input$FileInput
    if (is.null(infile))
      return(NULL)
    dat<-use(infile$datapath)
    names(dat) <-  gsub(" ", "_", names(dat), fixed = TRUE) 
    return(dat)
    #readr::read_csv(infile$datapath)
  })
  
  # Variables
  output$variable_names <- reactive({
    if (is.null(datasetInput()))
      return(NULL)
    gsub(" ", "_", names(datasetInput()), fixed = TRUE)
  })
  
  
  output$student_points_valuebox <- renderValueBox({
    valueBox(
      "2.11", "Average Daily Points", icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
}



shinyApp(ui, server)
