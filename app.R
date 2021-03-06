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


pointsgraph <- function(df) {
  students <- unique(df$name)
  for (i in students) {
    df %>%
      mutate(day = factor(day))  %>%
      mutate(week = factor(week)) %>%
      select(date, day, week, meanR, meanT, meanF, mean, name) %>%
      melt(id.vars = c("date", "day", "week", "name"))  %>%
      filter(variable != "mean") %>%
      filter(name == i) %>%
      ggplot(aes(x = date, y = value, color = variable))  +
      geom_jitter(alpha = 0.4,
                  width = 0.25,
                  height = 0.25) +
      geom_smooth() +
    #  scale_color_inova() +
      theme_light() +
      labs(title = "Trajectory of all points",
           subtitle = i,
           y = "Average Daily Points") +
      scale_y_continuous(limits = c(0.0, 3.0))
    ggsave(paste0(i, ".png"), device = "png")
  }
}
  
heatmapgraph <- function(df) {
  students <- unique(df$name)
  for (i in students) {
    df  %>%
      filter(name == i) %>%
      # mutate(date = mdy(date))  %>%
      mutate(day = fct_relevel(day, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))  %>%
      mutate(day = factor(day))  %>%
      melt(id.vars = c("date", "day", "week", "name"))  %>%
      filter(variable == "mean") %>%
      mutate(value = as.numeric(value)) %>% 
      ggplot(aes(
        x = day,
        y = week,
        fill = value
      ))  +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "#d52b1e",
                          high = "#6caddf",
                          limits = c(0, 3)) +
      scale_y_reverse() +
      theme_light() +
      geom_text(aes(label = round(value,2))) +
      labs(
        title = paste0(i, " Average Daily Points"),
        subtitle = "2018-2019 School Year",
        x = "Day"
      ) +
      scale_x_discrete(position = "top") 
   # ggsave(paste0(i, ".png"), device = "png", width = 6.2, height = 6.2)
  }
}

palette1<-c( "#115740",  "#cc5500",  "#00313c", "#e56a54",
           "#83434e",  "#00b388",  "#f0b323", "#5b6770", 
           "#64ccc9",  "#789D4a",  "#789f90",  "#cab64b",
           "#183028",  "#b9975b")





ui <- dashboardPagePlus(
  skin = "red",
  dashboardHeaderPlus(title = "BxDash"),
  
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("About", tabName = "grand_about", icon = icon("book")),
      menuItem("Your Data File", tabName="yourdata", icon = icon("file-upload")),
      menuItem("Individual Students" ,tabName = "onestudent", icon = icon("user-graduate")),
      menuItem("Schoolwide", tabName = "schoolwide", icon = icon("school")),
      menuItem("Teacher Data", tabName = "teacherdata", icon = icon("chalkboard-teacher"))
      )   # sidebarmenu
  ), #sidebar
  dashboardBody(
    tabItems(
    tabItem(tabName = "grand_about",
            boxPlus(tags$p("BxDash is a bespoke behavior management tracking tool for northern Virginia special education schools developed by Andrew McCartney. Contact the author for access or to join in the fun."),
                tags$p(tags$b("confidentiality"), "This app is generated anew every time the server loads and is crashed every time the server times out. That means that uploaded user data are never stored and are refreshed on each use. While that may be annoying (and certainly limits the utility of the application) it means that the app and user data are nearly impossible to hack and cannot be accessed by any other users. Your data are confidential."))),
    
    tabItem(
      tabName = "yourdata",
      boxPlus("BxDash is an opinionated data tool. Your dataset must have the correct structure when uploaded or the analysis will fail and the tool will be useless to you. A primer on the proper data structure goes here: along with a potential image describing the needed variables."),
      box(
        title = "Upload Student Data",
        fileInput("FileInput", "Input Your Student Data"),
        helpText("Dataset must be one of: .csv, .sav, .dta, or .xlsx"),
      ),#upload box
    ),
    
    tabItem(tabName = "onestudent", 
            #TODO: Needs columns layout. 
            fluidRow(
              boxPlus(tags$p("student graphs go here."),
                      selectizeInput("stu_select", "Select A Student", 
                                     choices = NULL, 
                                     selected = NULL, 
                                     multiple = FALSE,
                                     options = NULL))
              ),
            fluidRow(
              valueBoxOutput("student_points_valuebox"),
              valueBoxOutput("avg_daily_TOOL"),
              valueBoxOutput("days_attended")
              ),
            fluidRow(
            boxPlus("points trajectory"),
            boxPlus("heatmap"),
            boxPlus("by class")
            ) #fluidrow
            ),#tabItem
    tabItem(tabName = "schoolwide",
            boxPlus("school-wide data graphs go here",
            "like faceted trajectory charts"),
            boxPlus("Schoolwide TOOL chart goes here (with theoretical minimum?)"),
            boxPlus("student groupings based on levels (e.g. who is what level)")),
    tabItem(tabName = "teacherdata",
            "unclear as now what this tab will show.")
    )#tabItems
  ),
  footer = dashboardFooter(
    left_text = "Copyright Andrew McCartney 2020"
  )

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
  
  
  # create list of students to choose from for student tab.
  observeEvent(datasetInput(), {
    updateSelectInput(session, "stu_select", 
                    
                      choices = datasetInput()$student
                      )
  })
  
  
  
  output$student_points_valuebox <- renderValueBox({
    valueBox(
      "2.11", "Average Daily Points", icon = icon("chart-bar"),
      color = "red"
    )
  })
  
  output$avg_daily_TOOL<- renderValueBox({
    valueBox(
      "17.5", "Average Daily Minutes out of Class", icon = icon("clock"),
      color = "red"
    )
  })
  
  
  output$days_attended<- renderValueBox({
    valueBox(
      "37", "Total Days Attendance This Quarter", icon = icon("calendar-alt"),
      color = "red"
    )
  })
}














shinyApp(ui, server)
