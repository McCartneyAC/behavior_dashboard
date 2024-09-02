library(shiny)
library(bslib)
library(tibble)
library(googlesheets4)
gs4_auth()  # Interactive authentication
# options(
#   # whenever there is one account token found, use the cached token
#   gargle_oauth_email = TRUE,
#   # specify auth tokens should be stored in a hidden directory ".secrets"
#   gargle_oauth_cache = "your-app-folder-name/.secrets"
# )


incidentlist<-tribble(
  ~`Incident Descriptor`,~`Level of Incident`,
  "Не виконує завдання Not on task",1,
  "Відволікає інших студентів/ розмовляє на уроці  Disrupting another student/chatting in class",1,
  "Відволікає, перериває Distraction, interruption",1,
  "Відповідає зухвало, зневажливо Answering back ",1,
  "Ігнорує інструкції викладача Not taking instruction",1,
  "Каже неправду/підставляє інших Telling lies/getting others into trouble",1,
  "Дрібні словесні образи, незначна ненормативна лексика Minor verbal insults, minor inappropriate language",1,
  "Небезпечне пересування по класу Unsafe movement around the school classroom",1,
  "Небезпечна поведінка Unsafe behaviour",1,
  "Необережне пошкодження чогось Careless damage",1,
  "Навмисне пошкодження майна (вперше) Deliberate damage to property (first time)",1,
  "Ланч/перерва  Break/lunch time",1,
  "Постійне повторювання (помилок) Першого Рівня  Persistence of Level One ",2,
  "Невиконані завдання (навмисно) Incomplete tasks (deliberate)",2,
  "Відмовляється працювати Refusal to work",2,
  "Непокора Defiance",2,
  "Навмисне знищення роботи іншого учня Deliberate destruction of another student’s piece of work",2,
  "Незначне умисне пошкодження майна Minor deliberate damage to property",2,
  "Вилучення чиєїсь власності/намір вилучити чиюсь власність без дозволу Removing someone’s property/intending to remove someone’s property without their permission",2,
  "Прямі словесні образи Direct verbal insults",2,
  "Расові або релігійні образи (примітка.   Батьки/опікуни повинні бути проінформовані, а інцидент зафіксований у папці «Безпечно вивчати»)  Racial or religious insults (note. Parents/Guardians must be  informed and incident recorded in Safe to Learn Folder)",2,
  "Окремий випадок фізичного ушкодження (незначного) Isolated incident of physically hurting someone (minor)",2,
  "Агресивна поведінка Aggressive behaviour",2,
  "Знущання, постійні обзивання (примітка. Батьки/опікуни повинні бути проінформовані та зафіксовані випадки) Bullying, persistent name calling (note. Parents/Guardians must be informed and incident recorded)",2,
  "Постійне повторювання (помилок) Другого Рівня  Persistence of Level Two",3,
  "Сильне порушення класної діяльності Major disruption of class activity",3,
  "Послідовне або серйозне навмисне пошкодження майна Consistent or serious deliberate damage to property",3,
  "Вилучення чиєїсь власності/намір вилучити чиюсь власність без дозволу (постійно) Removing someone’s property/intending to remove someone’s property withouttheir permission (persistent)",3,
  "Повторні випадки булінгу Repeated incidents of bullying",3,
  "Постійна нецензурна лексика та словесні/расові образи Persistent bad language and verbal insults/racial insults",3,
  "Небезпечна відмова виконувати інструкції Dangerous refusal to obey an instruction",3,
  "Залишення приміщення школи без згоди/попередження викладача Leaving school premises without consent",3,
  "Куріння, вживання заборонених речовин, включаючи алкоголь Smoking, use of illegal substances, including alcohol",3,
  "Постійні випадки фізичного ушкодження інших людей (незначні) або серйозні випадки фізичного ушкодження когось Persistent incidents of physically hurting other people (minor) or a serious incident of physically hurting someone ",3,
  "Образлива або загрозлива поведінка по відношенню до дорослих або учнів Abusive or threatening behaviour towards adults or students",3,
  "Інше Other",NA)

requiredfus<-tribble(
  
  ~Number_of_Incidents, ~Intervention_Level, ~REQUIRED_FOLLOWUP,
  0,1,"Teacher Student Meeting",
  1,1,"Teacher Student Meeting",
  2,1,"Teacher Student Meeting",
  3,1,"Teacher Student Meeting",
  4,2,"Teacher Student Meeting + Reflection Time",
  5,3,"Teacher Student Parent Informal Meeting + behaviour support",
  7,4,"Teacher Student Parent Formal Meeting + behaviour support",
  8,5,"Teacher Student Parent Head of Discipline Formal Meeting",
  9,6,"Teacher Student Parent Head of Discipline Head of School Formal Meeting",
  10,7,"Internal Exclusion 1 day",
  11,8,"External Exclusion 1 day",
  12,9,"External Exclusion 3 day"#,
  #13,10,"Permanent Exclusion",
  #100,"AR","Student At Risk - HoS discretion only"
  
)

# Load data from Google Sheets

students = read_sheet("https://docs.google.com/spreadsheets/d/1TEKoMJqtsHsUedit?gid=0#gid=0", sheet = "Sheet1")
staff = read_sheet("https://docs.google.com/spreadsheets/d/150vc389D9gdit?gid=0#gid=0", sheet = "Sheet1")
incidents = read_sheet("https://docs.google.com/spreadsheets/d/1xjW4aNxTBSQ/edit?gid=0#gid=0", sheet = "Sheet1")
followup = read_sheet("https://docs.google.com/spreadsheets/d/1Gnt5DdIlEvDsY/edit?gid=0#gid=0")

my_theme <- bs_theme(
  bootswatch = "sketchy",  
  primary = "#115740",    
  secondary = "#cc5500", 
  #base_font = font_google("Lato"),  # Use a Google font
  #code_font = font_google("Space_Mono")  # Font for code blocks
)

# UI
ui <- fluidPage(
  theme = my_theme, 
  navbarPage(
    title = "BISU Behavior Tracking",
    tabPanel(
      "Home", icon = icon("home"),
      fluidRow(
        column(6, 
               h2("About"), 
               p("This is an app-based approach to collecting information on behavioral incidents at BISU, Pechersk campus.")),
        column(6, 
               h3("How to use this App"), 
               p("Use the appropriate tab for the kind of infraction or follow-up you intend to report. Fill in each drop down and click submit."))
      ) #fluidrow
    ), #tabpanel 
    tabPanel(
      "Behavior Report", icon = icon("children"),
      fluidRow(
        column(12, 
               h2("Behavior Report"), 
               #p("Show data visualizations here."),
               selectInput(inputId = "bx_staffname", label = "Ім'я співробітника Name of Staff Member:",choices = NULL),
               selectInput(inputId = "bx_stuyeargroup", label = "Група студента Year Group of Student:",choices = 1:13),
               selectInput(inputId = "bx_stuname", label = "Ім'я студента Name of Student:",choices = NULL),
               selectInput(inputId = "bx_incidenttype", label = "Тип інцидента Type of Incident:", choices = incidentlist$`Incident Descriptor`),
               textAreaInput(inputId = "bx_incidentdetails", label = "Деталі інцидента Details of Incident:"),
               actionButton(inputId = "bx_submit", icon = icon( "paper-plane"), label = "Submit"),
               br(),br()
               ))#fluidrow
      ), #tabPanel
    tabPanel(
      "Uniform Infringement", icon = icon("user-tie"), 
      fluidRow(
        column(12, 
               h2("Uniform Infraction"), 
               p("Student tie was half windsor instead of Full. Student skirt was 1.5cm too short. "))
      )
    ), #tabPanel
    tabPanel(
      "Follow Up Report", icon = icon("school"),
      fluidRow(
        column(12, 
               h2("Follow Up Report"), 
               #p("Show data visualizations here."),
               selectInput(inputId = "fu_staffname", label = "Ім'я співробітника Name of Staff Member:",choices = NULL),
               selectInput(inputId = "fu_stuname", label = "Ім'я студента Name of Student:",choices = NULL),
               selectInput(inputId = "fu_incidenttype", label = "Тип інцидента Type of Incident:", choices = incidentlist$`Incident Descriptor`),
               selectInput(inputId = "fu_followupreq", label= "Follow-up Required", choices = requiredfus$REQUIRED_FOLLOWUP),
               selectInput(inputId = "fu_incidentlevel", label= "Incident Level", choices = 1:9),
               selectInput(inputId = "fu_numincidenton", label = "Number of Incidents on", choices = 0:10),
               textAreaInput(inputId = "fu_followupdetails", label = "Follow up Details"),
               #selectInput(inputId = "adjintlevel", label = "Adjusted Intervention Level", choices = 0:10),
               actionButton(inputId = "fu_submit", icon = icon("paper-plane"), label = "Submit"),
               br(),br()
               
               ) # colummn
      ) #fluidrow
    ), #tabPanel
    tabPanel(
      "Admin Only", icon = icon("graduation-cap"),
      fluidRow(
        column(12, 
               h2("Admin Only"), 
               #p("Show data visualizations here.")
              
               selectInput(inputId = "admin_stuname", label = "Student",# icon = icon("child-reaching"),
                           choices = NULL),
        br(),br(),
        
        DT::DTOutput("by_student")
        ) #column
      ) #fluidrow
    ) #tabPanel
  )
)

# Server
server <- function(input, output, session) {
  
  values <- reactiveValues(
    students = read_sheet("https://docs.google.com/spreadsheets/d/1r48/edit?gid=0#gid=0", sheet = "Sheet1"),
    staff = read_sheet("https://docs.google.com/spreadsheets/d/150vc389DY5SIo/edit?gid=0#gid=0", sheet = "Sheet1"),
    incidents = read_sheet("https://docs.google.com/spreadsheets/d/1xeTBSQ/edit?gid=0#gid=0", sheet = "Sheet1"),
    followup = read_sheet("https://docs.google.com/spreadsheets/d/1Gnt5Du7-igAdIlEvDsY/edit?gid=0#gid=0", sheet = "Sheet1")
  )
  
  observe({
    updateSelectInput(session, "bx_staffname", choices = unique(values$staff$staff_name))
    updateSelectInput(session, "bx_stuname", choices = unique(values$students$student_name))
    updateSelectInput(session, "fu_staffname",choices = unique(values$staff$staff_name))
    updateSelectInput(session, "fu_stuname",choices = unique(values$students$student_name))
    #updateSelectInput(session, "admin_stuname", choices = unique(values$students$student_name))
    updateSelectInput(session, "admin_stuname", choices = unique(values$incidents$bx_stuname))
    # Update incident level choices dynamically (depending on logic)
    updateSelectInput(session, "incidentlevel", choices = sort(unique(incidentlist$`Level of Incident`)))
  }) 
  
  
  observeEvent(input$bx_submit, {
    new_data <- tibble(student = input$bx_stuname,
                       yeargroup = input$bx_stuyeargroup,
                       staff = input$bx_staffname,
                       type = input$bx_incidenttype,
                       details = input$bx_incidentdetails,
                       date = as.character(Sys.Date()),
                       time = as.character(Sys.time())
                       )
    # Convert tibble to data.frame
    new_data_df <- as.data.frame(new_data)
    
    # Show success modal
    showModal(modalDialog(
      title = tagList(icon("thumbs-up"), "Submission Successful"),
      "The behavior incident has been successfully submitted.",
      easyClose = TRUE
    ))
    
    print(new_data)
    print(str(new_data_df))  # Print the structure of the data frame
    
    #sheet_append(new_data, "https://docs.google.com/spreadshe1weF7eTBSQ/edit?gid=0#gid=0", sheet = "Sheet1")
    #values$incidents <- read_sheet("https://docs.google.com/spreadsheets/d/-MKr1weF7eTBSQ/edit?gid=0#gid=0", sheet = "Sheet1")
    tryCatch({
      sheet_append(data = new_data, 
                   ss = "https://docs.google.com/spreadsheets/d/1xjW4aNxmtr1weF7eTBSQ/edit?gid=0#gid=0", 
                   sheet = "Sheet1")
      values$incidents <- read_sheet("https://docs.google.com/spreadsheets/d/1xjW4aNxmF7eTBSQ/edit?gid=0#gid=0", sheet = "Sheet1")
    }, error = function(e) {
      print(paste("Error in sheet_append:", e$message))
      showModal(modalDialog(
        title = tagList(icon("thumbs-down"), "Submission Failed"),
        paste("Error:", e$message, "Contact Mr. Andrew or Ms. Rachel for help."),
        easyClose = TRUE
      ))
    })
    
    # Reset all input fields
    updateSelectInput(session, "bx_stuname", selected = "")
    updateSelectInput(session, "bx_stuyeargroup", selected = NULL)
    updateSelectInput(session, "bx_staffname", selected = "")
    updateSelectInput(session, "bx_incidenttype", selected = NULL)
    updateTextAreaInput(session, "bx_incidentdetails", value = "")
    
    
    })
  
  observeEvent(input$fu_submit, {
    follow_up_data <- tibble(
      staff_name = input$fu_staffname,
      student_name = input$fu_stuname,
      incident_type = input$fu_incidenttype,
      follow_up_required = input$fu_followupreq,
      incident_level = input$fu_incidentlevel,
      num_incidents = input$fu_numincidenton,
      follow_up_details = input$fu_followupdetails,
      date = as.character(Sys.Date()),
      time = as.character(Sys.time())
    )
    
    # Convert tibble to data.frame
    follow_up_data <- as.data.frame(follow_up_data)
    
    print(follow_up_data)
    print(str(follow_up_data))  # Print the structure of the data frame
    
    # Show success modal
    showModal(modalDialog(
      title = tagList(icon("thumbs-up"), "Submission Successful"),
      "The incident follow-up has been successfully submitted.",
      easyClose = TRUE
    ))
    
    tryCatch({
    sheet_append(data = follow_up_data, 
                 ss = "https://docs.google.com/spreadsheets/d/1Gnt5Dzjp2xs3jIlEvDsY/edit?gid=0#gid=0", 
                 sheet = "Sheet1")
    values$followup <- read_sheet("https://docs.google.com/spreadsheets/d/1Gnt5DzjplEvDsY/edit?gid=0#gid=0", sheet = "Sheet1")},# Refresh data
    error = function(e){
      print(paste("Error in sheet_append:", e$message))
      showModal(modalDialog(
        title = tagList(icon("thumbs-down"),"Submission Failed"),
        paste("Error:", e$message, "Contact Mr. Andrew or Ms. Rachel for help."),
        easyClose = TRUE
      ))
    } #function
  ) #trycatch
    
    # Reset all input fields
    updateSelectInput(session, "fu_staffname", selected = "")
    updateSelectInput(session, "fu_stuname", selected = "")
    updateSelectInput(session, "fu_incidenttype", selected = "")
    updateSelectInput(session, "fu_followupreq", selected = "")
    updateSelectInput(session, "fu_incidentlevel", selected = NULL)
    updateSelectInput(session, "fu_numincidenton", selected = NULL)
    updateTextAreaInput(session, "fu_followupdetails", value = "")
    
  })
  
  
  # Display a data table of incidents (filter by selected student)
  output$by_student <- DT::renderDT({
    #req(input$admin_stuname)  # Ensure student is selected
    values$incidents %>%
      filter(bx_stuname == input$admin_stuname)
  })
}

# Run the app
shinyApp(ui, server)
