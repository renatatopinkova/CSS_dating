library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyvalidate)
library(dplyr)
library(DBI)
library(RSQLite)
# library(reactlog)
# 
# reactlog_enable()


# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Tinder-like Dating App"),
  mainPanel(
    
    # adding custom stylesheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    # Questionnaire part
    tags$div(
      id = "quest",
      radioButtons(
        inputId = "gender",
        label = "I am a...",
        choices = c("Man" = "M", "Woman" = "F"),
        selected = character(0)
      ),
      radioButtons(
        inputId = "int_in",
        label = "I am romantically interested in...",
        choices = c("Women" = "F", "Men" = "M"),
        selected = character(0)
      ),
      
      numericInput(
        "age",
        "What is your age?",
        value = numeric(0),
        min = 18,
        max = 99,
      ),
      
      textInput(
        inputId = "prolific_id",
        label = "Please submit your Prolific ID"
      ),

      ## TODO: other fields

      disabled(actionButton("submit", "Submit!"))
    ),
    tags$div(id = "cards")
  )
)

# Define server
server <- function(input, output) {
  
  # Assign condition randomly
  # TODO: Add condition for visual
  condition <- sample(0:1, 1)
  
  
  # Database settings 
  table <- "users"
  table_choices <- "choices"
  sqlitePath <-  "db/responses.db"
  
  
  # Define function to Save user data
  
    saveDataUsers <- function(data) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table, 
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbSendStatement(db, query)
    dbDisconnect(db)
  }
  

    # Prepare user data for db
    formDataUsers <- reactive({
      data <- sapply(c(fieldsMandatory, "age"), function(x)
        input[[x]])
      data <- c(data, date = as.POSIXct(Sys.time()),
                condition = condition)
    })
    

  # Questionnaire  
  # Field validations
  
  # Set up validation rule
  iv <- InputValidator$new()
  iv$add_rule("age", sv_between(18, 99))
  
  # defer validation of age field so it's displayed only after user inputs some number
  observeEvent(req(is.numeric(input$age)), {
               iv$enable()})
  

  # Remaining input fields
  fieldsMandatory <- c("gender", "int_in", "prolific_id")
  
  observe({
    # Function that checks whether all input fields in questionnaire were filled
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },  logical(1))
    
    # verify valid age
    ageFilled <- input$age >= 18 & input$age < 99
    
    mandatoryFilled <- all(c(mandatoryFilled, ageFilled))
    
    # Toggle (activate) submit button when all fields are filled
    toggleState(id = "submit", condition = mandatoryFilled)
    
    
  })

  
  # Observe Submit button trigger 
  observeEvent(input$submit, {
    
    # Save user data to db
    saveDataUsers(formDataUsers())
    
    # Load in profile data
    profiles <- read.csv("profiles_cfd.csv") |>
      # filter based on user input
      filter(gender %in% input$int_in) |>
      # sample out N 
      sample_n(20, replace = FALSE)
    
    print(profiles)
    print(profiles[2, ])

    # hide questionnaire
    removeUI(selector = "#quest")
    
    
    # Generate profile information

    # get id of current sampled profile
    # has to be reactive to be updated after clicking
    currentProfile <- reactiveVal(
      profiles[1, ]
    )

    # Display exp. condition
    # TODO: Add condition for visual
    output$condition_display <- reactive(
      ifelse(condition == 0, "", paste("This profile is considered", currentProfile()$attr_level, "attractive"))
    )


    # Get photo of sampled profile
    output$profileImage <- renderImage(
      {
        list(
          src = paste0("imgs/", currentProfile()$photo),
          width = "90%"
        )
      },
      deleteFile = F
    )

    # Get age of sampled profile (has to match age)
    output$profileAge <- renderText({
      paste("Age:", currentProfile()$age)
    })

    
    # Randomize education level
    currentEduc <- reactiveVal(
      sample(c("Lower", "Medium", "High"), 1) # initial value
    )
    
    # Render education
    output$profileEduc <- renderText({
      paste("Education:", currentEduc())
    })
    

    # Function that updates profile after clicking
    # Set up counter
    counter <- reactiveVal(1)

    updateProfile <- function() {
      
      # Update counter
      newCounter <- counter() + 1
      counter(newCounter)

      # check how far (how many profiles were viewed) in the app
      if (counter() <= 20) {
        
        # select next row of sampled profiles
        newProfile <- profiles[counter(), ]
        currentProfile(newProfile)
        
        # update education level (randomized)
        newEduc <- sample(c("Lower", "Medium", "High"), 1)
        currentEduc(newEduc)
        
        # break after reaching last sampled profile
      } else {
        
        shinyalert("Thank you!", "You have finished all tasks. 
                   Please note the completion code ##### and then close this website.", 
                   type = "success",
                   closeOnEsc = FALSE,
                   closeOnClickOutside = FALSE,
                   showConfirmButton = FALSE)
        
      }
    }

    
    
    # Forming choice data

    formDataChoices <- reactive({
      data <- tibble(prolific_id = input$prolific_id, 
                     id = currentProfile()$profile_id,
                     url = currentProfile()$photo,
                     age = currentProfile()$age,
                     education = currentEduc(),
                     time = as.POSIXct(Sys.time()))
    })
    
    # Save choice data into db
    
    saveDataChoices <- function(data, success) {
      
      # Connect to the database
      db <- dbConnect(SQLite(), sqlitePath)
      # Construct the update query
      query <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s')",
        table_choices, 
        paste(c(names(data), "success"), collapse = ", "),
        paste(c(data, success), collapse = "', '")
      )
      print(query)
      # Submit the update query and disconnect
      dbSendStatement(db, query)
      dbDisconnect(db)
    }

   


    # Action when the "Success" button is clicked
    observeEvent(input$successButton, {
      saveDataChoices(formDataChoices(), success = TRUE)
      # update profile after saving data
      updateProfile()
    })

    # Action when the "Fail" button is clicked
    observeEvent(input$failButton, {
      saveDataChoices(formDataChoices(), success = FALSE)
      # update profile after saving data
      updateProfile()
    })
    
    
    
    ### UI part of the cards-part

    # show app
    insertUI(selector = "#cards", ui = tagList( # App part
      tags$div(
        width = 4,
        offset = 3,
        style = "text-align: center; 
              border-style:solid; 
              border-color: lightgray; 
              border-radius: 30px; 
              padding: 5%;
              margin: 5% 30% 5% 30%",

        # Display profile image
        imageOutput("profileImage"),

        # Display age and education
        tags$div(
          id = "profileInfo", style = "font-size: 18px",
          textOutput("profileAge")
        ),
        tags$div(
          id = "profileInfo", style = "font-size: 18px",
          textOutput("profileEduc")
        ),
        # tags$div(
        #   id = "profileInfo", style = "font-size: 18px",
        #   textOutput("profileGender")
        # ),
        tags$p(
          id = "attr",
          textOutput("condition_display")
        ),

        # Buttons for success and fail
        # TODO: style 
        tags$div(
          id = "buttonGroup",
          style = "display: flex; justify-content: space-around; margin-top: 20px;",
          actionButton("successButton", "Success", class = "btn-success"),
          actionButton("failButton", "Fail", class = "btn-danger")
        )
      )
    ))
  })
}


# Run the Shiny app
shinyApp(ui, server)
