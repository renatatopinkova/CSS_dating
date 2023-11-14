library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyvalidate)
library(dplyr)
library(DBI)
library(RSQLite)


# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("  "),
  mainPanel(
    
#    adding custom stylesheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    # Questionnaire part
    tags$div(
      id = "quest",
      
      p("Please fill in the following questionnaire and enter your Prolific ID."), 
        
      p("After submitting your information, you will view a series of online dating profiles. 
        For each profile, we ask you to evaluate whether you would be interested in pursuing the individual featured (by pressing 💚) or not (by pressing ❌ ). After you have rated 30 profiles, your completion code will be displayed."), 
        
      p(strong("! Please do not refresh this website beforehand, otherwise the rating counter will be reset to 0.")),
      
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
      
      # select inputs created empty to avoid defaulting to first value when not selected
      selectInput(
        "ethnicity",
        "Which race or ethnicity best describes you?",
        choices = "",
        selected = NULL,
        multiple = FALSE,
      ),
      
      selectInput("education",
                  "What is the highest level of education you have completed?",
                  choices = "",
                  multiple = FALSE),
      
      radioButtons("relationship",
                   "Are you currently in a committed romantic relationship?",
                   choices = c("Yes, I am in a committed romantic relationship" = "yes",
                               "No, I am not in a committed romantic relationship" = "no"),
                   selected = character(0)),
      
      selectInput("dating_ever",
                  "Have you ever used an online dating site or dating app?",
                  choices = "",
                  selected = NULL,
                  selectize=TRUE),
      
      # Filter for online dating experience == YES
      conditionalPanel(condition = "input.dating_ever.startsWith('yes')",
                       
                       radioButtons("dating_exp", 
                                    "Overall, would you say your OWN personal experiences with online dating sites or dating apps have been…",
                                    choices = c("Very positive", "Somewhat positive", "Somewhat negative", "Very negative"),
                                    inline=TRUE, 
                                    selected = character(0)),
                       
                       radioButtons("dating_paid",
                                    "Have you ever paid to use an online dating site or dating app, including for extra features on that site or app?",
                                    choices = c("Yes, I have done this", "No, I have not done this"),
                                    selected = character(0))),
      
      
      textInput(
        inputId = "prolific_id",
        label = "Please submit your Prolific ID"
      ),

      ## TODO: other fields

      disabled(actionButton("submit", "Submit!"))
    ),
  
    
    tags$div(id = "cards", style = "margin-left: 20%; max-width: 300px")
  #  tags$div(id = "instr_btn", style = "margin-left: 30%")
  )
)

# Define server
server <- function(input, output, session) {
  
  # update select inputs (fill with choice values)
  updateSelectInput(session, "dating_ever", 
                    choices = c("No" = "no", 
                    "Yes, I am currently using online dating site or dating app" = "yes_current", 
                    "Yes, I have used online dating site or dating app in the past" = "yes_past"), 
                    selected = "")
  
 
  updateSelectInput(session, "ethnicity",
                    choices =  c("Asian", "Black", "Hispanic", "White", "Multiple ethnicity", "Other"),
                    selected = "")
  
  updateSelectInput(session, "education",
                    choices = c("Less than high school",
                                "High school",
                                "College or University"),
                    selected = "")
  
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
                condition = condition,
                # conditional values export
                dating_exp = ifelse(input$dating_ever %in% c("yes_current", "yes_past"), input$dating_exp, NA_character_),
                dating_paid = ifelse(input$dating_ever %in% c("yes_current", "yes_past"), input$dating_paid, NA_character_))
    })
    

  # Questionnaire  
  # Field validations
  
  # Set up validation rule
  iv <- InputValidator$new()
  iv$add_rule("age", sv_between(18, 99, message_fmt = "Please enter valid age."))
  
  # defer validation of age field so it's displayed only after user inputs some number
  observeEvent(req(is.numeric(input$age)), {
               iv$enable()})
  

  # Remaining input fields
  fieldsMandatory <- c("gender", "int_in", "prolific_id", "ethnicity", "dating_ever", "relationship", "education")
  fieldsConditionals <- c("dating_exp", "dating_paid")
  
  observe({
    # Function that checks whether all input fields in questionnaire were filled
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },  logical(1))
    
    # verify valid age
    ageFilled <- input$age >= 18 & input$age < 99
    
    # verify conditionals
    conditionalsFilled <- ifelse(input$dating_ever %in% c("yes_current", "yes_past"),
                           # if experience with online dating, validate fields
                           vapply(fieldsConditionals,
                                  function(x) {
                                    !is.null(input[[x]]) && input[[x]] != ""
                                  },  logical(1)), 
                           # if no experience with online dating or not selected yet, 
                           # validate as true
                           TRUE)
    

    # print(paste("Conditionals filled", conditionals))
    
    mandatoryFilled <- all(c(mandatoryFilled, ageFilled, conditionalsFilled))
    
    # Toggle (activate) submit button when all fields are filled
    toggleState(id = "submit", condition = mandatoryFilled)
    
    
  })

  output$heart <- renderImage(
    {
      list(
        src = paste0("www/", "heart.png")
      )
    },
    deleteFile = F
  )
  
  # Observe Submit button trigger 
  observeEvent(input$submit, {
    
    # Save user data to db
    saveDataUsers(formDataUsers())
    
    # Load in profile data
    profiles <- read.csv("profiles_cfd_filtered.csv") |>
      # filter based on user input
      filter(gender %in% input$int_in) |>
      # sample out N 
      sample_n(30, replace = FALSE)
    
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
      ifelse(condition == 0, "", paste("This profile has a", currentProfile()$attr_level, "popularity rating, indicating that", 
                                       ifelse(currentProfile()$attr_level == "high", "many",
                                              ifelse(currentProfile()$attr_level == "medium", "some", "not many")),
                                       "other users pursued the profile."))
    )
    
    
    # Get photo of sampled profile
    output$profileImage <- renderImage(
      {
        list(
          src = paste0("imgs_filtered/", currentProfile()$photo),
          width = "100%"
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
      sample(c("Less than high school", "High school", "University"), 1) # initial value
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
      if (counter() <= 30) {
        
        # select next row of sampled profiles
        newProfile <- profiles[counter(), ]
        currentProfile(newProfile)
        
        # update education level (randomized)
        newEduc <- sample(c("Less than high school", "High school", "University"), 1)
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
    
    
    observeEvent(input$instructions, {
      shinyalert("Your choices",
                 "💚 Green Heart - If you're interested in pursuing the individual.
                  ❌ Red Cross - If you're not interested in pursuing the individual.", 
                 type = "info")
      }
    )
    
    
    ### UI part of the cards-part

    # show app
    insertUI(selector = "#cards", ui = tagList( # App part
      
     

      tags$div(
        id = "main_card",
        style = "text-align: center; 
              border; border-style:solid; 
              border-color: black; 
              border-radius: 35px;
              border-width: 5px;
              padding: 5%",
        

        # Display profile image
        tags$div(style = "padding: 5%",
          imageOutput("profileImage")),

        # Display age and education
        tags$div(
          id = "profileInfo", style = "font-size: 18px",
          textOutput("profileAge")
        ),
        tags$div(
          id = "profileInfo", style = "font-size: 18px",
          textOutput("profileEduc")
        ),

    
        tags$p(
          id = "attr",
          style = "font-size: 16px",
          textOutput("condition_display")
        ),

        # Buttons for success and fail
        # TODO: style 
        tags$div(
          id = "buttonGroup",
          style = "display: flex; justify-content: space-around; margin-top: 10px; padding-bottom:5%",
          actionButton("successButton", "", class = "button-heart"),
          actionButton("failButton", "", class = "button-cross")
        )
      
      
      ),
      tags$div(
        style = "display: flex; justify-content: center; padding: 5%",
        actionButton("instructions", "Instructions", class="btn-info",)
      )
      
      
    ))
  })
}


# Run the Shiny app
shinyApp(ui, server)
