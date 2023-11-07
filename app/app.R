library(shiny)
library(shinyjs)
library(dplyr)
library(reactlog)

reactlog_enable()


# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Tinder-like Dating App"),
  
  mainPanel(
    
    # Questionnaire part
    tags$div(id = "quest", 
             
             radioButtons(inputId = "gender", 
                          label = "I am a...", 
                          choices = c("Man", "Woman"),
                          selected = character(0)),
             
             radioButtons(inputId = "int_in", 
                          label = "I am romantically interested in...",
                          choices = c("Men" = "M", "Women" = "F"),
                          selected = character(0)),
             
             textInput(inputId = "prolific_id", 
                       label = "Please submit your Prolific ID"),
             
             ## TODO: other fields
             
             actionButton("submit", "Submit!")
             
             ),
    
    tags$div(id = "cards")
  ) 
   
)

# Define server
server <- function(input, output) {
  
  ## assign condition randomly
  # TODO: Add condition for visual
  condition <- sample(0:1, 1)
  
  
  # hide questionnaire, show cards after submitting input
  # TODO: should check that all fields filled in before allowing this
  # maybe change inputs to shinyforms
  
 # profiles <- read.csv("profiles.csv") 
  
  observeEvent(input$submit, {
    ## TODO: Save the user-level info in DB
    
    #print(profiles)
    profiles <- read.csv("profiles.csv")|> filter(gender %in% input$int_in)
    print(profiles)

    # hide questionnaire
    removeUI(selector = "#quest")
    
    # get id of current sampled profile
    # has to be reactive to be updated after clicking
    currentProfile <- reactiveVal(
      sample(profiles$profile_id, 1)
    )
    
    
    currentEduc <- reactiveVal(
      sample(c("Lower", "Medium", "High"), 1)
    )
    
    # TODO: Add condition for visual
    output$condition_display <- reactive(
      ifelse(condition == 0, "", paste("This profile is considered", profiles$attractive[currentProfile()], "attractive"))
    )
    
    # TODO: current id should be stored in a vector and appended each iteration, so
    # they are not repeated
    
    # Get photo of sampled profile
    output$profileImage <- renderImage({
      list(src = paste0("imgs/", profiles$photo[currentProfile()]),
           width = "90%")
    }, deleteFile = F)
    
    # Get age of sampled profile (has to match age)
    output$profileAge <- renderText({
      paste("Age:", profiles$age[currentProfile()])
    })
    
    output$profileEduc <- renderText({
      paste("Education:", currentEduc())
    })
    
    
    output$profileGender <- renderText({
      paste("Gender:", profiles$gender[currentProfile()])
    })
    
    # Function that updates profile after clicking
    # TODO: Profiles should not repeat, there should be a vector storing each,
    # then restricting the sample + when end, the app should stop
    updateProfile <- function() {
      # clunky but works
      # TODO: Make 
      newProfile <- sample(profiles$profile_id, 1)
      currentProfile(newProfile)
      newEduc <- sample(c("Lower", "Medium", "High"), 1)
      currentEduc(newEduc)
      print(currentProfile)
    }
    
    
    
    saveData <- function(success) {
      i <- currentProfile()
      # Record the choice in a CSV file
      choice <- data.frame(
        prolific_id = input$prolific_id, 
        id = profiles$profile_id[i],
        url = profiles$photo[i],
        age = profiles$age[i],
        education = currentEduc(),
        success = success,
        time = Sys.time()
      )
      write.csv(choice, "choices.csv", append = TRUE, row.names = FALSE)
    }
    
    
    
    # Action when the "Success" button is clicked
    observeEvent(input$successButton, {
      saveData(success = TRUE)
      # update profile after saving data
      updateProfile()
    })
    
    # Action when the "Fail" button is clicked
    observeEvent(input$failButton, {
      saveData(success = FALSE)
      # update profile after saving data
      updateProfile()
    })
    
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
        tags$div(
          id = "profileInfo", style = "font-size: 18px",
          textOutput("profileGender")
        ),
        tags$p(
          id = "attr",
          textOutput("condition_display")
        ),

        # Buttons for success and fail
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
