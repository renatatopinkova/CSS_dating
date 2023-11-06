library(shiny)
library(shinyjs)
library(shinyalert)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Tinder-like Dating App"),
  
  # Main Tinder-like card display area
  mainPanel(
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
    tags$div(id = "profileInfo", style = "font-size: 18px",
             textOutput("profileAge")),
    tags$div(id = "profileInfo", style = "font-size: 18px",
             textOutput("profileEduc")),
    
    # Buttons for success and fail
    tags$div(
      id = "buttonGroup",
      style = "display: flex; justify-content: space-around; margin-top: 20px;",
      actionButton("successButton", "Success", class = "btn-success"),
      actionButton("failButton", "Fail", class = "btn-danger")
    )
  )
)

# Define server
server <- function(input, output) {
  
  
  
  profiles <- read.csv("profiles.csv")
  
  # get id of current sampled profile
  # has to be reactive to be updated after clicking
  currentProfile <- reactiveVal(
    sample(profiles$profile_id, 1)
    )
  
  currentEduc <- reactiveVal(
    sample(c("Lower", "Medium", "High"), 1)
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
  
  # TODO: education should be reactive too 

  output$profileEduc <- renderText({
    paste("Education:", currentEduc())
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
  }
  
  saveData <- function(success) {
    i <- currentProfile()
    # Record the choice in a CSV file
    choice <- data.frame(
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
  
}  


# Run the Shiny app
shinyApp(ui, server)
