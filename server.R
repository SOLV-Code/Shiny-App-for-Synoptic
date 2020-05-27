# Server funtion for the State of the Salmon Synoptic Status Evaluation Tool
# Developed by B. Dorner and B. MacDonald
# Mach, 2020

#------------------------ Setup -----------------------

list.of.packages <- c("shiny",
                      "shinydashboard",
                      "shinydashboardPlus",
                      "shinyjs",
                      "tibble",
                      "devtools",
                      "ggplot2",
                      "dplyr",
                      "GGally",
                      "DT",
                      "htmltools",
                      "shinyWidgets",
                      "plotly",
                      "forcats",
                      "rsconnect",
                      "brew",
                      "ini",
                      "xfun",
                      "readxl",
                      "markdown",
                      "crosstalk",
                      "rgdal",
                      "sp",
                      "leaflet",
                      "leaflet.extras",
                      "shinyBS",
                      "shinycssloaders",
                      "sparkline",
                      "mapview", 
                      "leafem",
                      "blastula",
                      "parcoordsSoS")
# 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
# NOTE: to make this work with shinyapps.io, you MUST install parcoordsSoS from the github repo, i.e.
# by using install_github("brigitte-dorner/parcoordsSoS") before uploading.
# Downloading the code and building parcoordsSoS locally will let you run the app
# locally, but the install on shinyapps.io will fail unless all required packages
# are installed from CRAN or github.

# helper functions
source('helpers.R')

# ==========Define server components ================

# Define server logic 
function(input, output, session){
  source('DataFilters.R', local=TRUE)
  source('SharedComponents.R', local=TRUE)
  source('Map.R', local=TRUE)
  source('ParCoords.R', local=TRUE)
  source('TSPlots.R', local=TRUE)
  source('DataTable.R', local=TRUE)
  source('HistogramSummary.R', local=TRUE)
  
  observeEvent(input$contact_Btn, {
    showModal(
      modalDialog(
        includeMarkdown("Markdown/contact.md"),
        wellPanel(style = WellPanelStyle,
        textInput(inputId = "contact_Name", label = NULL, placeholder = "Your name", width = '100%'),
        textInput(inputId = "contact_Email", label = NULL, placeholder = "Your email", width = '100%'),
        textAreaInput(inputId = "contact_Msg", label = NULL, placeholder = 'Your message', width = '100%', rows = '10'),
        actionButton(inputId = "contact_Send", "Send")),
        footer = modalButton('Close'),
        size = 'l'))
  }) 
  
  observeEvent(input$contact_Send, {
    emailMsg <- blastula::compose_email(body=htmlEscape(input$contact_Msg))
    # send the email
    blastula::smtp_send(email = emailMsg,
              from = SoS_email,
              to = SoS_email,
              subject = paste0(input$contact_Name, ' (', input$contact_Email, ')', ' via SSET contact form'),
              credentials = creds_file('gmail_credentials')
#             verbose = TRUE # turn on for debugging
            )
    # clear the form
    updateTextInput(session, inputId = "contact_Name", value = "", placeholder = "Your name")
    updateTextInput(session, inputId = "contact_Email", value = "", placeholder = "Your email")
    updateTextAreaInput(session, inputId = "contact_Msg", value = "", placeholder = 'Your message')
    # let user know message went
    showNotification("Your message was sent successfully")
  })
} # end server function
#   


