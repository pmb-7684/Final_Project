#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
theme1 <- bs_theme(
  bg = "#0b3d91", fg = "white", primary = "#FCC780",
  base_font = font_google("Space Mono"),
  code_font = font_google("Space Mono")
)


# Define UI for application that draws a histogram
ui <- navbarPage(theme = theme1, "App Title",
           tabPanel("About"),
           tabPanel("Data Exploration"),
           navbarMenu("Modeling",
                      tabPanel("Model Info"),
                      "----",
                      tabPanel("Model fitting"),
                      "----",
                      tabPanel("Prediction"),
           ),
           tabPanel("Data Exploration")
)

# Define server logic 
server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)
