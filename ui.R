rm(list=ls())


source("packages.R")
source("call-data.R")
source("functions.R")
source("table_vars.R")

# Define UI for application that draws a histogram
fluidPage(theme=shinytheme("cerulean"),

          navbarPage(title = "MSWP Tableau de Bord",collapsible = TRUE,

           navbarMenu("About",
                      tabPanel("Instructions",
                               h3("The dashboard"),
                               uiOutput("about_naturedb"),
                               h3("Instructions"),
                               uiOutput("about_instructions"),
                               img(src="requin-baleine.jpg",width="100%")),
                      tabPanel("Field Collect",
                               h3("Field Collect"),
                               uiOutput("about_fieldcollect")),
             ),
          
          tabPanel("Raw Data",h3("View and download raw data"),
                   sidebarPanel(selectizeInput("dataset", 
                                               label = h3("Select Dataset"), 
                                               choices = list("Dives",
                                                              "Shark sightings",
                                                              "Shark scar sightings",
                                                              "Megafauna sightings"),
                                               selected = "Shark sightings"),
                                downloadButton("downloadData", "Download"),
                                width=3),
                   mainPanel(DTOutput('table'),width=9)
                   )
))

