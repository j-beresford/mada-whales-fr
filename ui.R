rm(list=ls())

source("packages.R")
source("call-data.R")
source("functions.R")
source("table_vars.R")
source("graphs.R")
source("client_info.R")


# Define UI for application that draws a histogram
fluidPage(theme=shinytheme("cerulean"),
          
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    

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
                   sidebarPanel(
                     selectizeInput("dataset", 
                                               label = h3("Select Dataset"), 
                                               choices = list("Dives",
                                                              "Shark sightings",
                                                              "Shark scar sightings",
                                                              "Megafauna sightings",
                                                              "Client information"),
                                               selected = "Shark sightings"),
                                downloadButton("downloadData", "Download"),
                                width=3),
                   mainPanel(DTOutput('table'),width=9)
                   ),
          tabPanel("Classifier Form",
                   sidebarPanel(
                     h4("Classify a sighting"),
                     selectizeInput("sighting_id", "Enter sighting ID", 
                          choices = mapUpdateUNClassified()%>%
                            pull(sighting_id),
                          options = list(
                            placeholder = 'Please type or copy/paste ID',
                            onInitialize = I('function() { this.setValue(""); }'))),
                   textInput("i3s_id",
                                    "Insert MD number"),
                   radioButtons("no_id_reason","File as:",
                                choices = list(
                                  "Done - I3S ID provided"="",  
                                  "To do - Bad photo/need advice"="advice_needed",
                                  "Discarded - Photo is unusable"="unusable_sighting"),
                                selected = ""),
                   uiOutput("error_message"),
                  actionButton("submit","Submit"),
                  hr(),
                  h4("Delete a classified sighting"),
                  selectizeInput("sighting_deletion", "Enter sighting ID", 
                               choices = mapUpdateClassified(map_classified_vars)%>%
                                 full_join(mapUpdateUnusable())%>%
                                 pull(sighting_id),
                                options = list(
                                  placeholder = 'Please type or copy/paste ID',
                                onInitialize = I('function() { this.setValue(""); }'))),
                  actionButton("delete","Delete")),
                  mainPanel(
                    h3("Unclassified sightings"),
                    checkboxInput("show_advice_needed",
                                  label = "Show advice needed",
                                  value=FALSE),
                    DTOutput("unclassified_sightings"))),
   
      navbarMenu("Sightings",
           tabPanel("Unusable",
                h3("All sightings that can't be classified"),
                h5("Sharks shown here have a sighting ID, but no photos or ws.org ID"),
                DTOutput("unusable_sightings")),
           tabPanel("Classified",
                h3("All sightings that have been classified"),
                DTOutput("classified_sightings"))
                 
      ),
      
      navbarMenu("Clean data",
         tabPanel("Summary Stats",
                  h3("Summary Statistics"),
                  h5("Unique sharks grouped by year"),
                  DTOutput("summary_stats"),
                  h5("Unique sharks grouped by week"),
                  DTOutput("summary_stats_weekly")),
         tabPanel("Unique sharks",
                  sidebarPanel(selectInput("clean_dataset", 
                                           label = h3("Select Dataset"), 
                                           choices = list("Known sharks",
                                                          "Merged yearly",
                                                          "Merged weekly",
                                                          "Merged daily")),
                               downloadButton("downloadCleanData", "Download"),
                               width=3),
                  mainPanel(DTOutput('table_clean'),width=9)),
         tabPanel("Shark scars",
                  h3("Shark scars"),
                  h5("Row per shark scar"),
                  DTOutput("clean_shark_scars"))),
      
      navbarMenu("Graphs",
       tabPanel("Trips",
        tabsetPanel(
          tabPanel("Trip counts",plotOutput('plotTrip',width="100%")),
          tabPanel("Correlations",plotOutput('plotCorrs',width="100%")))),
       tabPanel("Maps",
        tabsetPanel(
          tabPanel("Shark sightings",plotlyOutput('plotSharkMap',width="100%")),
          tabPanel("Shark density",plotlyOutput('plotSharkDensity',width="100%")),
          tabPanel("Megaf. density",plotlyOutput('plotMegafDensity',width="100%")),
          tabPanel("Megafauna",plotlyOutput('plotMegafMap',width="100%")))),
       tabPanel("Sightings",
        tabsetPanel(
          tabPanel("Sharks",plotOutput('plotSightings',width="100%")),
          tabPanel("Megafauna",plotOutput('plotMegaf',width="100%"))))
)))


