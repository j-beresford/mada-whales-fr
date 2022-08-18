rm(list=ls())
source("packages.R")
source("login-deets.R")
source("call-data.R")

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
                     ))
)

