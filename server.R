
fields <- c("sighting_id", "i3s_id","no_id_reason")

# Define server logic required to draw a histogram
function(input, output, session) {

    tr <- function(text){ # translates text into current language
    sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
  }  
  
###### ABOUT ########
    
  output$about_naturedb <- renderUI(HTML("NatureDB is designed to be used in conjuction with the <a href='https://ee.kobotoolbox.org/x/PME7pT8m'>this survey</a>. It has several use cases: 
  <ul>
  <li>Raw survey outputs are available to view in near real-time.</li>
  <li>Summary statistics and data visualisation are automised and available to view in the <i>clean data</i> and <i>graphs</i> tabs.</li>
  <li>The 'classifier' lets users assign I&sup3;S ID's to shark sightings. Armed with this information, NatureDB automates the merging of  
  <dfn title='A sighting is defined as any shark registered in the survey, regardless of whether a left ID is taken'>
  <u style='text-decoration:underline dotted'>shark sightings</u></dfn>
  into 
  <dfn title='A known shark is any shark to which we have assigned an  I&sup3;S ID'>
  <u style='text-decoration:underline dotted'>known sharks.</dfn></li> 
  </ul>"))
  
  output$about_instructions <- renderUI(HTML(
    "<ol>
    <li>While at sea, fill in  <a href='https://ee.kobotoolbox.org/x/PME7pT8m'>the survey</a></li>
    <li>Check your sightings have appeared in <i>raw data</i> (Note: you can filter by your name, date, tablet etc). Results will take a few minutes to appear.</li>
    <li>Upload your photos to I&sup3;S and, where an ID available, make a note of the I&sup3;S ID for each of your sightings.</li>
    <li>Go to <i>classifier</i> and file each of your sightings as 'done', 'advice needed' or 'unusablke.</li>
    <li>Check that your sighting information appears in the <i>classified sightings</i> tab s well as in the <i>clean data</i> tabs</li>
    </ol>"
  ))
  
  
  ############### Raw Data download ###############
  ## Raw data Table selection
  datasetInput <- reactive({
    switch(input$dataset,
           "Dives" = displayTrip(trip_vars),
           "Shark sightings" = displaySharkSightings(shark_sightings_vars),
           "Shark scar sightings" = displaySharkScars(shark_scar_vars),
           "Megafauna sightings" = displayMegaf(megaf_vars),
           "Client information" = df_client
    )
  })
  
  ## Raw data - Show table (main panel)
  output$table <- renderDT(
    {datasetInput()},
    filter = "top",
    options = list(
      pageLength = 10,
      scrollX=TRUE,
      scrollY=TRUE
    )
  )
  
  ## Raw data - Download CSV button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
 ######## Classifier form ############
 
  updateSelectizeInput(session,"sighting_id",
                       choices = mapUpdateUNClassified()%>%
                         pull(sighting_id),selected = NA,server = T)
  
  
  observeEvent(input$submit,{
   
    if (input$sighting_id=="" && input$i3s_id=="") {
      output$error_message<-renderUI({
          HTML(as.character(div(style="color: red;",
          "Required: sighting ID and I3S ID")))})
    } else if (input$sighting_id %in% is_not_allowed()$sighting_id)  {
      output$error_message<-renderUI({
          HTML(as.character(div(style="color: red;",
          "Sighting ID has already been mapped")))})
      updateSelectizeInput(session,"sighting_id",
                           choices = mapUpdateUNClassified()%>%
                             pull(sighting_id))
      updateTextInput(session,"i3s_id",value = "")
      
    } else if (input$no_id_reason %in% c("advice_needed","unusable_sighting") && 
               input$i3s_id!="") {
      output$error_message<-renderUI({
          HTML(as.character(div(style="color: red;",
          "If I3S ID available, sighting should be marked 'done'.")))})
      updateSelectizeInput(session,"sighting_id",
                           choices = mapUpdateUNClassified()%>%
                             pull(sighting_id))
      updateTextInput(session,"i3s_id",value = "")
      
    } else if (!input$no_id_reason %in% c("advice_needed","unusable_sighting") &&
               input$i3s_id=="") {
      output$error_message<-renderUI({
        HTML(as.character(div(style="color: red;",
                              "No I3S ID is given but sighting filed as done.")))})
      updateSelectizeInput(session,"sighting_id",
                           choices = mapUpdateUNClassified()%>%
                             pull(sighting_id))
      updateTextInput(session,"i3s_id",value = "")
      showNotification("Error",type="error")      
    } else {    
      new_row<-data.frame(
        sighting_id=input$sighting_id,
        i3s_id=input$i3s_id,
        no_id_reason=input$no_id_reason)
    
      updateTextInput(session,"i3s_id",value = "")
      
      previous<-s3readRDS(object = "map.rds", bucket = "mada-whales")
      mapping<-rbind(previous,new_row)
      s3saveRDS(x = mapping, bucket = "mada-whales", object = "map.rds")
      
      updateSelectizeInput(session,"sighting_id",
                           choices = mapUpdateUNClassified()%>%
                             pull(sighting_id))
      updateSelectizeInput(session,"sighting_deletion", 
                           choices = mapUpdateClassified(map_classified_vars)%>%
                             full_join(mapUpdateUnusable())%>%
                             pull(sighting_id))
      showNotification("Submitted.",type="message")
      }
  })
  
  observeEvent(input$delete, {
    showModal(modalDialog(
      p("Are you sure you want to delete this sighting?"),
      p("Êtes-vous sûr de vouloir supprimer cette observation?"),
      title="Warning / Attention !",
      footer = tagList(actionButton("confirmDelete", "Delete",
                  style="color: #f54257; border-color: #f54257"),
                       modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirmDelete, {
    delete_sighting(input$sighting_deletion)
    removeModal()
    updateSelectizeInput(session,"sighting_deletion", 
                         choices = mapUpdateClassified(map_classified_vars)%>%
                           full_join(mapUpdateUnusable())%>%
                           pull(sighting_id))
    showNotification("Sighting deleted.",type="error")
    updateSelectizeInput(session,"sighting_id",
                         choices = mapUpdateUNClassified()%>%
                           pull(sighting_id))
    output$unclassified_sightings <- renderDataTable({
      input$submit
      if (input$show_advice_needed==TRUE){
        uc<-mapUpdateUNClassified()%>%
          filter(no_id_reason=="advice_needed")
      } else {
        uc<-mapUpdateUNClassified()%>%
          filter(!no_id_reason %in% c("advice_needed"))
      }
      uc},
      options = list(scrollX=TRUE,scrollY=TRUE, scrollCollapse=TRUE),filter="top"
    )  
  })
  
  ############# Sightings (class, unclass, unusable) ##########
  
  ## Show table (unknown)

  output$unclassified_sightings <- renderDataTable({
    input$submit
    if (input$show_advice_needed==TRUE){
      uc<-mapUpdateUNClassified()%>%
        filter(no_id_reason=="advice_needed")
    } else {
      uc<-mapUpdateUNClassified()%>%
        filter(!no_id_reason %in% c("advice_needed"))
    }
    uc},
    options = list(scrollX=TRUE,scrollY=TRUE, scrollCollapse=TRUE),filter="top"
  )
  
  ## Show table (unusable sightings)
  output$unusable_sightings <- renderDataTable({
    input$submit
    mapUpdateUnusable()},
    options = list(scrollX=TRUE,scrollY=TRUE, scrollCollapse=TRUE),filter="top"
  )
  
  ## Show table (known)
  output$classified_sightings <- renderDataTable({
    input$submit
    mapUpdateClassified(map_classified_vars)},
    options = list(scrollX=TRUE, scrollCollapse=TRUE),filter="top"
  )
  
  ############### Clean data download ###############
  
  output$summary_stats<-renderDataTable({
    get_summary_stats(mapUpdateKnownSharks())},
    options = list(scrollX=TRUE, scrollCollapse=TRUE)
  )
  
  output$summary_stats_weekly<-renderDataTable({
    get_summary_stats_weekly(mapUpdateUniqueWeeklySightings())},
    options = list(scrollX=TRUE, scrollCollapse=TRUE)
  )
  
  
  ## Table selection
  cleanDatasetInput <- reactive({
    switch(input$clean_dataset,
           "Known sharks" = mapUpdateKnownSharks(),
           "Merged yearly" = mapUpdateUniqueYearlySightings(),
           "Merged weekly" = mapUpdateUniqueWeeklySightings(),
           "Merged daily" = mapUpdateUniqueTripSightings()
    )
  })
  
  ## Show table (main panel)
  output$table_clean <- renderDT(
    {cleanDatasetInput()},
    filter = "top",
    options = list(
      pageLength = 10,
      scrollX=TRUE
    )
  )
  
  
  ## Download CSV
  output$downloadCleanData <- downloadHandler(
    filename = function() {
      paste(input$clean_dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cleanDatasetInput(), file, row.names = FALSE)
    }
  )
  
  ##### Shark Scars #########
  
  ### Graphs ####
  output$clean_shark_scars <- renderDT(
    {mapUpdateScars()},
    filter = "top",
    options = list(
      pageLength = 10,
      scrollX=TRUE
    )
  )
  
  
  output$plotTrip <- renderPlot({
    print(daily_dives_sst)
  })
  
  output$plotSightings <- renderPlot({
    print(sightings_sex)
  })
  
  output$plotSharks <- renderPlot({
    print(sharks_by_size_sex)
  })
  

  output$plotNewSharkRate <- renderPlot({
    print(new_sharks_by_year)
  })
  
  output$plotMegaf <- renderPlot({
    print(megaf_all)
  })
  
  output$plotCorrs <- renderPlot({
    print(distributions)
  })
  
  output$plotMegafMap <- renderPlotly({
    print(megaf_map)
  })
  
  output$plotSharkMap <- renderPlotly({
    print(shark_map)
  })
  
  output$plotMegafDensity <- renderPlotly({
    print(megaf_density)
  })
  
  output$plotSharkDensity <- renderPlotly({
    print(shark_density)
  })
  
  
}

