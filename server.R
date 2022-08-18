# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$about_naturedb <- renderUI(HTML(
    "NatureDB is designed to be used in conjuction with the <a href='https://ee.kobotoolbox.org/x/PME7pT8m'>this survey</a>. It has several use cases: 
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
  
  
}
