shinyUI(fluidPage(theme = "yeti.css", #sandstone #slate good until final DT output # united good except orange
                  tagList(
                    singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
                    singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))
                  ), 
                  navbarPage('Ecoregion 69 Biological Condition Gradient',
                             tabPanel('About',fluidRow(column(10,
                                                              h5("This app was created to run the Biological Condition 
                                                                 Gradient (BCG) model developed for EPA Level III
                                                                 Ecoregion 69- Central Appalachians by Jen Stamp, Ben Jessup, and
                                                                 Erik Leppo (Tetra Tech)."),
                                                              h5("To run this application, follow the on screen prompts
                                                                 on each tab. Proceed from down the navigation bar under either the Fish or Macroinvertebrate
                                                                 navigation bar to run the appropriate BCG model, moving from 
                                                                 'Data Upload' to 'Subbasin Connection' (in the case of the Fish model), and finally to
                                                                 'BCG Model Results.'"),
                                                              h5("Questions regarding the fish model applicability and usage should
                                                                 be directed to Jason Hill (jason.hill@deq.virginia.gov) and
                                                                 Lou Reynolds (reynolds.louis@epa.gov). For macroinvertebrate model questions, 
                                                                 contact Greg Pond (pond.greg@epa.gov). Please contact Emma Jones 
                                                                 (emma.jones@deq.virginia.gov) for all questions regarding app troubleshooting.")))),
                             navbarMenu('Fish Model',
                                        tabPanel('Fish Data Upload',
                                                 sidebarPanel(
                                                   h4(strong('Fish Model')),
                                                   h5(strong('Instructions:')),
                                                   p("Please upload site taxa list(s) in as a flat file (.csv). All data fed into the model must be formatted correctly.
                                                     If you are unsure whether your data is in the correct format, please download the 
                                                     'fishtemplate.csv' file first to check your data structure."),
                                                   hr(),
                                                   downloadButton('downloadTemplate',"Download fishtemplate.csv"),
                                                   fileInput('sites','Upload Sites (flat file)',accept='.csv',width='100%')),
                                                 mainPanel(tableOutput('inputTable'))
                                                 ),
                                        tabPanel('Fish Subbasin Connection',
                                                 sidebarPanel(
                                                   h4(strong('Fish Model')),
                                                   h5(strong('Instructions:')),
                                                   p("Press 'Connect SubBasins' button to link uploaded data to subbasin geospatial file. Review all uploaded sites to ensure they are plotting in the correct location. An 
                                          appropriate model will be selected based upon each site's latitude and longitude. If
                                          all information is correct, proceed to the 'Fish BCG Model Results' under the 'Fish Model' drop down menu."),
                                                   actionButton("connectSubB","Connect SubBasins")),
                                                 mainPanel(# conditional panel? make them upload data first?
                                                   p('Click on sites and basins for further information.'),
                                                   p('Review the table below to ensure all sites are associated with the correct subbasin.'),
                                                   leafletOutput('Map'),
                                                   tableOutput('stationsWithSubbasins'))
                                        ),
                                        tabPanel('Fish BCG Model Results',
                                                 sidebarPanel(
                                                   h4(strong('Fish Model')),
                                                   h5(strong('Instructions:')),
                                                   p("Once you have ensured the sites are plotting in the correct Subbasin, click 'Run BCG Model' to run the model. 
                                                     When the calculations are complete, results will be displayed in a table in the main panel. A progress bar on 
                                                     the bottom of the screen will indicate computational progress. You may download a flat file of results by clicking 
                                                     the 'Download Results' button."),
                                                   actionButton("runModel","Run BCG Model"),
                                                   br(),
                                                   hr(),
                                                   downloadButton("downloadResults","Download Results")),
                                                 mainPanel(
                                                   h4(strong('BCG Model Results'),
                                                      DT::dataTableOutput('BCGresults'))))),
                             navbarMenu('Macroinvertebrate Model',
                                        tabPanel('Macroinvertebrate Data Upload',
                                                 sidebarPanel(
                                                   h4(strong('Macroinvertebrate Model')),
                                                   h5(strong('Instructions:')),
                                                   p("Please upload site taxa list(s) in as a flat file (.csv). All data fed into the model must be formatted correctly.
                                                     If you are unsure whether your data is in the correct format, please download the 
                                                     'bugtemplate.csv' file first to check your data structure."),
                                                   hr(),
                                                   downloadButton('downloadTemplate_Bug',"Download bugtemplate.csv"),
                                                   fileInput('sites_Bug','Upload Macroinvertebrate Sites (flat file)',accept='.csv',width='100%')),
                                                 mainPanel(tableOutput('inputTable_Bug'))),
                                        tabPanel('Macroinvertebrate BCG Model Results',
                                                 sidebarPanel(
                                                   h4(strong('Macroinvertebrate Model')),
                                                   h5(strong('Instructions:')),
                                                   p("Click 'Run BCG Model' to run the model. 
                                                     Once the calculations are complete, results will be displayed in a table in the main panel. A progress bar on 
                                                     the bottom of the screen will indicate computational progress. You may download a flat file of results by clicking 
                                                     the 'Download Results' button."),
                                                   actionButton("runBugModel","Run Macroinvertebrate BCG Model"),
                                                   br(),
                                                   hr(),
                                                   downloadButton("downloadResults_Bug","Download Macroinvertebrate Results")),
                                                 mainPanel(
                                                   h4(strong('BCG Model Results'),
                                                      DT::dataTableOutput('BCGMacroresults')))))
                                        
                                        
                                        )))


