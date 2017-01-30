source('global.R')
template <- read.csv('data/template.csv')
shp_wgs84 <- readOGR('data/','AboveOther_final')
dat <- read.csv('data/sampleList_GIS.csv')
options(digits=3)

shinyServer(function(input, output, session) {
  ## Tab 1, bring in taxa data
  # Download taxa list template
  output$downloadTemplate <- downloadHandler(filename=function(){'template.csv'},
                                             content=function(file){write.csv(template,file)})
  
  # Upload taxa list
  inputFile <- reactive({inFile <- input$sites
  if(is.null(inFile))
    return(NULL)
  read.csv(inFile$datapath)
  })
  output$inputTable <- renderTable({inputFile()})
  
  
  
  ## Tab 2, connect subbasins
  # Empty leaflet map
  output$Map <- renderLeaflet({
    pal <- colorFactor(c("blue","red"),levels=shp_wgs84@data$AboveOther)
    leaflet() %>% addProviderTiles("Thunderforest.Outdoors") %>% 
      setView(-80.22,37.69, zoom=6)%>% 
      addPolygons(data=shp_wgs84,color=~pal(shp_wgs84@data$AboveOther), weight=2, popup=shp_wgs84@data$SUBBASIN) %>%
      addLegend(position = "bottomright",title='Ecoregion 69 Model',pal = pal, values = shp_wgs84@data$AboveOther)})
  
  # Function to connect Subbasins to sites, need to make modal later!!!!!!
  SubbasinConnection <- function(dfInCorrectFormat,gisLayer){
    # Bring in subbasins polygon
    polys <- gisLayer
    # Make shapefile from dfInCorrectFormat
    sites_shp <- dfInCorrectFormat
    coordinates(sites_shp) <- ~Longitude+Latitude
    sites_shp@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ") #first need to give it it's own projection 
    dfOUT <- data.frame(matrix(ncol=1,nrow=1))
    names(dfOUT) <- 'Subbasin'
    # Pull Subbasin information where site lat/long intersects
    for(i in 1:length(sites_shp)){
      sites_subB <- polys[sites_shp[i,],]
      dfOUT[i,] <- as.character(sites_subB@data[1,1])
    }
    dfInCorrectFormat <- cbind(dfInCorrectFormat,dfOUT)%>%
      mutate(Subbasin_short=revalue(Subbasin,c('Upper New'='UNew','Middle New- Virginia'='MNew_VA'
                                               ,'Middle New- West Virginia'='MNew_WV','Lower New'='LNew'
                                               ,'Upper Levisa'='ULev','Upper Kanawha'='UKan'
                                               ,'Upper Guyandotte'='UGuy','Lower Guyandotte'='LGuy'
                                               ,'Upper Clinch, Tennessee, Virginia'='UClinch'
                                               ,'Twelvepole'='Tpole'),warn_missing=F)) # Rename values as they come out of shapefile to match attribute lists
  }
  
  # Run subbasin connection function
  subB <- eventReactive(input$connectSubB, #{withProgress(message='Processing Sites',value=10,{
   SubbasinConnection(inputFile(),shp_wgs84))#})})
  
  # Make table at below map for users to review subbasin connections
  output$stationsWithSubbasins <- renderTable({
    if(is.null(subB()))
      return(NULL)
    subB <- subset(subB(), !duplicated(SampleName))
    return(select(subB,-c(CommonName,ScientificName,Count,Date,Subbasin_short)))})
  
  # Add markers to leaflet map once sites are connected to subbasins
  observe({if(!is.null(subB())){
    leafletProxy('Map') %>% 
      addMarkers(data=subB(), ~Longitude,~Latitude,
                 popup=paste(sep="<br/>",strong('Station:'),subB()$SampleName,strong('Date Sampled:'),subB()$Date))}})
  
  
  ## Tab 3, run the model
  # BCG Model, need to put in modal!!!!!!!!!!!!!! 
  BCGresults <- eventReactive(input$runModel,{withProgress(message='Processing Sites',value=10,{
    BCG_Model_GIS(subB())})})
  
  # Display BCG model results
  options(digits=3)
  output$BCGresults <- renderDataTable({if(!is.null(BCGresults())){
    datatable(BCGresults(),options = list(lengthMenu=list(c(5,10,25,-1),c('5','10','25','All')),pageLength=10))}})
  
  # Download taxa list template
  output$downloadResults <- downloadHandler(filename=function(){paste(input$sites,Sys.Date(),'.csv',sep='')},
                                            content=function(file){
                                              write.csv(BCGresults(),file)})
  

  
  
  
  
  
})