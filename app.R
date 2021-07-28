library(raster)
library(leaflet)
library(shiny)
library(shinycssloaders)
library(rgeos)
library(htmltools)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(rsconnect)
library(leaflet.extras)
library(rgdal)



##########################################
####   Shiny ui                       ####
##########################################

# ------------------
# Main title section
# ------------------

ui<-navbarPage("Digital Soil Mapping",
               theme = shinytheme("spacelab"),
               tabPanel("Main",
                        
                        # Application title
                        fluidRow(imageOutput("Main_img"),
                                 tagList(
                                   h2(style="color:purple","Digital soil map of west Madhya Pradesh"),
                                   p(style = "font-family: 'times'; font-si30pt;color:'black'",
                                     "The objective of the digital soil map is to create a digitized soil map with high accuracy and 
                           thereby the users can get to know about there soil properties instaneously
                           without going for any lab testing and we can get the values for each pixel which is about",
                                     strong("500m X 500 m resolution")),
                                   tags$br(),
                                   tags$br(),
                                 )),
                        
                        
                        sidebarLayout(
                          sidebarPanel(
                            h3("Property"),
                            tags$br(),
                            selectInput(
                              "property",
                              "Select property",
                              choices = list("NDVI"),
                              selected = "NDVI"
                            ),
                            numericInput("lat",
                                         "Latitude",
                                         value = ""
                                         
                                         
                                         
                            ),
                            numericInput("long",
                                         "Longitude",
                                         value = ""
                                         
                                         
                            ),
                            actionButton("go","Get Values"),
                            
                            fluidRow( box(title = "Values", status = "info", solidHeader = TRUE,
                                          tableOutput("df") ))
                          ),
                          
                          
                          mainPanel( ##### leaflet output
                            leafletOutput(outputId = "map", width = "700px", 
                                          height = "500px"),
                            tags$br(),
                            tags$br(),
                            
                            actionButton("gps","Get values for my location"),
                            
                            fluidRow( box(title = "Values of my location", status = "info", solidHeader = TRUE,
                                          verbatimTextOutput("lat1"),
                                          verbatimTextOutput("long1"),
                                          tableOutput("df1") )),
                            
                          ),
                        ),###siderbarlayout end 
               ),
               tabPanel("About",
                        h1("Site is under construction"),
                        fluidRow(imageOutput("about_img")),
                        
               ),
               tabPanel("Contact",
                        h1("Site is under construction"),
                        fluidRow(imageOutput("contact_img")))
)     




##########################################
####   Shiny server                   ####
##########################################

server <- function(session, input, output) {
  
  
  ############### Output files #################
  # Load the covariates from the covariates *.tif files
  files <- list.files(path = "www/", pattern = ".tif",
                      full.names = TRUE)
  # Stack all the files in one RasterStack
  images <- stack(files)
  
  # Check the coordinate system (as same as data)
  crs(images)
  
  images<-projectRaster(images, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  
  ##### main page title image
  output$Main_img <- renderImage({
    
    list(src = "www/Front.png",width = "100%")
  },deleteFile = F)
  
  #Get Coordinates for Basemap
  xBase <- (extent(images)[2] + extent(images)[1]) /2
  yBase <- (extent(images)[4] + extent(images)[3]) /2
  
  
  sp <- SpatialPoints(data.frame(xBase ,yBase))
  crs(sp) <- crs(images)
  sp <- spTransform(sp, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  
  #Assign output$Map with renderLeaflet object
  output$map<-renderLeaflet({
    
    # initialise the leaflet object
    leaflet()  %>%
      setView(sp@coords[1],sp@coords[2], 7) %>%
      addProviderTiles(c(providers$OpenStreetMap.Mapnik),
                       providerTileOptions(noWrap = TRUE))%>%
      addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                         autoCenter = TRUE, maxZoom = 60, 
                                         setView = TRUE))
  })
  
  
  reactiveRaster <- reactive({images[[input$property]]})
  
  observe({#Observer to show Popups on click
    click <- input$map_click
    if (!is.null(click)) {
      showpos(x=click$lng, y=click$lat)
    }
  })
  showpos <- function(x=NULL, y=NULL) {#Show popup on clicks
    #Translate Lat-Lon to cell number using the unprojected raster
    #This is because the projected raster is not in degrees, we cannot use it!
    cell <- cellFromXY(reactiveRaster(), c(x, y))
    if (!is.na(cell)) {#If the click is inside the raster...
      xy <- xyFromCell(reactiveRaster(), cell) #Get the center of the cell
      x <- xy[1]
      y <- xy[2]
      #Get row and column, to print later
      rc <- rowColFromCell(reactiveRaster(), cell)
      #Get value of the given cell
      val = extract(reactiveRaster(),cell)
      content <- paste0(" Lon=", round(x, 5),
                        "; Lat=", round(y, 5),
                        "; Value=", round(val, 3))
      proxy <- leafletProxy("map")
      #add Popup
      proxy %>% clearPopups() %>% addPopups(x, y, popup = content)
      
    }
  }
  
  observe({
    
    leafletProxy("map") %>%
      clearImages() %>%
      addRasterImage(reactiveRaster())
    
  })
  
  output$cellnumber <- renderText(round(Coords(),5))
  
  
  output$rasvalue   <- renderText(value())
  
  Coords <- reactive({
    c(input$long, input$lat)
    
  })
  
  ####gettting values for output
  output$df<- renderTable({
    valdf        <-  data.frame("Layer" = names(images), "Value" = (unlist(value())[1,]),
    )
    colnames(valdf) <- c("Name","Value")
    rownames(valdf)  <- 1:nlayers(images)
    valdf
  })
  
  
  value  <- eventReactive(input$go,{
    extract(images,cellFromXY(images,Coords()))
  })
  
  
  ##### about page title image
  output$about_img <- renderImage({
    
    list(src = "www/miky.gif")
  },deleteFile = F)
  
  
  ##### contact page title image
  output$contact_img <- renderImage({
    
    list(src = "www/miky1.gif")
  },deleteFile = F)
  
  lat1<-reactive({
    input$map_gps_located$coordinates$lat
  })
  long1<-reactive({
    input$map_gps_located$coordinates$lng
  })
  Coords1 <- reactive({c(lat1(),long1())
  })
  
  
  ####gettting values for output
  output$df1<- renderTable({
    
    valdf1        <-  data.frame("Layer" = names(images), "Value" = (unlist(value1())[1,]),
    )
    colnames(valdf1) <- c("Name","Value")
    rownames(valdf1)  <- 1:nlayers(images)
    valdf1
  })
  
  
  value1  <- eventReactive(input$gps,{
    extract(images,cellFromXY(images,Coords1()))
  })
  
  output$lat1<-renderText({lat1()})
  output$long1<-renderText({long1()})
  
  
}



##################### WEb interface ########

shinyApp(ui = ui, server = server)
