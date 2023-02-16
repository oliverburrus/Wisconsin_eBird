#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#import tidyverse and data.table to work with data
library(tidyverse)
library(data.table)

#Set our 
eBirdFile <- "C:/Users/olive/Downloads/0279848-220831081235567/0279848-220831081235567.csv"
options(data.table.fread.auto.chunksize = 1000000) #Set the chunk size as we are working with a very large csv (20 million rows)
data <- fread(eBirdFile) #Note to use fread to read large dataframes

#Import libraries for converting lat/lon to county
library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees
# reused from StackOverflow
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

# Test the function using points in Wisconsin and Oregon.
testPoints <- data.frame(x = data$decimalLongitude, y = data$decimalLatitude)

counties = latlong2county(testPoints)
foo <- data.frame(do.call('rbind', strsplit(counties,',',fixed=TRUE)))
data$county = str_to_title(foo$X2) #Capitalize the county names

#Create a baseline for observations per county
y = data.frame(table(data$county))

#import mapping libraries and ggthemes
library(sf)
library(tidyverse)
library(ggthemes) 
library(ggplot2)

nepal_shp <- read_sf("C:/Users/olive/Downloads/County_Boundaries_24K.geojson")

# calculate points at which to plot labels
centroids <- nepal_shp %>% 
  st_centroid() %>% 
  bind_cols(as_data_frame(st_coordinates(.)))    # unpack points to lat/lon columns

centroids$COUNTY_NAME[50] = "St Croix"
centroids$COUNTY_NAME[22] = "Fond Du Lac"
nepal_shp$COUNTY_NAME[50] = "St Croix"
nepal_shp$COUNTY_NAME[22] = "Fond Du Lac"


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Wisconsin eBird Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Type of Filter", 
                  choices = c("species", "family", "order")
      ),
      conditionalPanel(
        condition = "input.type == 'species'",
        selectInput(
          "species", "Enter a Species Name?", unique(data$species),
          multiple = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.type == 'family'",
        selectInput(
          "family", "Enter a Family Name?", unique(data$family),
          multiple = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.type == 'order'",
        selectInput(
          "order", "Enter an Order Name?", unique(data$order),
          multiple = TRUE
        )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot2"),
      plotOutput("plot3")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data1 <- as.data.table(data)
  y1 <- y
  dat <- reactive({
    if(input$type == "species"){
      # set the key of the data.table to the column to filter by
      setkey(data1, species)
      
      # filter the rows where the species column equals "Eurasian Wigeon"
      x <- data1[.(c(input$species))]
    } else if(input$type == "family"){
      # set the key of the data.table to the column to filter by
      setkey(data1, family)
      
      # filter the rows where the species column equals "Eurasian Wigeon"
      x <- data1[.(c(input$family))]
    } else{
      # set the key of the data.table to the column to filter by
      setkey(data1, order)
      
      # filter the rows where the species column equals "Eurasian Wigeon"
      x <- data1[.(c(input$order))]
    }
    a <- data.frame(table(x$county))
    z <- left_join(a, y1, by = "Var1")
    z$Freq <- (z$Freq.x/z$Freq.y)*100
    z
  })
  dat1 <- reactive({
    if(input$type == "species"){
      # set the key of the data.table to the column to filter by
      setkey(data1, species)
      
      # filter the rows where the species column equals "Eurasian Wigeon"
      x <- data1[.(c(input$species))]
    } else if(input$type == "family"){
      # set the key of the data.table to the column to filter by
      setkey(data1, family)
      
      # filter the rows where the species column equals "Eurasian Wigeon"
      x <- data1[.(c(input$family))]
    } else{
      # set the key of the data.table to the column to filter by
      setkey(data1, order)
      
      # filter the rows where the species column equals "Eurasian Wigeon"
      x <- data1[.(c(input$order))]
    }
    x
  })
  output$plot2 <- renderPlot({
    left_join(nepal_shp, dat(), by = c('COUNTY_NAME' = 'Var1')) %>% 
      ggplot() + 
      geom_sf(aes(fill = Freq)) + 
      geom_text(aes(X, Y, label = COUNTY_NAME), data = centroids, size = 1, color = 'white')+ 
      theme_tufte()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  })
  output$plot3 <- renderPlot({
    ggplot()+
      geom_histogram(data = dat1(), aes(month), bins = 12)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
