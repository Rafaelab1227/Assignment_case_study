
# Data loading ------------------------------------------------------------
download.file("https://datos.madrid.es/egob/catalogo/300228-21-accidentes-trafico-detalle.csv",destfile="accidentsmadrid.csv")
data1 <- read.csv("accidentsmadrid.csv", sep=";")


# Packages requiere -------------------------------------------------------
require(shiny)
require(dplyr)
require(stringr)

# Prepare data ------------------------------------------------------------
# Geolocation of accidents ------------------------------------------------
# Changes in dataset ------------------------------------------------------
data <- data1 %>% 
    rename(
        NUMERO =NÚMERO,
        ESTADO.METEREOLOGICO = ESTADO.METEREOLÓGICO 
    )

data <- data %>% mutate(ADDRESS=paste(str_trim(CALLE), str_trim(NUMERO), "MADRID", sep=", ") %>% 
                           str_replace(" NA, ", "") %>% 
                           str_replace(", -, ", ", 0,")
)

#Eliminate all the names ("CALL.", "AV.", etc) that are not supported by geolocation
data <- data %>% mutate(ADDRESS=sub('.*\\/', '', ADDRESS),ADDRESS= sub('.*\\. ', '',ADDRESS))

data_address <- unique(data$ADDRESS)

geo <- function(location){
    d <- jsonlite::fromJSON( 
        gsub('\\@addr\\@', gsub('\\s+', '\\%20', location), 
             'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1'))

        if(length(d) == 0){
            return(data.frame(lon = NA,
                              lat = NA))
            } else {
            return(data.frame(lon = as.numeric(d$lon),
                                lat = as.numeric(d$lat)))
        }
}

locations <- suppressWarnings(lapply(data_address, function(lol) {
    result = geo(as.character(lol))
    return(result)
    }) %>%bind_rows() %>% data.frame())

# Panels ------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)



# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
