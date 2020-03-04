
# Data loading ------------------------------------------------------------
download.file("https://datos.madrid.es/egob/catalogo/300228-19-accidentes-trafico-detalle.csv",destfile="accidentsmadrid.csv")
data <- read.csv("accidentsmadrid.csv", sep=";")


library(rjson)

pop_proj_data_df <- fromJSON(getURL("https://data.cambridgema.gov/resource/ybny-g9cv.json"))


# Packages requiere -------------------------------------------------------
library(shiny)




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
