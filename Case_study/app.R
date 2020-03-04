
# Data loading ------------------------------------------------------------
download.file("https://datos.madrid.es/egob/catalogo/300228-21-accidentes-trafico-detalle.csv",destfile="accidentsmadrid.csv")
data1 <- read.csv("accidentsmadrid.csv", sep=";")


# Packages requiere -------------------------------------------------------
require(shiny)
require(tidyverse)
require(magrittr)
require(lubridate)
# Prepare data ------------------------------------------------------------
# Geolocation of accidents ------------------------------------------------
# Changes in dataset ------------------------------------------------------
data <- data1 %>% 
    rename(
        NUMERO =NÚMERO,
        ESTADO.METEREOLOGICO = ESTADO.METEREOLÓGICO,
        TIPO.VEHICULO = TIPO.VEHÍCULO,
        LESIVIDAD = LESIVIDAD.,
        NEXPEDIENTE = Nº..EXPEDIENTE
    )

data <- data %>% mutate(ADDRESS=paste(str_trim(CALLE), str_trim(NUMERO), "MADRID", sep=", ") %>% 
                           str_replace(" NA, ", "") %>% 
                           str_replace(", -, ", ", 0,")
)

data <- data %>% mutate_all(na_if,"")

#Eliminate all the names ("CALL.", "AV.", etc) that are not supported by geolocation
data <- data %>% mutate(ADDRESS=sub('.*\\/', '', ADDRESS),ADDRESS= sub('.*\\. ', '',ADDRESS))

data_address <- unique(data$ADDRESS)

#geo <- function(location){
#    d <- jsonlite::fromJSON( 
#        gsub('\\@addr\\@', gsub('\\s+', '\\%20', location), 
#             'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1'))
#
#        if(length(d) == 0){
#            return(data.frame(lon = NA,
#                               lat = NA))
#             } else {
#             return(data.frame(lon = as.numeric(d$lon),
#                                 lat = as.numeric(d$lat)))
#         }
# }
# 
# locations <- suppressWarnings(lapply(data_address, function(lol) {
#     result = geo(as.character(lol))
#     return(result)
#     }) %>%bind_rows() %>% data.frame())


# Factor variables --------------------------------------------------------
data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
data[is.na(data)] <- "UNKNOWN"
data[data=="Se desconoce"]<-"UNKNOWN"

data %<>% mutate_at(c("NEXPEDIENTE", "DISTRITO","TIPO.ACCIDENTE","ESTADO.METEREOLOGICO",
                      "LESIVIDAD","SEXO","RANGO.DE.EDAD","TIPO.PERSONA","TIPO.VEHICULO"),
                    as.factor)

data_district = levels(data$DISTRITO) %>% str_sort()
data_acc = levels(data$TIPO.ACCIDENTE) %>% str_sort()
data_weather = levels(data$ESTADO.METEREOLOGICO) %>% str_sort()
data_level = levels(data$LESIVIDAD) %>% str_sort()
data_car = levels(data$TIPO.VEHICULO) %>% str_sort()

days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
          "Friday", "Saturday")
# Date variables ----------------------------------------------------------
data %<>% mutate( FECHA = as.POSIXct(FECHA, format="%d/%m/%Y"),
                  MONTH = month.abb[month(as.POSIXlt(FECHA, format="%d/%m/%Y"))],
                  #DAY = as.POSIXlt(FECHA)$wday)
                  #DAY = weekdays(as.Date(DATE)))
                  DAY = days[as.POSIXlt(FECHA)$wday + 1],
                  #DAY = format(FECHA, format = "%A"),
                  TIME = format(as.POSIXct(HORA, format = "%H:%M"),  "%H"))

data %<>% mutate_at(c("DAY", "MONTH","TIME"),
                    as.factor)
data_hour = levels(data$TIME)
data_day = levels(data$DAY)
data_day = levels(ordered(data$DAY, levels=days))
data_month = levels(data$MONTH)

# Panels ------------------------------------------------------------------
tab1 <- tabItem(tabName = "tab1",print("Hello1"))
tab2 <- tabItem(tabName = "tab2",print("Hello2"))
tab3 <- tabItem(tabName = "tab3",print("Hello3"))
tab4 <- tabItem(tabName = "tab4",print("Hello4"))


# Ui ----------------------------------------------------------------------
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = 'Madrid accidents 2020'),
    dashboardSidebar(sidebarMenu(
                                    menuItem("Description", tabName = "tab1", icon = icon("binoculars")),
                                    menuItem("Type", tabName = "tab2", icon = icon("bus-alt")),
                                    menuItem("Date", tabName = "tab3", icon = icon("calendar-alt")),
                                    menuItem("Location", tabName = "tab4", icon = icon("map-marked-alt"))
)
),
dashboardBody(tabItems(tab1,
              tab2,
              tab3,
              tab4)
)
)

# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
    print("Hello")
}

# Run the application 
shinyApp(ui = ui, server = server)
