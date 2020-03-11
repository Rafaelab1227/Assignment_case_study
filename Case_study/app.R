
# Data loading ------------------------------------------------------------
#download.file("https://datos.madrid.es/egob/catalogo/300228-21-accidentes-trafico-detalle.csv",destfile="accidentsmadrid.csv")
data1 <- read.csv("accidentsmadrid.csv", sep=";")

#download.file("https://datos.madrid.es/egob/catalogo/300228-19-accidentes-trafico-detalle.csv",destfile="accidentsmadrid1.csv")
data2 <- read.csv("accidentsmadrid1.csv", sep=";")


# Packages requiere -------------------------------------------------------
require(shiny)
require(tidyverse)
require(magrittr)
require(lubridate)
require(shinydashboard)
require(plotly)
require(tidyr)
require(shinyjs)
require(leaflet)

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

data <- data %>% mutate(ADDRESS=paste(str_trim(CALLE), str_trim(NUMERO), "MADRID, SPAIN", sep=", ") %>% 
                           str_replace(" NA, ", "") %>% 
                           str_replace(", -, ", ", 0,")
)

#Change levels of injury based on dataset dictionary
data$INJURY <- "Mild"
data[is.na(data$LESIVIDAD),match("LESIVIDAD",names(data))]<- "Unknown"
data[is.na(data$LESIVIDAD), match("INJURY",names(data))] <- "Unknown"
data[data$LESIVIDAD=="3", match("INJURY",names(data))] <- "Fatal"
data[data$LESIVIDAD=="14"|data$LESIVIDAD=="", match("INJURY",names(data))] <- "Without assistance"
data[data$LESIVIDAD=="77", match("INJURY",names(data))] <- "Unknown"

#Changes to NA
data <- data %>% mutate_all(na_if,"")

#Eliminate all the names ("CALL.", "AV.", etc) that are not supported by geolocation
data <- data %>% mutate(ADDRESS=sub('.*\\/', '', ADDRESS),ADDRESS= sub('.*\\. ', '',ADDRESS))

# Factor variables --------------------------------------------------------
data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
data[is.na(data)] <- "Unknown"
data[data=="Se desconoce"]<-"Unknown"

data %<>% mutate_at(c("NEXPEDIENTE", "DISTRITO","TIPO.ACCIDENTE","ESTADO.METEREOLOGICO",
                      "LESIVIDAD","SEXO","RANGO.DE.EDAD","TIPO.PERSONA","TIPO.VEHICULO",
                      "INJURY"),
                    as.factor)
levelfactor <- function(x){
    if(is.factor(x)) return(fct_explicit_na(x, na_level = "(Missing)"))
    return(x)
}

data <- as.data.frame(lapply(data, levelfactor))

data_district = levels(data$DISTRITO) %>% str_sort()
data_acc = levels(data$TIPO.ACCIDENTE) %>% str_sort()
data_weather = levels(data$ESTADO.METEREOLOGICO) %>% str_sort()
data_level = levels(data$LESIVIDAD) %>% str_sort()
data_car = levels(data$TIPO.VEHICULO) %>% str_sort()
data_inj = levels(data$INJURY) %>% str_sort()

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


# Data subset expediente --------------------------------------------------
df_exp <- unique(select(data, NEXPEDIENTE, FECHA, DISTRITO, TIPO.ACCIDENTE, ESTADO.METEREOLOGICO,
                   ADDRESS, MONTH, DAY, TIME))
df_count <- select(data, TIPO.ACCIDENTE, TIPO.PERSONA, SEXO, RANGO.DE.EDAD)%>% group_by(TIPO.ACCIDENTE, TIPO.PERSONA, SEXO, RANGO.DE.EDAD)%>%summarise(count=n())


# Data date historic--------------------------------------------------------

# Transformations historic ------------------------------------------------
datah <- data2 %>% 
    rename(
        NUMERO =NÚMERO,
        ESTADO.METEREOLOGICO = ESTADO.METEREOLÓGICO,
        TIPO.VEHICULO = TIPO.VEHÍCULO,
        LESIVIDAD = LESIVIDAD.,
        NEXPEDIENTE = Nº..EXPEDIENTE
    )

datah$INJURY <- "Mild"
datah[is.na(datah$LESIVIDAD),match("LESIVIDAD",names(datah))]<- "Unknown"
datah[is.na(datah$LESIVIDAD), match("INJURY",names(datah))] <- "Unknown"
datah[datah$LESIVIDAD=="3", match("INJURY",names(datah))] <- "Fatal"
datah[datah$LESIVIDAD=="14"|data$LESIVIDAD=="", match("INJURY",names(datah))] <- "Without assistance"
datah[datah$LESIVIDAD=="77", match("INJURY",names(datah))] <- "Unknown"

datah %<>% mutate( FECHA = as.POSIXct(FECHA, format="%d/%m/%Y"))
                  
data_dateh <- datah %>% group_by(FECHA)%>% summarise(Victims = n(),
                                                   Accidents = n_distinct(NEXPEDIENTE))

df_dateh <- data.frame(seq(as.Date(min(datah$FECHA)),as.Date(max(datah$FECHA)),by = 1)+1)
names(df_dateh)[1]<-"FECHA"
df_dateh %<>% mutate(DAY = days[as.POSIXlt(FECHA)$wday + 1],
                    FECHA = as.POSIXct(as.character(FECHA)),
                    MONTH = month.abb[month(as.POSIXlt(FECHA, format="%d/%m/%Y"))],
)

df_dateh <- left_join(df_dateh, data_dateh)

weekdays <- df_dateh[1:7,2]

data_deathsh <- datah[datah$INJURY=="Fatal",] %>% group_by(FECHA) %>% summarise(FVictims = n())

df_dateh <- left_join(df_dateh, data_deathsh)

# Tranformations for current set ------------------------------------------

df_date <- data.frame(seq(as.Date(min(data$FECHA)),as.Date(max(data$FECHA)),by = 1)+1)
names(df_date)[1]<-"FECHA"
df_date %<>% mutate(DAY = days[as.POSIXlt(FECHA)$wday + 1],
                    FECHA = as.POSIXct(as.character(FECHA)),
                    MONTH = month.abb[month(as.POSIXlt(FECHA, format="%d/%m/%Y"))],
)
data_date <- data %>% group_by(FECHA)%>% summarise(Victims = n(),
                                                   Accidents = n_distinct(NEXPEDIENTE))

data_date %<>% mutate(FECHA = as.POSIXct(FECHA, format="%d/%m/%Y"))

df_date <- left_join(df_date, data_date)

data_deaths <- data[data$INJURY=="Fatal",] %>% group_by(FECHA) %>% summarise(FVictims = n())

df_date <- left_join(df_date, data_deaths)


# Join dataset historic ---------------------------------------------------
df_date_historic <- rbind(df_dateh, df_date)



# Geolocation for fatal victims -------------------------------------------
# data_deaths_ad <- ungroup(data[data$INJURY=="Fatal",]) 
# data_address <- unique(as.character(data_deaths_ad$ADDRESS))
# 
# geo <- function(location){
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
# 
# locations <-cbind(locations,data_address)
# names(locations)[3] <- "ADDRESS"
# data_deaths_ad <- left_join(data_deaths_ad, locations)
# 
# nofound <- nrow(data_deaths_ad[is.na(data_deaths_ad$lon),])
# info_nofound <- data_deaths_ad[is.na(data_deaths_ad$lon),] %>% select(CALLE, NUMERO, DISTRITO, RANGO.DE.EDAD, TIPO.ACCIDENTE)
# names(info_nofound) <- c("Street", "Number", "District", "Age", "Type of accident")
# 
# df_deaths_ad <- data_deaths_ad[!is.na(data_deaths_ad$lon),]

 info_found <- data_deaths_ad[!is.na(data_deaths_ad$lon),] %>% select(CALLE, NUMERO, DISTRITO, RANGO.DE.EDAD, TIPO.ACCIDENTE)
 names(info_found) <- c("Street", "Number", "District", "Age", "Type of accident")
 
 df_deaths_ad <- data_deaths_ad[!is.na(data_deaths_ad$lon),]

# Panels ------------------------------------------------------------------
tab1 <- tabItem(tabName = "tab1",
                tags$head(tags$style(HTML(".small-box {height: 200px}"))),
                fluidRow(
                    valueBoxOutput("box1", width=15),
                    valueBoxOutput("box2", width=15),
                    valueBoxOutput("box3", width=15)
                    )
)

tab2 <- tabItem(tabName="tab2",
                sidebarLayout(
                    sidebarPanel(
                        checkboxGroupInput(
                            inputId = "sel_type",
                            label = "Select type of accident",
                            choices = data_acc,
                            selected = data_acc
                        )
                        #actionLink("selectall","Select All"),
                        #actionButton('clickme',  'Click me')
                        #,checkboxGroupInput(
                        #    inputId = "sel_injury",
                        #    label = "Select level of injury",
                        #    choices = data_inj,
                        #    selected = c("Mild")
                        #)
                        
                    ),
                    mainPanel(
                            fluidRow(
                            tabBox(#tabPanel(title = "Historical accidents"),
                                tabPanel(title = "Total",
                                         fluidRow(
                                         column(width = 7,
                                         plotOutput("fig21", hover = hoverOpts(id = "fig21hover", delayType = "throttle")),
                                         ),
                                         column(width = 5,
                                         h2("Information about type of accident"),
                                         h3(textOutput("hovertype", container = span)),
                                         tableOutput("table1"),
                                         tableOutput("table2")
                                         )
                                         )
                                         ),
                                tabPanel(title = "Accidents per district", 
                                         fluidRow(infoBoxOutput("d1"), tags$style("#d1 {height:75px; line-height:75px; padding-top:0px; padding-bottom:100px; width:910px;}")),
                                         plotlyOutput("fig221"),
                                         fluidRow(infoBoxOutput("d2"), tags$style("#d2 {height:75px; line-height:75px; padding-top:0px; padding-bottom:100px; width:910px;}")),
                                         plotlyOutput("fig22")),
                                tabPanel(title = "Weather",
                                         fluidRow(infoBoxOutput("w1"), tags$style("#w1 {height:75px; line-height:75px; padding-top:0px; padding-bottom:100px; width:910px;}")),
                                         plotlyOutput("fig23"),
                                         fluidRow(infoBoxOutput("w2"), tags$style("#w2 {height:75px; line-height:75px; padding-top:0px; padding-bottom:100px; width:910px;}")),
                                         plotlyOutput("fig24")),
                                tabPanel(title = "Injury level",
                                         fluidRow(infoBoxOutput("i1"), tags$style("#i1 {height:75px; line-height:75px; padding-top:0px; padding-bottom:100px; width:910px;}")),
                                         tableOutput("table3"),plotlyOutput("fig25")),
                                width = 15
                            )
                        )
                        
                    )
                )
                
)
tab3 <- tabItem(tabName = "tab3", 
                fluidRow(
                valueBoxOutput("box4", width=5),
                valueBoxOutput("box5", width=5), 
                ),
                fluidRow(infoBoxOutput("da1"), tags$style("#da1 {height:75px; line-height:75px; padding-top:0px; padding-bottom:100px; width:1120px;}")),
                fluidRow(plotlyOutput("fig3")),
                fluidRow(infoBoxOutput("da2"), tags$style("#da2 {height:75px; line-height:75px; padding-top:0px; padding-bottom:100px; width:1120px;}")),
                fluidRow(plotlyOutput("fig31"))
               )
tab4 <- tabItem(tabName = "tab4",
                fluidRow(infoBoxOutput("map1"), tags$style("#map1 {height:75px; line-height:75px; padding-top:0px; padding-bottom:100px; width:1120px;}")),
                leafletOutput('map', width = '100%', height = '300px'),
                useShinyjs(),
                actionButton("btn", "Info of victims geolocation not found"),
                tableOutput("element"),
                actionButton("btn2", "Info victims display"),
                tableOutput("element2")
                )


# Ui ----------------------------------------------------------------------
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = 'Madrid accidents 2020', titleWidth = 500),
    dashboardSidebar(sidebarMenu(
                                    menuItem("Description", tabName = "tab1", icon = icon("binoculars")),
                                    menuItem("Type", tabName = "tab2", icon = icon("bus-alt")),
                                    menuItem("Date", tabName = "tab3", icon = icon("calendar-alt")),
                                    menuItem("Location", tabName = "tab4", icon = icon("map-marked-alt"))
)
),
dashboardBody(
              tabItems(tab1,
              tab2,
              tab3,
              tab4)
)
)

# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
# Tab 1 -------------------------------------------------------------------
    
    # Box 1 -----------------------------------------------------------------
    output$box1 <- renderValueBox({
        valueBox(
            value = as.numeric(sum(df_date$Accidents, na.rm=TRUE)),
            color = "blue",
            icon = icon("car-crash"),
            subtitle = "Total accidents"
            
        )}) 
    # Box 2 -----------------------------------------------------------------
    output$box2 <- renderValueBox({
        valueBox(
            value = as.numeric(sum(df_date$Victims, na.rm=TRUE)),
            color = "olive",
            icon = icon("users"),
            subtitle = "Total victims"
            
        )}) 
    # Box 3 -----------------------------------------------------------------
    output$box3 <- renderValueBox({
        valueBox(
            value = as.numeric(sum(df_date$FVictims, na.rm=TRUE)),
            color = "orange",
            icon = icon("ambulance"),
            subtitle = "Total fatal victims"
            
        )}) 
    
# Tab 2 -------------------------------------------------------------------
    data2 <- reactive({data %>% filter(TIPO.ACCIDENTE %in% unlist(input$sel_type))})
   # data2 <- reactive({data %>% filter(TIPO.ACCIDENTE %in% input$sel_type, INJURY %in% input$sel_injury)})
    

    df_count2 <-reactive({data2%>% group_by(TIPO.ACCIDENTE, TIPO.PERSONA, SEXO, RANGO.DE.EDAD)%>%summarise(count=n())})
    df_exp <- reactive({unique(select(data2, NEXPEDIENTE, FECHA, DISTRITO, TIPO.ACCIDENTE, ESTADO.METEREOLOGICO,
                            ADDRESS, MONTH, DAY, TIME))})
# Plots tab 2 -------------------------------------------------------------
   
    # Total -----------------------------------------------------------------
    output$fig21 <- renderPlot({
        df_total <-data2()%>% group_by(TIPO.ACCIDENTE)%>%summarise(Victims=n(),
                                                                Accidents = n_distinct(NEXPEDIENTE))
        df_total_long <-gather(df_total, var, value, Victims:Accidents)
        
        ggplot(df_total_long, aes(x=TIPO.ACCIDENTE, y=value, fill=var))+ geom_bar(stat='identity', position=position_dodge())+
            coord_flip()+
            theme_classic()+
            labs(x = "Type accident selected", y = "Frequency") 
    })

# Hover fig21 -------------------------------------------------------------
    hoverfig21 <- reactive({
        req(input$fig21hover$y)
        round(input$fig21hover$y)
    })
    
    hovertype <- reactive({
        req(hoverfig21() > 0 & hoverfig21() <= length(input$sel_type))
        input$sel_type[hoverfig21()]
    })
    
    output$hovertype <- renderText(hovertype())
    
    df_tipo_victims <- data%>% group_by(TIPO.ACCIDENTE, TIPO.PERSONA)%>%summarise(Victims=n())
    names(df_tipo_victims)[2] <- "Type of victim"
    df_total_v <-data%>% group_by(TIPO.ACCIDENTE)%>%summarise(Victims=n(),
                                                               Accidents = n_distinct(NEXPEDIENTE))
    
    
    tabletipo <- reactive(df_tipo_victims %>% filter( TIPO.ACCIDENTE == hovertype()) %>% ungroup()%>% select(-TIPO.ACCIDENTE))
    output$table2 <- renderTable(tabletipo())
    
    tabletotal <- reactive(df_total_v %>% filter( TIPO.ACCIDENTE == hovertype())%>% ungroup()%>% select(-TIPO.ACCIDENTE)) 
    output$table1 <- renderTable(tabletotal())
    
    # District -----------------------------------------------------------------
    output$d1 <- renderInfoBox({
        infoBox(title = "Number of accidents per district",
                fill = T, color = "purple")
    })
    output$d2 <- renderInfoBox({
        infoBox(title = "Number of victims per district",
                fill = T, color = "purple")
    })
    

    # Plots -------------------------------------------------------------------
    output$fig221 <- renderPlotly({
    df_district2 <-data2()%>% group_by(DISTRITO)%>%summarise(Victims=n(),
                                                             Accidents = n_distinct(NEXPEDIENTE)
    )
    plot_ly(data.frame(df_district2),
                x = ~DISTRITO, 
                y = ~Accidents,
                type = 'bar')
    
    })
    output$fig22 <- renderPlotly({
    df_victim2 <-data2()%>% group_by(DISTRITO, TIPO.PERSONA)%>%summarise(Victims=n())
    
    
    df_victim2$ids <- gsub('\\s+', '', df_victim2$DISTRITO)
    df_victim2_wide <- spread(df_victim2, TIPO.PERSONA, Victims)

    plot <- plot_ly(df_victim2,
            x = ~DISTRITO, 
            y = ~Victims, 
            color= ~TIPO.PERSONA,
            colors = 'Blues',
            type = 'bar',
            legendgroup = "A")  
    
        
        subplot(plot, shareY = T) %>% layout(barmode = 'stack')
    })
# Weather -----------------------------------------------------------------
    output$w1 <- renderInfoBox({
        infoBox(title = "Number of accidents based based on the weather of the day",
                fill = T, color = "purple")
    })

    output$w2 <- renderInfoBox({
        infoBox(title = "Number of victims based on the based on the weather of the accident day",
                fill = T, color = "purple")
    })

    # Plot --------------------------------------------------------------------

        output$fig23 <- renderPlotly({
        
        df_weather2 <-data2()%>% group_by(ESTADO.METEREOLOGICO)%>%summarise(Victims=n(),
                                                                        Accidents = n_distinct(NEXPEDIENTE))

        fig23 <- plot_ly(df_weather2, labels = ~ESTADO.METEREOLOGICO, values = ~Accidents, type = 'pie')
        fig23 <- fig23 %>% layout(
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig23
    })
    
    output$fig24 <- renderPlotly({
        df_weather2 <-data2()%>% group_by(ESTADO.METEREOLOGICO)%>%summarise(Victims=n(),
                                                                            Accidents = n_distinct(NEXPEDIENTE))
        fig24 <- plot_ly(df_weather2, labels = ~ESTADO.METEREOLOGICO, values = ~Victims, type = 'pie')
        fig24 <- fig24 %>% layout(
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig24
    })
    

# Injury level ------------------------------------------------------------
    output$i1 <- renderInfoBox({
        infoBox(title = "Number of victims based on the level of injury register",
                fill = T, color = "purple")
    })


    # Plots -------------------------------------------------------------------
    tabletotal3 <- reactive(data2()%>% group_by(INJURY)%>%summarise(Victims=n()))
    output$table3 <- renderTable(tabletotal3())
    
    output$fig25 <- renderPlotly({
        df_district2 <-data2()%>% group_by(INJURY)%>%summarise(Victims=n(),
                                                                 Accidents = n_distinct(NEXPEDIENTE)
        )
       p <- plot_ly(data.frame(df_district2),
                x = ~INJURY)%>%
                add_bars(y = ~Victims, color = rainbow(nrow(df_district2)))
       hide_legend(p)
    })
    
    
# Tab 3 -------------------------------------------------------------------
    # Box 4 -------------------------------------------------------------------
    output$box4 <- renderValueBox({
        valueBox(
            value = as.numeric(sum(df_date_historic$Accidents, na.rm=TRUE)),
            color = "purple",
            icon = icon("car-crash"),
            subtitle = "Total accidents"
            
        )}) 
    
    output$box5 <- renderValueBox({
        valueBox(
            value = as.numeric(sum(df_date_historic$Victims, na.rm=TRUE)),
            color = "purple",
            icon = icon("ambulance"),
            subtitle = "Total Victims"
            
        )}) 

    output$da1 <- renderInfoBox({
        infoBox(title = "Historical statistics since january 2019 until available information",
                fill = T, color = "purple")
    })
    
    output$da2 <- renderInfoBox({
        infoBox(title = "Percentage of historical accidents based on the day that occured",
                fill = T, color = "purple")
    })
    
    # Plots -------------------------------------------------------------------

    output$fig3 <- renderPlotly({
    fig3 <- plot_ly(df_date_historic, x = ~FECHA)
    fig3 <- fig3 %>% add_bars(y = ~Accidents, name = "Accidents", marker = list(color = 'green'))
    fig3 <- fig3 %>% add_bars(y = ~Victims, name = "Victims", marker = list(color = 'rgb(26, 118, 255)'))
    fig3 <- fig3 %>% add_bars(y = ~FVictims, name = "Fatal victims", marker = list(color = 'rgb(300, 0, 0)'))

    fig3 <- fig3 %>% layout(
        xaxis = list(
            rangeselector = list(
                buttons = list(
                    list(
                        count = 1,
                        label = "Last month",
                        step = "month",
                        stepmode = "backward"),
                    list(
                        count = 6,
                        label = "6 months",
                        step = "month",
                        stepmode = "backward"),
                    list(
                        count = 1,
                        label = "1 year",
                        step = "year",
                        stepmode = "backward"),
                    list(
                        count = 1,
                        label = "YTD",
                        step = "1 year to date",
                        stepmode = "todate"),
                    list(step = "all"))),
            
            rangeslider = list(type = "date")),
        
        yaxis = list(title = "Frequency"))
    fig3
    })

# Pie chart ---------------------------------------------------------------

    output$fig31 <- renderPlotly({
        df_weather2 <-df_date_historic%>% group_by(DAY)%>%summarise(Accidents=sum(Accidents, na.rm=TRUE))
        fig31 <- plot_ly(df_weather2, labels = ~DAY, values = ~Accidents, type = 'pie')
        fig31 <- fig31 %>% layout(
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig31
    })
    
# Tab 4 -------------------------------------------------------------------
    output$map1 <- renderInfoBox({
        infoBox(title = "Location of accidents which involved fatal victims",
                fill = T, color = "purple")
    })


 output$map <- renderLeaflet({
  df_deaths_ad %>%
  leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=df_deaths_ad$lon, lat=df_deaths_ad$lat)

  })
    onclick("btn", output$element <- renderTable({info_nofound}))
    onclick("btn2", output$element2 <- renderTable({info_found}))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
