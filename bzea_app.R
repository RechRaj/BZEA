library(shiny)
library(shiny.router)
library(leaflet)
library(dplyr)
library(DT)
library(shinydashboard)
library(ggplot2)
library(htmltools)

main <- read.csv(file = "BZEA.csv")
main2 <- read.csv(file = "All.csv")

ui <- fluidPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
  # ),
  dashboardPage(title = "BZEA-DB Analysis (J2Teo)",
  skin = 'purple',
  dashboardHeader(title = "BZEA-DB Analysis (J2Teo)"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Home", tabName = 'home', icon = icon("home")),
      menuItem("Data Table", tabName = 'data', icon = icon("table")),
      menuItem("Map Insights", tabName = 'maps', icon = icon("map")),
      menuItem("Altitude Dumbbell", tabName = 'dumbbell', icon = icon("chart-bar")),
      menuItem("Pie Charts", tabName = 'pie', icon = icon("chart-pie")),
      menuItem("Data with Filters", tabName = "filter", icon = icon("filter"))
    ),
    img(src = "zea.png", height = 200, width = 200)
  ),
  dashboardBody(
    tags$style(HTML(
      "body{
        background-image: url('https://images.unsplash.com/photo-1691063617861-7ade0c1cb34d?q=80&w=3870&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D');
      }
      .content-wrapper{
        background-image: url('https://images.unsplash.com/photo-1691063617861-7ade0c1cb34d?q=80&w=3870&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D');
        background-size: cover;
        background-repeat: no-repeat;
        background-attachment: fixed;
      }
      .box.box-solid.box-warning {
        background-color: #EDEAF4;
      }
      .box.box-solid.box-warning>.box-header{
        background: #6B4AA3;
      }
      .box.box-solid.box-warning {
      position: relative;
      }
      .content{
        color: white
      }
      .box-body{
        color: black
      }"
    )),
    tabItems(
      tabItem( # fluidRow() in here if needed
        tabName = 'home',
        # titlePanel("Home"),
        h1("BZEA DB DATA (J2TEO)", style = "text-align: center; font-size: 48px; font-weight: bold;"),
        p("This dashboard contains the data, visualizations, Filtered data and map representations of Bzea DB (J2Teo)", style = "text-align: center; font-weight: bold;"),
        p("*****************", style = "text-align: center;")
      ),
      tabItem(
        tabName = 'data',
        # titlePanel("Raw Data"),
        # dataTableOutput("table")
        box(
          title = "Bzea DB Data Table Representation",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          DTOutput('tableDT', width = 'auto', fill = FALSE)
        )
      ),
      tabItem(
        tabName = 'maps',
        box(
          title = "Altitude Map",
          status = "warning",
          solidHeader = TRUE,
          leafletOutput("altmap")
        ),
        box(
          title = "Race Map",
          status = "warning",
          solidHeader = TRUE,
          leafletOutput("racemap")
        ),
        box(
          title = "Species Map",
          status = "warning",
          solidHeader = TRUE,
          leafletOutput("speciesmap")
        )
      ),
      tabItem(
        tabName = 'dumbbell',
        fluidRow(
          box(
            DTOutput('bellmodded'),
            width = 4
          ),
          box(
            plotOutput('p', height = '1000px'),
            width = 8
          )
        )
      ),
      tabItem(
        tabName = "pie",
        fluidRow(
          box(
            title = "Race Distribution",
            status = "warning",
            solidHeader = TRUE,
            plotOutput('racepie')
          ),
          box(
            title = "Species Distribution",
            status = "warning",
            solidHeader = TRUE,
            plotOutput('speciespie')
          )
        )
      ),
      
      tabItem(
        tabName = 'filter',
        fluidRow(
          box(
            title = "Filters",
            status = "warning",
            solidHeader = TRUE,
            
            selectInput("Species", "Species:", choices = c("", unique(main2$Species))),
            selectInput("subspecies", "subspecies:", choices = c("", unique(main2$subspecies))),
            selectInput("race", "race:", choices = c("", unique(main2$race))),
            selectInput("gen", "Generation:", choices = c("", unique(main2$gen))),
            selectInput("F1", "F1 Hybrid:", choices = c("", unique(main2$F1))),
            selectInput("BC1", "Backcross 1 (BC1):", choices = c("", unique(main2$BC1))),
            actionButton("clearFilters", "Clear Filters")
        ),
        box(
          title = "Filtered Data Table",
          status = "warning",
          solidHeader = TRUE,
          DTOutput('filteredTableDT', width = 'auto', fill = FALSE)
        )
      )
    )
      
    )
  )
)
)


server <- function(input, output, session) {

  # main <- read.csv(file = "BZEA.csv")
  # main2 <- read.csv(file = "All.csv")

  output$altmap <- renderLeaflet({
    pal <- colorNumeric(palette = "OrRd",
                        domain = main$Altitude)
    
    leaflet(data = main) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       radius = 5,
                       group = "marker",
                       color = "#000000",
                       weight = 1,
                       opacity = 1,
                       fillColor = ~pal(Altitude),
                       fillOpacity = 1,
                       popup = paste("<b>Species: </b>", main$Species, "<br/>",
                                     "<b>Race: </b>", main$Race, "<br/>",
                                     "<b>Population: </b>", main$Population)) %>%
      addLegend(position = "bottomright",
                pal = pal,
                values = ~Altitude,
                opacity = 1,
                title = "Sample Altitude (Meters)")
  })

  output$racemap <- renderLeaflet({
    pal <- colorFactor(palette = "Set3",
                       domain = main$Race)
    
    leaflet(data = main) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       radius = 5,
                       group = "marker",
                       color = "#000000",
                       weight = 1,
                       opacity = 1,
                       fillColor = ~pal(Race),
                       fillOpacity = 1,
                       popup = paste("<b>Species: </b>", main$Species, "<br/>",
                                     "<b>Race: </b>", main$Race, "<br/>"))
  })
 
  output$speciesmap <- renderLeaflet({
    pal <- colorFactor(palette = "Set3",
                       domain = main$Species)
    
    leaflet(data = main) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       radius = 5,
                       group = "marker",
                       color = "#000000",
                       weight = 1,
                       opacity = 1,
                       fillColor = ~pal(Species),
                       fillOpacity = 1,
                       popup = paste("<b>Species: </b>", main$Species, "<br/>",
                                     "<b>Race: </b>", main$Race, "<br/>",
                                     "<b>Population: </b>", main$Population))
  })
  
  
  # PLEASE use comma inside this thing
  # output$table <- renderDataTable(
  #   main,
  #   options = list(pageLength = 20)
  # )
  output$tableDT <- renderDT(
    main,
    options = list(pageLength = 15, scrollX = TRUE, scrollY = "600px")
  )
  
  
  main %>%
    select(Race, Altitude) %>%
    group_by(Race) %>%
    summarize(Max = max(Altitude), Min = min(Altitude)) -> bellmod
  
  output$bellmodded <- renderDT(
    bellmod,
    options = list(pageLength = 15)
  )
  
  # getting rid of blank data
  bellmod <- subset(bellmod, nchar(Race) > 0)
  main2 <- subset(main, nchar(Race) > 0)
  
  output$p <- renderPlot({
    ggplot(bellmod) + 
    geom_segment(aes(x = Min, xend = Max, y = Race, yend = Race),
                 color = '#aeb6bf',
                 size = 3,
                 alpha = 0.5) +
    # geom_point(aes(x = Min, y = Race, color = '#42b6f5'),
    #            size = 4,
    #            show.legend = TRUE) +
    # geom_point(aes(x = Max, y = Race, color = '#e64c35'),
    #            size = 4)
    geom_point(data = main2,
               aes(x = Altitude, y = Race),
               color = '#001858') + 
    theme_classic()
  })
  
  # pie chart
  main %>%
    select(Race) %>%
    group_by(Race) %>%
    mutate(Race = if_else(nchar(Race)==0, "Unspecified", Race)) %>%
    summarise(Count = n()) %>%
    mutate(Race = if_else(Count<4, "Others", Race)) %>%
    group_by(Race) %>%
    summarize(Count = if_else(Race=="Others", sum(Count), Count)) %>%
    summarize(Count = max(Count)) %>%
    group_by(Race) -> racemod
  
  output$racepie <- renderPlot(
    pie(
      x = racemod$Count,
      labels = racemod$Race,
      radius = 1,
      init.angle = 315
    )
  )
  
  main %>%
    select(Species) %>%
    group_by(Species) %>%
    mutate(Species = if_else(nchar(Species)==0, "Unspecified", Species)) %>%
    summarise(Count = n()) -> speciesmod
  
  output$speciespie <- renderPlot(
    pie(
      x = speciesmod$Count,
      labels = speciesmod$Species,
      radius = 1,
      init.angle = 0
    )
  )
  
  observeEvent(input$clearFilters, {
    updateSelectInput(session, "Species", selected = "")
    updateSelectInput(session, "subspecies", selected = "")
    updateSelectInput(session, "race", selected = "")
    updateSelectInput(session, "gen", selected = "")
    updateSelectInput(session, "F1", selected = "")
    updateSelectInput(session, "BC1", selected = "")
  })
  
  
  filteredData <- reactive({
    filtered <- read.csv(file="All.csv")
    if (!is.null(input$Species) && input$Species != "")
      filtered <- filtered[filtered$Species == input$Species, ]
    if (!is.null(input$subspecies) && input$subspecies != "")
      filtered <- filtered[filtered$subspecies == input$subspecies, ]
    if (!is.null(input$race) && input$race != "")
      filtered <- filtered[filtered$race == input$race, ]
    if (!is.null(input$gen) && input$gen != "")
      filtered <- filtered[filtered$gen == input$gen, ]
    if (!is.null(input$F1) && input$F1 != "")
      filtered <- filtered[filtered$F1 == input$F1, ]
    if (!is.null(input$BC1) && input$BC1 != "")
      filtered <- filtered[filtered$BC1 == input$BC1, ]
    return(filtered)
  })
  
  output$filteredTableDT <- renderDT(
    filteredData(),
    options = list(pageLength = 15, scrollX = TRUE, scrollY = "800px")
  )

  # router$server(input, output, session)
}


shinyApp(ui, server)
