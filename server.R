shinyServer(function(input,output,session){
  # Author ----
  output$user <- renderUser({
    dashboardUser(
      name = "David Martínez",
      image = "https://avatars.githubusercontent.com/u/74486319?s=400&u=c277213b232af5e7710bebdc7a50bb9426ab9a62&v=4",
      title = "Dashboard PAS 148",
      subtitle = "Autor",
      footer = fluidRow(
        tags$p("Mi Github", socialButton(href = "https://github.com/DavidJMartinezS", icon = icon("github")), class = "text-center"),
        tags$p("Geobiota", socialButton(href = "https://google.com", icon = icon("globe")), class = "text-center"),
      ),
      "Especialista en plantas de Geobiota"
    )
  })
  
  LB <- callModule(module = leer_sf, id = "linea_base")
  obras <- callModule(module = leer_sf, id = "obras")
  predios <- callModule(module = leer_sf, id = "predios")
  hidro <- callModule(module = leer_sf, id = "hidro")
  caminos <- callModule(module = leer_sf, id = "caminos")
  DEM <- reactive({
    req(input$dem)
    read_stars(input$dem)
  })
  BN <- reactive({
    req(LB())
    LB() %>% 
      filter(regulacion = "Bosque nativo")
  })
  
  output$leaf_inputs <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group ="CartoDB.Positron") %>%
      addProviderTiles(providers$OpenStreetMap, group ="OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group ="Esri.WorldImagery") %>%
      addProviderTiles(providers$Esri.DeLorme, group ="Esri.DeLorme") %>%
      addPolygons(
        data = BN() %>% st_transform(4326),
        fillColor = "#2ECC71",
        fillOpacity = 0.2,
        group = "LB",
        weight = 1,
        color = "gray40",
        highlightOptions = highlightOptions(
          color = "gray20",
          weight = 2.5,
          bringToFront = TRUE,
        )
      ) %>%
      addPolygons(
        data = obras() %>% st_transform(4326),
        fillColor = "#EDBB99",
        fillOpacity = 0.3,
        group = "BN",
        weight = 1,
        color = "gray40",
        highlightOptions = highlightOptions(
          color = "gray20",
          weight = 2.5,
          bringToFront = TRUE,
        )
      ) %>%
      addPolygons(
        data = predios() %>%
          st_transform(4326),
        fillColor = "#20854EFF",
        fillOpacity = 0.9,
        group = "BNP",
        weight = 1,
        color = "gray40",
        highlightOptions = highlightOptions(
          color = "gray20",
          weight = 2.5,
          bringToFront = TRUE,
        )
      ) %>%
      addPolygons(
        data = predios() %>% st_transform(4326),
        fillColor = "transparent",
        weight = 1,
        color = "#F1C40F",
        dashArray = "10,5"
      ) %>%
      addFullscreenControl(position = "topleft") %>%
      addResetMapButton() %>%
      addMiniMap(
        position = "topright",
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE,
        minimized = FALSE
      ) %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters"
      ) %>%
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(imperial = FALSE)
      ) %>%
      addLegend(
        title = "Legend",
        colors = c("#FFDC91FF", "#84BD00FF","#20854EFF","#00A087FF"),
        labels = c("Formaciones Xerofíticas", "Bosque Nativo","Bosque Nativo de Preservación","Cuenca"),
        opacity = c(1,1,1,1),
        group = 'legend',
        position = "bottomleft"
      ) %>%
      addLayersControl(
        overlayGroups = c("FX","BN","BNP","legend"),
        baseGroups = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Esri.DeLorme"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
})