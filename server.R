shinyServer(function(input,output,session){
  # Author ----
  output$user <- renderUser({
    dashboardUser(
      name = "David Martínez",
      image = "https://avatars.githubusercontent.com/u/74486319?s=400&u=c277213b232af5e7710bebdc7a50bb9426ab9a62&v=4",
      title = "Dashboard PAS 148",
      subtitle = "Autor",
      footer = fluidRow(
        tags$p(
          socialButton(href = "https://github.com/DavidJMartinezS", icon = icon("github")) %>% bs_embed_tooltip("Mi github"),
          socialButton(href = "https://geobiota.com/", icon = icon("globe")) %>% bs_embed_tooltip("Geobiota"), 
          class = "text-center"
        ),
      ),
      "Especialista en plantas de Geobiota"
    )
  })
  
  # Inputs ----
  crs <- reactive({
    ifelse(input$huso == "18S",32718,32719)
  })
  LB <- leer_sf(id = "linea_base", crs = crs(), fx = function(x){
    x %>% rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) %>% st_transform(crs())
  })
  observeEvent(LB(),{
    if(!all(c('Tipo_for', 'Subtipo_fo', 'Regulacion') %in% names(LB()))){
      shinyalerta()
    }
  })
  obras <- leer_sf(id = "obras", crs = crs(), fx = function(x){
    x %>% rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) %>% st_transform(crs())
  })
  observeEvent(obras(),{
    if(!all(c('Obra', 'Tipo') %in% names(obras()))){
      shinyalerta()
    }
  })
  predios <- leer_sf(id = "predios", crs = crs(), fx = function(x){
    x %>% st_transform(crs())
  })
  observeEvent(predios(),{
    if(!all(c('N_Predio','Nom_Predio','Rol','Prop') %in% names(predios()))){
      shinyalerta()
    }
  })
  suelos <- leer_sf(id = "suelos", crs = crs(), fx = function(x){
    x %>% rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) %>% st_transform(crs())
  })
  observeEvent(predios(),{
    if(!all(c('Textcaus') %in% names(suelos()))){
      shinyalerta()
    }
  })
  
  BN <- reactive({
    req(LB())
    LB() %>% filter(regulacion = "Bosque nativo")
  })
  
  # Generar áreas de corta ----
  observeEvent(input$group_by_dist,{
    output$distanceUI <- renderUI({
      if (input$group_by_dist) {
        numericInputIcon(
          inputId = "distance",
          label = "Distancia",
          value = 50,
          step = 5,
          icon = icon("ruler-horizontal")
        )
      }
    })
  })
  
  distance <- reactive({
    req(input$group_by_dist)
    if (input$group_by_dist) {
      input$distance
    } else {
      NULL
    }
  })
  
  observeEvent(LB(),{
    updatePickerInput(
      session = session,
      inputId = "group_by_LB",
      choices = names(LB())
    )
  })
  
  areas_prop <- eventReactive(input$get_area,{
    req(LB(),obras(),predios(),suelos())
    get_rod_area(
      LB = LB(), 
      obras = obras(), 
      predios = predios(), 
      suelos = suelos(), 
      group_by_LB = input$group_by_LB, 
      sep_by_CUS = input$sep_by_CUS, 
      group_by_dist = input$group_by_dist, 
      distance_max = distance()
    )
  })
  
  observeEvent(input$get_area,{
    req(LB(),obras(),predios(),suelos())
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(),p("Generando rodales y áreas de corta.",br()," Por favor espere, esto puede tardar unos minutos"))
    )
    req(areas_prop())
    remove_modal_spinner()
  })
  
  down(id = "down_areas", x = areas_prop(), name_save = list("Rodales_propuestos","Areas_propuestas","Predios_propuestos"), filetype = "sf")
  
  # Ordenar shapefile ----
  shp_to_order <- leer_sf("sf_order")
  shp_to_order_name <- leer_sf("sf_order", path = T)
  
  observeEvent(shp_to_order(),{
    updatePickerInput(
      session = session,
      inputId = "select_field_order",
      choices = names(shp_to_order())
    )
  })
  
  observeEvent(input$apply_order,{
    req(shp_ordered())
    notify_success("Shapefile ordenado!",timeout = 1500,position = "right-bottom")
  })
  
  shp_ordered <- eventReactive(input$apply_order,{
    req(shp_to_order())
    shp_to_order() %>% 
      mutate(ID_ord = st_order(geometry)) 
  })
  
  observeEvent(input$apply_order,{
    req(shp_to_order())
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(),p("Generando campo 'ID_ord' con la numeración.",br()," Por favor espere, esto puede tardar un poco"))
    )
    req(shp_ordered())
    remove_modal_spinner()
  })
  
  down(id = "down_sf_ordered", x = shp_to_order(), name_save = shp_to_order_name(), filetype = "sf")
  
  # Chequeo de cartografía ----
  shp_check <- leer_sf(id = "sf_check")
  observeEvent(input$check_carto,{
    req(shp_check(), input$select_sf_check)
    check_carto(x = shp_check(), id = input$select_sf_check)
  })
  
  # Obtener y descargar cartografía digital ----
  area_def <- leer_sf(id = "cart_area", crs = crs(), fx = function(x){x %>% mutate(Tipo_Bos = "BN")})
  observeEvent(area_def(),{
    if(!all(c('Nom_Predio', 'N_a', 'Sup_ha') %in% names(area_def()))){
      shinyalerta()
    }
  })
  
  rodales_def <- leer_sf(id = "cart_rodales", crs = crs(), fx = function(x){x %>% mutate(Tipo_Bos = "BN")})
  observeEvent(rodales_def(),{
    if(!all(c('N_Rodal', 'Tipo_For', 'Sup_ha') %in% names(rodales_def()))){
      shinyalerta()
    }
  })
  
  predios_def <- leer_sf(id = "cart_predios", crs = crs())
  observeEvent(predios_def(),{
    if(!all(c('N_Predio', 'Nom_Predio', 'Rol') %in% names(predios_def()))){
      shinyalerta()
    }
  })
  
  hidro <- leer_sf(id = "hidro", crs = crs(), fx = function(x){
    x %>% rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII"))))
  })
  observeEvent(hidro(),{
    if(!all(c('Nombre', 'Tipo', 'Permanencia') %in% names(hidro()))){
      shinyalerta()
    }
  })
  DEM <- reactive({
    req(input$dem)
    dem <- read_stars(input$dem) 
    names(dem) <- "elev"
    return(dem)
  })
  
  observeEvent(input$get_carto_btn,{
    req(c(area_def(),rodales_def(),predios_def()))
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(),p("Generando cartografía digital.",br()," Por favor espere, esto puede tardar unos minutos"))
    )
    req(carto_digital())
    remove_modal_spinner()
    # output$down_carto_ui <- renderUI({
    #   if (input$get_carto_btn){
    #     tags$div(
    #       tags$hr(),
    #       tags$h5("Ya puede descargar su cartografía digital!! Seleccione el directorio y luego descargue su cartografía", 
    #               style = "font-weight: bold;"),
    #       splitLayout(
    #         shinyDirButton(
    #           "directory",
    #           label = NULL,
    #           title = "Select directory",
    #           multiple = FALSE,
    #           icon = icon("folder"),
    #           viewtype = "detail",
    #           style = "padding: 10px 12px;background-color: #008CBA;border-radius: 12px;"
    #         ), 
    #         downloadBttn(
    #           outputId = "down_carto_btn",
    #           label = NULL,
    #           style = "material-circle",
    #           size = "sm",
    #           color = "warning",
    #           disabled = T
    #         ), 
    #         verbatimTextOutput("dir_out_output"), 
    #         cellWidths = c("10%","10%", "80%")
    #       )
    #     )
    #   }
    # })
  })
  
  # roots <- c(wd = path.expand("~"))
  # shinyDirChoose(
  #   input,
  #   id = "directory",
  #   roots = roots,
  #   updateFreq = 0,
  #   session,
  #   defaultPath = "",
  #   defaultRoot = NULL,
  #   allowDirCreate = TRUE
  # )
  # 
  # observeEvent(input$directory, {
  #   updateActionButton(
  #     session,
  #     "down_carto_btn", 
  #     disabled = c(TRUE, FALSE)[((all(c("root", "path") %in% names(input$directory))) %% 2) + 1])
  # })
  # 
  # directorio <- reactive({
  #   if(all(c("root", "path") %in% names(input$directory))){
  #     selected_path <- do.call(file.path, c(roots[input$directory$root], input$directory$path))
  #   } else {
  #     selected_path <- nullfile()
  #   }
  #   return(selected_path)
  # })
  # 
  # output$dir_out_output <- renderPrint({
  #   if (dir.exists(directorio())) {
  #     directorio()
  #   } else {
  #     "Directorio no seleccionado"
  #   }
  # })
  # 
  # observeEvent(input$down_carto_btn,{
  #   req(directorio())
  #   temp_dir <- tempdir()
  #   
  #   show_modal_spinner(
  #     spin = "flower",
  #     color = "#35978F",
  #     text = div(br(),p("Descargando cartografía digital.",br()," Por favor espere, esto puede tardar unos segundos"))
  #   )
  #   down_carto_digital(carto_digital(), temp_dir, input)
  #   remove_modal_spinner()
  #   
  #   zip_file <- file.path(temp_dir, "Compilado_Carto_digital.zip")
  #   list_files <- list.files(temp_dir,
  #                            ".dbf$|.prj$|.shp$|.shx$|.kml&|.xlsx$",
  #                            full.names = TRUE)
  #   zip::zipr(zipfile = zip_file, files = Sys.glob(list_files))
  #   file.copy(zip_file, directorio(),overwrite = T)
  #   if (length(Sys.glob(list_files)) > 0) file.remove(Sys.glob(list_files))
  #   shinyalert::shinyalert(
  #     title = "Listo!", 
  #     text = paste0("Su cartografía digital ha sido descargada en:\n", directorio()),
  #     type = "success",
  #     closeOnEsc = T, 
  #     showConfirmButton = T,
  #     animation = TRUE
  #   )
  # })
  
  # Crear accesos ----
  map<-leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$OpenStreetMap,
                     options = tileOptions(minZoom = 2, maxZoom = 15)) %>%
    addProviderTiles(providers$Esri.WorldImagery,
                     options = tileOptions(minZoom = 15, maxZoom = 20),
                     group = "Esri.WorldImagery") %>%
    addPmToolbar(targetGroup = "name_long",
                 toolbarOptions = pmToolbarOptions(drawMarker = T,
                                                   drawPolygon = F,
                                                   drawPolyline = F,
                                                   drawCircle = F,
                                                   drawRectangle = F,
                                                   editMode = F,
                                                   cutPolygon = F,
                                                   removalMode = TRUE,
                                                   position = "topleft"))
  
  observeEvent(input$get_access,{
    showModal(editModUI("editor"))
  })
  edits <- callModule(editMod,
                      "editor",
                      targetLayerId = "layerId",
                      leafmap = map)
  
  ns <- shiny::NS("editor")
  
  observe({
    req(predios())
    proxy.lf <- leafletProxy(ns("map"))
    
    # bounds <- predios() %>%
    #   st_bbox() %>%
    #   as.character()
    
    proxy.lf %>%
      # fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      leaflet::addPolygons(data = predios(),
                           weight = 3,
                           opacity = 1,
                           fill = FALSE,
                           color = 'red',
                           fillOpacity = 1,
                           smoothFactor = 0.01,
                           group = "name_long")
  })
  
  polygons<-reactive({
    req(edits()$finished)
    edits()$finished
    #edits()$edited
    # edits()$deleted
  })
  
  output$plot<-renderPlot({
    req(polygons())
    plot(polygons())
  })
})




