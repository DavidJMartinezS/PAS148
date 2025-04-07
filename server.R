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
  
  # datos ----
  comunas <- reactive({
    read_sf(system.file("Comunas.gdb", package = "dataPAS")) %>% st_transform(crs())
  })
  red_hidro <- reactive({
    read_sf(system.file("Red_hidrografica.gdb", package = "dataPAS")) %>% st_transform(crs())
  })
  red_vial <- reactive({
    read_sf(system.file("Red_vial.gdb", package = "dataPAS")) %>% st_zm() %>% st_transform(crs())
  })
  provincia <- reactive({
    req(input$provincia)
    comunas() %>% filter(PROVINCIA == input$provincia) %>% group_by(PROVINCIA) %>% summarise(geometry = st_union(geometry))
  })
  
  # Inputs ----
  crs <- reactive({
    req(LB())
    sf_use_s2(F)
    crs <- if_else(
      LB() %>% st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_centroid() %>% st_coordinates() %>% .[,1] >= -72,
      32719, 
      32718
    )
    sf_use_s2(T)
    return(crs)
  })
  LB <- leer_sf(id = "linea_base", fx = function(x){
    x %>% 
      rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) %>% 
      rename_if(names(.) %>% stri_cmp_equiv("pid", strength = 1), ~ "PID") %>% 
      rename_if(names(.) %>% stri_detect_regex("^tipo.*for", case_insensitive = T), ~ "Tipo_fores") %>% 
      rename_if(names(.) %>% stri_detect_regex("^sub.*tipo.*fo", case_insensitive = T), ~ "Subtipo_fo") %>% 
      rename_if(names(.) %>% stri_detect_regex("ley.*20283", case_insensitive = T), ~ "Regulacion") 
  })
  observeEvent(LB(),{
    if(!all(c('PID','Tipo_fores', 'Subtipo_fo', 'Regulacion') %in% names(LB()))){
      shinyalerta(names_act = names(st_drop_geometry(LB())), names_req = c('PID', 'Tipo_fores', 'Subtipo_fo', 'Regulacion'))
    } else {
      notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
    }
  })
  
  obras <- leer_sf(id = "obras", crs = crs(), fx = function(x){
    x %>% rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII"))))
  })
  observeEvent(obras(),{
    if(!all(c('Obra', 'Temporal') %in% names(obras()))){
      shinyalerta(names_act = names(st_drop_geometry(obras())), names_req = c('Obra', 'Temporal'))
    } else {
      notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
    }
  })
  
  predios <- leer_sf(id = "predios", crs = crs(), fx = function(x){
    x %>% 
      rename_if(names(.) %>% stri_cmp_equiv("n_predio", strength = 1), ~ "N_Predio") %>% 
      rename_if(names(.) %>% stri_cmp_equiv("nom_predio", strength = 1), ~ "Nom_Predio") %>% 
      rename_if(names(.) %>% stri_cmp_equiv("rol", strength = 1), ~ "Rol") %>% 
      rename_if(names(.) %>% stri_detect_regex("^prop", case_insensitive = T), ~ "Propietari")
  })
  observeEvent(predios(),{
    if(!all(c('N_Predio','Nom_Predio','Rol','Propietari') %in% names(predios()))){
      shinyalerta(names_act = names(st_drop_geometry(predios())), names_req = c('N_Predio', 'Nom_Predio', 'Rol', 'Propietari'))
    } else {
      notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
    }
  })
  
  suelos <- leer_sf(id = "suelos", crs = crs(), fx = function(x){
    x %>% 
      rename_if(names(.) %>% stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "Clase_uso") 
  })
  observeEvent(suelos(),{
    if(!all(c('Clase_uso') %in% names(suelos()))){
      shinyalerta(shinyalerta(names_act = names(st_drop_geometry(suelos())), names_req = c('Clase_uso')))
    } else {
      notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
    }
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
      choices = names(LB())[!names(LB()) == "geometry"]
    )
  })
  
  observeEvent(input$cut_by_prov,{
    output$select_prov_UI <- renderUI({
      if (input$cut_by_prov) {
        pickerInput(
          inputId = "provincia",
          label = "Seleccione provincia", 
          choices = provincias_list,
          selected = NULL
        )
      }
    })
  })
  
  observeEvent(input$ord_rodales,{
    output$ord_rodales_UI <- renderUI({
      if (input$ord_rodales) {
        pickerInput(
          inputId = "orden_rodales",
          label = "Ordenar de:",
          choices = c("NS-EO","NS-OE","SN-EO","SN-OE","EO-NS","EO-SN","OE-NS","OE-SN"),
          selected = "NS-OE"
        )
      }
    })
  })
  
  shinyjs::disable("get_area")
  observe({
    req(c(LB(), obras(), predios(), suelos()))
    shinyjs::enable("get_area")
  })

  areas_prop <- eventReactive(input$get_area,{
    req(LB(), obras(), predios(), suelos())
    get_rod_area(
      LB = LB(), 
      obras = obras(), 
      predios = predios(), 
      suelos = suelos(), 
      group_by_LB = input$group_by_LB, 
      sep_by_CUS = input$sep_by_CUS, 
      group_by_dist = input$group_by_dist, 
      distance_max = distance(),
      cut_by_prov = input$cut_by_prov,
      provincia = provincia(),
      n_rodal_ord = input$ord_rodales,
      orden_rodal = input$orden_rodales
    )
  })
  
  observeEvent(input$get_area,{
    req(LB(), obras(), predios(), suelos())
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(), p("Generando rodales y áreas de corta.", br(), " Por favor espere, esto puede tardar unos minutos"))
    )
    req(areas_prop())
    remove_modal_spinner()
  })
  
  downfile(id = "down_areas", x = areas_prop(), name_save = c("Rodales_propuestos","Areas_propuestas","Predios_propuestos"))
  
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
  
  shinyjs::disable("apply_order")
  observe({
    req(shp_to_order())
    shinyjs::enable("apply_order")
  })
  
  shp_ordered <- eventReactive(input$apply_order,{
    req(shp_to_order())
    shp_to_order() %>% 
      {if(!is.null(input$select_field_order)) group_by(., !!!syms(input$select_field_order)) else .} %>% 
      mutate(ID_ord = st_order(geometry, order = input$orden)) 
  })
  
  observeEvent(input$apply_order,{
    req(shp_to_order())
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(), p("Generando campo 'ID_ord' con la numeración.", br(), " Por favor espere, esto puede tardar un poco"))
    )
    req(shp_ordered())
    remove_modal_spinner()
    notify_success("Shapefile ordenado!", timeout = 3000, position = "right-bottom")
  })
  
  downfile(id = "down_sf_ordered", x = shp_ordered(), name_save = str_c(shp_to_order_name(),"_ord"))
  
  # Chequeo de cartografía ----
  shp_check <- leer_sf(id = "sf_check")
  
  shinyjs::disable("check_carto")
  observe({
    req(shp_check(), input$select_sf_check)
    shinyjs::enable("check_carto")
  })
  
  observeEvent(input$check_carto,{
    req(shp_check(), input$select_sf_check)
    check_carto_nueva(x = shp_check(), id = input$select_sf_check)
  })
  
  # Obtener y descargar cartografía digital ----
  crs_carto <- reactive({
    ifelse(input$huso == "18S", 32718, 32719)
  })
  # AREAS DE CORTA
  areas_def <- leer_sf(id = "cart_area", crs = crs_carto(), fx = function(x){
    x %>%
      rename_if(names(.) %>% stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "Clase_Uso") %>% 
      rename_if(names(.) %>% stri_detect_regex("n_a|n_area", case_insensitive = T), ~ "N_Area") %>% 
      rename_if(names(.) %>% stri_detect_regex("nom_predio", case_insensitive = T), ~ "Nom_Predio") %>% 
      mutate(Tipo_Bos = "BN")
  })
  observeEvent(areas_def(),{
    if(!all(c('Nom_Predio', 'N_Area', 'Clase_Uso') %in% names(areas_def()))){
      shinyalerta(names_act = names(areas_def()), names_req = c('Nom_Predio', 'N_Area'))
    }
    if((areas_def() %>% st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_centroid() %>% st_coordinates() %>% .[,1] >= -72 & input$huso == "18S") |
       (areas_def() %>% st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_centroid() %>% st_coordinates() %>% .[,1] < -72 & input$huso == "19S")){
      shinyalert(
        title = "Ups!", 
        text = "Coordenadas del shp no coinciden con la seleccionada",
        type = "error",
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = T
      )
    }
  })
  # n_rca_areas <- RCA_SRV("rca_areas")
  
  # RODALES
  rodales_def <- leer_sf(id = "cart_rodales", crs = crs_carto(), fx = function(x){x %>% mutate(Tipo_Bos = "BN")})
  observeEvent(rodales_def(),{
    if(!all(c('N_Rodal', 'Tipo_For') %in% names(rodales_def()))){
      shinyalerta(names_act = names(rodales_def()), names_req = c('N_Rodal', 'Tipo_For'))
    }
  })
  # n_rca_rodales <- RCA_SRV("rca_rodales")
  
  # PREDIOS
  predios_def <- leer_sf(id = "cart_predios", crs = crs_carto())
  observeEvent(predios_def(),{
    if(!all(c('N_Predio', 'Nom_Predio', 'Rol') %in% names(predios_def()))){
      shinyalerta(names_act = names(predios_def()), names_req = c('N_Predio', 'Nom_Predio', 'Rol'))
    }
  })
  
  # CAMINOS 
  observeEvent(input$add_cam,{
    output$add_cam_ui <- renderUI({
      if(input$add_cam){
        div(
          div(
            id = "flex",
            div(id = "inline", pickerInput(inputId = "cut_cam", label = "Corte", choices = c("clip", "buffer", "crop", "crop_by_row"), selected = "clip")),
            div(id = "inline", numericInput(inputId = "buffer_cam", label = "Buffer", value = 0, step = 10, width = "100px"), style = "margin-left: 25px;"),
            div(tags$b("m"), style = "margin-top: 10px;"),
            style = "margin-top: -10px; margin-bottom: 5px"
          ),
          p("Caminos serán creados a partir de la red vial del MOP actualizado al 07-02-2024 (descargar ",
            a("aqui", .noWS = "outside", href = "https://mapas.mop.gov.cl/red-vial/Red_Vial_Chile.zip"),
            ") ¿Desea crear otra capa de caminos a partir de información de Google?"
          ),
          switchInput(
            inputId = "add_cam_osm",
            size = "mini",
            onLabel = "Si",
            offLabel = "No",
            onStatus = "success"
          )
        )
      }
    })
  })
  
  # HIDROGRAFIA
  observeEvent(input$add_hidro,{
    output$add_hidro_ui <- renderUI({
      if(input$add_hidro){
        div(
          div(
            id = "flex",
            div(id = "inline", pickerInput(inputId = "cut_hidro", label = "Corte", choices = c("clip", "buffer", "crop", "crop_by_row"), selected = "clip")),
            div(id = "inline", numericInput(inputId = "buffer_hidro", label = "Buffer", value = 0, step = 10, width = "100px"), style = "margin-left: 25px;"),
            div(tags$b("m"), style = "margin-top: 10px;"),
            style = "margin-top: -10px; margin-bottom: 5px"
          ),
          p("Hidrografía será creada a partir de la hidrografía subida a Geoportal actualizada al 31-12-2022 (link ", 
            a("aquí", .noWS = "outside", href = "https://www.geoportal.cl/geoportal/catalog/36436/Hidrograf%C3%ADa%20de%20la%20regi%C3%B3n%20de%20Arica%20a%20la%20regi%C3%B3n%20de%20Los%20Lagos"),
            ") ¿Desea crear capa hidrografíca a partir de información de Google? (De lo contrario )"
          ),
          switchInput(
            inputId = "add_hidro_osm",
            size = "mini",
            onLabel = "Si",
            offLabel = "No",
            onStatus = "success"
          )
        )
      }
    })
  })
  
  # CURVAS DE NIVEL
  observeEvent(input$add_CN,{
    output$add_CN_ui <- renderUI({
      if(input$add_CN){
        div(
          id = "flex",
          div(
            id = "inline", 
            pickerInput(inputId = "cut_hidro", label = "Corte", choices = c("clip", "buffer", "crop", "crop_by_row"), selected = "clip")
          ),
          div(
            id = "inline", 
            numericInput(inputId = "buffer_hidro", label = "Buffer", value = 0, step = 10, width = "50px"),
            style = "margin-left: 20px;"
          ),
          div(tags$b("m"), style = "margin-top: 8px;"),
          div(
            id = "inline",
            numericInputIcon(inputId = "step", label = "Intervalo", value = 10, step = 5, icon = icon("ruler-horizontal"), width = "50px"),
            style = "margin-left: 20px;"
          ),
          style = "margin-top: -8px;"
        )
      }
    })
  })
  
  # USO ACTUAL
  observeEvent(input$add_uso_actual,{
    output$add_uso_actual_ui <- renderUI({
      if(input$add_uso_actual){
        div(
          leer_sfUI("catastro", "Ingrese capa del catastro de CONAF") %>% 
            add_help_text(title = "Campos minimos requeridos:\n'USO', 'SUBUSO', 'ESTRUCTURA'"),
          div(style = "margin-top: -10px"),
          leer_sfUI("suelos_uso_act", "Ingrese capa de suelos del CIREN") %>% 
            add_help_text(title = "Campos minimos requeridos:\n'TEXTCAUS'")
        )
      }
    })
  })
  
  catastro <- leer_sf(
    id = "catastro", 
    crs = crs_carto(), 
    fx = function(x){x %>% rename_all(~ if_else(. == "geometry", ., str_to_upper(stri_trans_general(.,"Latin-ASCII"))))},
    wkt_filter = st_as_text(st_geometry(predios_def() %>% st_union()))
  )
  observeEvent(catastro(),{
    if(!all(c('USO', 'SUBUSO', 'ESTRUCTURA') %in% names(catastro()))){
      shinyalerta(names_act = names(catastro()), names_req = c('USO', 'SUBUSO', 'ESTRUCTURA'))
    }
  })
  suelos_uso_act <- leer_sf(
    id = "suelos_uso_act", 
    crs = crs_carto(),
    fx = function(x){x %>% rename_all(~ if_else(. == "geometry", ., str_to_upper(stri_trans_general(.,"Latin-ASCII"))))},
    wkt_filter = st_as_text(st_geometry(predios_def() %>% st_union()))
  )
  observeEvent(suelos_uso_act(),{
    if(!all(c('TEXTCAUS') %in% names(suelos_uso_act()))){
      shinyalerta(names_act = names(suelos_uso_act()), names_req = c('TEXTCAUS'))
    }
  })
  
  # DEM
  DEM <- reactive({
    req(input$dem)
    dem <- read_stars(input$dem) 
    names(dem) <- "elev"
    return(dem)
  })
  
  # NOMBRE PREDIO
  iv <- InputValidator$new()
  iv$add_rule("NOMPREDIO", sv_required())
  iv$enable()
  
  # CARTOGRAFÍA DIGITAL
  carto_digital <- eventReactive(input$get_carto_btn,{
    req(c(area_def(), rodales_def(), predios_def(), input$dem, input$get_carto_btn))
    get_carto_digital(
      areas = areas_def(), 
      rodales = rodales_def(), 
      predios = predios_def(), 
      tipo_for = input$tipo_for,
      DEM = DEM()
    )
  })
  
  observeEvent(input$get_carto_btn,{
    req(c(area_def(), rodales_def(), predios_def(), input$dem, input$get_carto_btn))
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(),p("Generando cartografía digital.",br()," Por favor espere, esto puede tardar unos minutos"))
    )
    req(carto_digital())
    remove_modal_spinner()
    output$down_carto_ui <- renderUI({
      downUI("down_carto")
    })
  })
  
  downfile(
    id = "down_carto", 
    x = carto_digital(), 
    name_save = c("Area", "Rodales", "Suelos", "Rangos_pend", "Caminos", "Hidrografia","Curvas_niv", "Limite_Predial", "Uso_actual")
  )
  
  # ANALISIS METODOLOGICO ----
  
})




