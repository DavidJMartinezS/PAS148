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
      notify_success("Perfecto!", timeout = 1500, position = "right-bottom")
    }
  })
  
  obras <- leer_sf(id = "obras", crs = crs(), fx = function(x){
    x %>% rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII"))))
  })
  observeEvent(obras(),{
    if(!all(c('Obra', 'Temporal') %in% names(obras()))){
      shinyalerta(names_act = names(st_drop_geometry(obras())), names_req = c('Obra', 'Temporal'))
    } else {
      notify_success("Perfecto!", timeout = 1500, position = "right-bottom")
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
      notify_success("Perfecto!", timeout = 1500, position = "right-bottom")
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
      notify_success("Perfecto!", timeout = 1500, position = "right-bottom")
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
  
  observeEvent(input$apply_order,{
    req(shp_ordered())
    notify_success("Shapefile ordenado!", timeout = 3000, position = "right-bottom")
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
    check_carto(x = shp_check(), id = input$select_sf_check)
  })
  
  # Obtener y descargar cartografía digital ----
  areas_def <- leer_sf(id = "cart_area", crs = crs(), fx = function(x){x %>% mutate(Tipo_Bos = "BN")})
  observeEvent(areas_def(),{
    if(!all(c('Nom_Predio', 'N_a', 'Sup_ha') %in% names(areas_def()))){
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
    if(!all(c('Nombre', 'Tipo', 'Perma') %in% names(hidro()))){
      shinyalerta()
    }
  })
  DEM <- reactive({
    req(input$dem)
    dem <- read_stars(input$dem) 
    names(dem) <- "elev"
    return(dem)
  })
  
  iv <- InputValidator$new()
  iv$add_rule("NOMPREDIO", sv_required())
  iv$enable()
  
  carto_digital <- eventReactive(input$get_carto_btn,{
    req(c(area_def(), rodales_def(), predios_def(), input$dem, input$get_carto_btn))
    get_carto_digital(
      areas = areas_def(), 
      rodales = rodales_def(), 
      predios = predios_def(), 
      tipo_for = input$tipo_for
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
  
  downfile(id = "down_carto", x = carto_digital(), name_save = c("Area","Rodales","Suelos","Caminos","Hidrografia","Curvas_niv","Limite_Predial","Uso_actual","Rangos_pend"))
  
  # Crear accesos ----
  
})




