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
  
  # DATOS ----
  comunas <- eventReactive(crs(),{
    read_sf(
      system.file("Comunas.gdb", package = "dataPAS")
    ) %>% st_transform(crs()) %>% st_set_geometry("geometry")
  })
  red_hidro <- reactive({
    read_sf(system.file("Red_hidrografica.gdb", package = "dataPAS")) %>% st_transform(crs())
  })
  red_vial <- reactive({
    read_sf(system.file("Red_vial.gdb", package = "dataPAS")) %>% st_zm() %>% st_transform(crs())
  })
  provincia <- reactive({
    req(comunas())
    comunas %>%  
      filter(PROVINCIA == input$provincia) %>% 
      group_by(PROVINCIA) %>% summarise(geometry = st_union(geometry))
  })
  
  # INPUTS ----
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
      rename_if(names(.) %>% stri_detect_regex("^tipo.*veg", case_insensitive = T), ~ "Tipo_veg") %>% 
      rename_if(names(.) %>% stri_detect_regex("^sub.*tipo.*fo", case_insensitive = T), ~ "Subtipo_fo") %>% 
      rename_if(names(.) %>% stri_detect_regex("ley.*20283", case_insensitive = T), ~ "Regulacion") 
  })
  observeEvent(LB(),{
    if(!all(c('Tipo_fores', 'Subtipo_fo', 'Tipo_veg', 'Regulacion') %in% names(LB()))){
      shinyalerta(names_act = names(st_drop_geometry(LB())), names_req = c('Tipo_fores', 'Subtipo_fo', 'Tipo_veg', 'Regulacion'))
    } else {
      notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
    }
    if ('PID' %in% names(LB())){
      notify_warning("Campo 'PID' creado", timeout = 3000, position = "right-bottom")
    } 
  })
  
  obras <- leer_sf(id = "obras", crs = crs())
  # observeEvent(obras(),{
  #   if(!all(c('Obra', 'Temporal') %in% names(obras()))){
  #     shinyalerta(names_act = names(st_drop_geometry(obras())), names_req = c('Obra', 'Temporal'))
  #   } else {
  #     notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
  #   }
  # })
  
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
      rename_if(names(.) %>% stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "Clase_Uso") 
  })
  observeEvent(suelos(),{
    if(!all(c('Clase_Uso') %in% names(suelos()))){
      shinyalerta(shinyalerta(names_act = names(st_drop_geometry(suelos())), names_req = c('Clase_Uso')))
    } else {
      notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
    }
  })
  
  # GENERAR ÁREAS DE CORTA ----
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
      text = div(br(), p("Generando capa de predios, rodales y áreas de corta.", br(), " Puede ir a prepararse un café, esto tardará unos minutos"))
    )
    req(areas_prop())
    remove_modal_spinner()
    gc(reset = T)
  })
  
  downfile(id = "down_areas", x = areas_prop(), name_save = c("Rodales_propuestos","Areas_propuestas","Predios_propuestos"))
  
  # ORDENAR SHAPEFILE ----
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
    gc(reset = T)
  })
  
  downfile(id = "down_sf_ordered", x = shp_ordered(), name_save = str_c(shp_to_order_name(),"_ord"))
  
  # CHEQUEO CARTOGRAFÍA ----
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
  
  # CARTOGRAFIA DIGITAL Y ANEXOS ----
  crs_carto <- reactive({
    ifelse(input$huso == "18S", 32718, 32719)
  })
  provincia_carto <- reactive({
    req(input$provincia_carto)
    read_sf(system.file("Comunas.gdb", package = "dataPAS")) %>% st_transform(crs()) %>% 
      filter(PROVINCIA == input$provincia_carto) %>% 
      group_by(PROVINCIA) %>% 
      summarise(geometry = st_union(geometry))
  })
  # AREAS DE CORTA ----
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
  
  # RODALES ----
  rodales_def <- leer_sf(id = "cart_rodales", crs = crs_carto(), fx = function(x){x %>% mutate(Tipo_Bos = "BN")})
  observeEvent(rodales_def(),{
    if(!all(c('N_Rodal', 'Tipo_For') %in% names(rodales_def()))){
      shinyalerta(names_act = names(rodales_def()), names_req = c('N_Rodal', 'Tipo_For'))
    }
    if((rodales_def() %>% st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_centroid() %>% st_coordinates() %>% .[,1] >= -72 & input$huso == "18S") |
       (rodales_def() %>% st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_centroid() %>% st_coordinates() %>% .[,1] < -72 & input$huso == "19S")){
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
  # n_rca_rodales <- RCA_SRV("rca_rodales")
  
  # PREDIOS ----
  predios_def <- leer_sf(id = "cart_predios", crs = crs_carto())
  observeEvent(predios_def(),{
    if(!all(c('N_Predio', 'Nom_Predio', 'Rol') %in% names(predios_def()))){
      shinyalerta(names_act = names(predios_def()), names_req = c('N_Predio', 'Nom_Predio', 'Rol'))
    }
    if((predios_def() %>% st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_centroid() %>% st_coordinates() %>% .[,1] >= -72 & input$huso == "18S") |
       (predios_def() %>% st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_centroid() %>% st_coordinates() %>% .[,1] < -72 & input$huso == "19S")){
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
  
  # CAMINOS ----
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
  
  # HIDROGRAFIA ----
  observeEvent(input$add_hidro,{
    output$add_hidro_ui <- renderUI({
      if(input$add_hidro){
        div(
          div(
            id = "flex",
            div(id = "inline", pickerInput(inputId = "fuente_hidro", label = "Fuente", choices = c("MOP", "BCN"), selected = "MOP")),
            div(id = "inline", pickerInput(inputId = "cut_hidro", label = "Corte", choices = c("clip", "buffer", "crop", "crop_by_row"), selected = "clip"), style = "margin-left: 25px;"),
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
  
  # DEM ----
  DEM <- reactive({
    req(input$dem$datapath)
    dem <- read_stars(input$dem$datapath) %>% `st_crs<-`(crs_carto()) %>% `names<-`(c("elev"))
    return(dem)
  })
  
  observeEvent(input$add_CN,{
    output$add_CN_ui <- renderUI({
      if (input$add_CN) {
        numericInputIcon(
          inputId = "step",
          label = "Intérvalo",
          value = 10,
          min = 10,
          max = 500,
          step = 10,
          icon = icon("ruler-horizontal")
        )
      }
    })
  })
  
  # BD FLORA ----
  bd_parcelas <- reactive({
    req(input$bd_parcelas$datapath)
    read_xlsx(input$bd_parcelas$datapath) %>% 
      rename_all(~str_to_sentence(stri_trans_general(.,"Latin-ASCII"))) %>% 
      rename_if(names(.) %>% stri_cmp_equiv("cob_bb", strength = 1), ~ "Cob_BB") %>% 
      rename_if(names(.) %>% stri_detect_regex("p500", case_insensitive = T), ~ "N_ind") %>% 
      rename_at(vars(contains("UTM")), str_to_upper) %>% 
      mutate_at(vars(contains("Cob_BB")), ~str_trim(str_to_lower(.)))
  })
  
  parcelas_rodales <- reactive({
    req(bd_parcelas(), rodales_def())
    bd_parcelas() %>% 
      mutate_at("N_ind", as.integer) %>% 
      filter(
        Habito %>% stri_trans_general("Latin-ASCII") %>% stri_detect_regex("arbol", case_insensitive = T),
        !Cob_BB %>% str_to_lower() %in% c(NA_character_, "fp", "---"),
        !N_ind %in% c(NA, 0)
      ) %>% 
      select(-matches("Nom_Predio|N_Rodal|Tipo_veg|Tipo_For|Subtipo_fo")) %>% 
      st_as_sf(coords = c("UTM_E","UTM_N"), crs = st_crs(rodales_def()), remove = F) %>%
      st_join(rodales_def() %>% select(Nom_Predio, N_Rodal, Tipo_For, Subtipo_fo, Tipo_veg)) %>% 
      mutate_at("N_Rodal", as.integer) %>% 
      st_drop_geometry() %>% 
      mutate_at("N_ind", as.integer) %>% 
      mutate(Nha = N_ind * 20) %>% 
      arrange(N_Rodal) %>% 
      group_by(Parcela, UTM_E, UTM_N) %>%
      arrange(N_Rodal) %>% 
      mutate(N = cur_group_id()) %>% 
      group_by(N_Rodal, N) %>% 
      mutate(N_Parc = cur_group_id()) %>% 
      ungroup()
  })
  
  observeEvent(parcelas_rodales(),{
    check_bd_flora(x = parcelas_rodales(), shinyalert = T)
  })
  
  # BD PCOB ----
  observeEvent(input$add_bd_pcob,{
    output$add_bd_pcob_ui <- renderUI({
      if(input$add_bd_pcob){
        div(
          fileInput(
            inputId = "bd_pcob",
            label = "Ingresar BD  de parcelas de cobertura",
            multiple = F,
            accept = c(".xlsx"),
            buttonLabel = "Seleccionar",
            placeholder = "Archivo no seleccionado"
          ) %>% 
            add_help_text(
              title = "Campos minimos requeridos:\n
              'Parcela', 'Especie', 'Copa_NS', 'Copa_EO'"
            ),
          div(style = "margin-top: -10px")
        )
      }
    })
  })
  
  bd_pcob <- reactive({
    req(input$bd_pcob$datapath)
    read_xlsx(input$bd_pcob$datapath) %>% 
      clean_names() %>% 
      rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) %>% 
      rename_if(names(.) %>% stri_cmp_equiv("copa_ns", strength = 1), ~ "Copa_NS") %>% 
      rename_if(names(.) %>% stri_cmp_equiv("copa_eo", strength = 1), ~ "Copa_EO") %>% 
      rename_if(names(.) %>% stri_detect_regex("diametro.*1", case_insensitive = T), ~ "Copa_NS") %>% 
      rename_if(names(.) %>% stri_detect_regex("diametro.*1", case_insensitive = T), ~ "Copa_EO") %>% 
      rename_at(vars(contains("UTM")), str_to_upper) %>% 
      mutate_at(vars(starts_with("Copa")), str_replace, "\\,", "\\.") %>% 
      mutate_at(vars(starts_with("Copa")), as.numeric)
  })
  
  # observeEvent(bd_pcob(),{
  #   check_bd_pcob(bd_pcob())
  # })
  
  # USO ACTUAL ----
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
    fx = function(x){x %>% rename_if(names(.) %>% stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "TEXTCAUS")},
    wkt_filter = st_as_text(st_geometry(predios_def() %>% st_union()))
  )
  observeEvent(suelos_uso_act(),{
    if(!all(c('TEXTCAUS') %in% names(suelos_uso_act()))){
      shinyalerta(names_act = names(suelos_uso_act()), names_req = c('TEXTCAUS'))
    }
  })
  
  # CARTOGRAFÍA DIGITAL ----
  shinyjs::disable("get_carto_btn")
  observe({
    req(c(areas_def(), rodales_def(), predios_def(), DEM()))
    shinyjs::enable("get_carto_btn")
  })
  carto_digital <- eventReactive(input$get_carto_btn,{
    req(c(areas_def(), rodales_def(), predios_def(), DEM()))
    get_carto_digital(
      PAS = 148,
      areas = areas_def(),
      rodales = rodales_def(),
      predios = predios_def(),
      TipoFor_num = input$tipo_for,
      dem = DEM(),
      add_parcelas = input$add_parcelas,
      bd_parcelas = bd_parcelas(),
      from_RCA = F,
      RCA = NULL,
      add_uso_actual = input$add_uso_actual,
      catastro = catastro(),
      suelos = suelos_uso_act(),
      add_caminos = input$add_cam,
      add_caminos_osm = input$add_cam_osm,
      caminos_arg = list(cut = input$cut_cam, buffer = input$buffer_cam),
      add_hidro = input$add_hidro,
      fuente_hidro = input$fuente_hidro,
      add_hidro_osm = input$add_hidro_osm,
      hidro_arg = list(cut = input$cut_hidro, buffer = input$buffer_hidro),
      add_curv_niv = input$add_CN,
      curv_niv_arg = list(cut = input$cut_curv_niv, buffer = input$buffer_curv_niv),
      step = input$step,
      dec_sup = input$n_dec
    )
  })
  
  observeEvent(input$get_carto_btn,{
    req(c(areas_def(), rodales_def(), predios_def(), DEM()))
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(),p("Generando cartografía digital.",br()," Puede ir a prepararse un café, esto tardarará unos minutos"))
    )
    req(carto_digital())
    remove_modal_spinner()
  })
  
  downfile(
    id = "down_carto",
    x = carto_digital()[-c(which(names(carto_digital()) %in% c("tabla_predios","tabla_areas")))],
    name_save = c(
      "Area",
      "Rodales",
      "Limite_Predial",
      "Suelos",
      "Rangos_pend",
      "Parcela",
      "Uso_actual",
      "Caminos",
      "Caminos_osm",
      "Hidrografia",
      "Hidrografia_osm",
      "Curvas_niv"
    ) %>%
      str_c(.,input$NOMPREDIO, sep = "_") %>%
      subset(c(rep(T, 5), input$add_parcelas, input$add_uso_actual, input$add_cam, input$add_cam_osm, input$add_hidro, input$add_hidro_osm, input$add_curv_niv))
  )
  
  # APÉNDICES 2 Y 3 ----
  shinyjs::disable("get_apendices_2y3_btn")
  observe({
    req(bd_parcelas(), rodales_def(), predios_def())
    shinyjs::enable("get_apendices_2y3_btn")
  })
  
  apendices_2y3 <- eventReactive(input$get_apendices_2y3_btn,{
    req(bd_parcelas(), rodales_def(), predios_def())
    apendice_2_3(
      bd_flora = parcelas_rodales(), 
      bd_pcob = bd_pcob(), 
      rodales = rodales_def(), 
      predios = predios_def(), 
      portada = input$portada, 
      provincia = input$provincia_carto, 
      huso = input$huso
    )
  })
  
  observeEvent(input$get_apendices_2y3_btn,{
    req(bd_parcelas(), rodales_def(), predios_def())
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(), p("Generando apéndices 2 y 3.", br(), " Por favor espere, esto puede tardar unos minutos"))
    )
    req(apendices_2y3())
    remove_modal_spinner()
  })
  
  downfile(
    id = "down_apendices_2",
    x = apendices_2y3()[[1]],
    name_save = c("APÉNDICE 2. Densiadad de especies")
  )
  downfile(
    id = "down_apendices_3",
    x = apendices_2y3()[[2]],
    name_save = c("APÉNDICE 3. Coordenadas ubicación de parcelas")
  )
  
  # ATRIBUTOS DE RODAL ----
  tabla_attr_rodal_0 <- reactive({
    req(c(parcelas_rodales(), rodales_def()))
    
    nha_parc <- parcelas_rodales() %>% 
      group_by(Nom_Predio, N_Rodal, Parcela, N_Parc, UTM_E, UTM_N, Tipo_veg) %>% 
      summarise(Nha = sum(Nha,na.rm = T), .groups = "drop") %>%
      rename(Coord_X = UTM_E, Coord_Y = UTM_N) %>% 
      st_as_sf(coords = c("Coord_X","Coord_Y"), crs = st_crs(rodales_def()), remove = F) %>% 
      arrange(N_Parc)
    
    if (any(nha_parc %>% st_distance(rodales) %>% apply(1, min) %>% .[] > 5)) {
      shinyalert(
        title = "OJO!", 
        text = tags$p(
          "Los siguientes puntos están a más de 5 metros de los rodales:", br(),
          nha_parc[nha_parc %>% st_distance(rodales) %>% apply(1, min) %>% .[] > 5, ]$Parcela %>% 
            {if(length(. > 10)) .[c(1:10)] else .} %>% 
            shQuote() %>% str_c(collapse = ", ") %>% 
            {if(length(. > 10)) str_c(., ", etc...") else .}, br(),
          "Por favor revisar"
        ),
        type = "warning",
        html = T,
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    }
    
    estimaciones_x_tipo <- parcelas_rodales() %>% 
      select(Tipo_veg, N_Parc, Especie, Nha) %>% 
      split(.$Tipo_veg) %>%
      map(~complete(.,Tipo_veg, N_Parc, Especie, fill = list(Nha = 0))) %>% 
      map(function(x) x %>% group_by(Tipo_veg, Especie) %>% summarise(Nha = mean(Nha, na.rm = T) %>% round()) %>% ungroup()) %>% 
      bind_rows() %>% 
      filter(Nha != 0) %>% 
      arrange(Tipo_veg, desc(Nha))
    
    df <- rodales_def() %>% 
      count(N_Rodal, Tipo_fores, Subtipo_fo, Tipo_veg) %>% select(-n) %>% 
      st_join(nha_parc %>% select(N_Parc, Nha))
    
    tabla_attr_rodal <- df %>% 
      bind_rows(
        nha_parc %>% 
          filter(!N_Parc %in% df$N_Parc) %>% 
          select(N_Parc, Nha) %>% 
          st_join(rodales_def() %>% select(N_Rodal, Tipo_fores, Subtipo_fo, Tipo_veg), join = st_nearest_feature) %>% 
          st_drop_geometry() %>% 
          left_join(rodales_def() %>% count(N_Rodal, Tipo_fores, Subtipo_fo, Tipo_veg) %>% select(-n)) %>% st_as_sf(crs = st_crs(rodales_def())) %>% 
          st_collection_extract("POLYGON")
      ) %>% 
      group_by(N_Rodal, Tipo_fores, Subtipo_fo, Tipo_veg, geometry) %>%
      summarise(
        Parcelas = str_c(N_Parc, collapse = '-'),
        NHA = mean(Nha, na.rm=T),
        .groups = "drop"
      ) %>% 
      st_drop_geometry() %>% 
      mutate(
        Tipo_attr = if_else(is.na(Parcelas), "Estimación", "Parcela directa"),
        Nom_attr = case_when(
          !is.na(Parcelas) ~ str_c("Parcela ", Parcelas),
          is.na(Parcelas) & !(Tipo_veg %in% unique(estimaciones_x_tipo$Tipo_veg)) ~ "Estimación por Tipo vegetacional similar",
          .default = "Estimación por Tipo vegetacional"
        )
      ) %>% 
      select(-c(Parcelas, NHA)) %>% 
      unnest(N_Rodal) %>% 
      arrange(N_Rodal)
    
    return(tabla_attr_rodal)
  })
  
  tabla_attr_rodal <- reactive({
    req(input$tabla_attr_rodal)
    read_xlsx(input$tabla_attr_rodal$datapath)
  })
  
  downfile(
    id = "tabla_attr_rodal_0",
    x = tabla_attr_rodal_0(),
    name_save = c("Tabla atributacion de rodales")
  )
  
  # APÉNDICE 5 ----
  shinyjs::disable("get_apendice_5_btn")
  observe({
    req(bd_parcelas(), rodales_def(), predios_def(), carto_digital(), areas_def())
    shinyjs::enable("get_apendice_5_btn")
  })
  
  obras_ap5 <- leer_sf(id = "obras_ap5", crs = crs_carto(), fx = function(x){
    x %>% 
      rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) 
  })
  observeEvent(obras_ap5(),{
    if(!all(c('Tipo', 'Obra') %in% names(obras_ap5()))){
      shinyalerta(names_act = names(obras_ap5()), names_req = c('Tipo', 'Obra'))
    }
  })
  
  bd_fauna <- reactive({
    req(input$bd_fauna$datapath)
    read_xlsx(input$bd_fauna$datapath) %>% 
      janitor::clean_names() %>% 
      rename_all(~ str_to_sentence(stri_trans_general(.,"Latin-ASCII")))
  })
  observeEvent(bd_fauna(),{
    if(!all(c('Tipo', 'Obra') %in% names(bd_fauna()))){
      shinyalerta(names_act = names(bd_fauna()), names_req = c('Nombre_cientifico', 'UTM_E', 'UTM_N', 'Categoria', 'Decreto'))
    }
  })
  
  apendice_5 <- eventReactive(input$get_apendice_5_btn,{
    req(bd_parcelas(), rodales_def(), predios_def(), carto_digital(), areas_def())
    apendice_5(
      bd_flora = parcelas_rodales(), 
      rodales = rodales_def(), 
      tabla_predios = carto_digital()$tabla_predios, 
      tabla_areas = carto_digital()$tabla_areas, 
      tabla_attr_rodal = tabla_attr_rodal(),
      portada = input$portada,
      carto_uso_actual = carto_digital()$carto_uso_actual, 
      obras = obras_ap5(), 
      bd_fauna = bd_fauna()
    )
  })
  
  observeEvent(input$get_apendice_5_btn,{
    req(bd_parcelas(), rodales_def(), predios_def(), carto_digital(), areas_def())
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(), p("Generando compilado de tablas.", br(), " Por favor espere, esto puede tardar unos minutos"))
    )
    req(apendice_5())
    remove_modal_spinner()
  })
  
  downfile(
    id = "down_apendice_5",
    x = apendice_5(),
    name_save = c("APÉNDICE 5. Tablas formulario CONAF")
  )
  
})




