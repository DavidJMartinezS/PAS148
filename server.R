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
  
  LB <- callModule(module = leer_sf, id = "linea_base", fx = function(x){
    x %>% rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII"))))
  })
  observeEvent(LB(),{
    if(!c('Tipo_for', 'Subtipo_fo', 'Regulacion') %in% names(LB())){
      shinyalert(
        title = "Listo!", 
        text = "Shapefile sin los campos requeridos",
        type = "error",
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    }
  })
  obras <- callModule(module = leer_sf, id = "obras")
  observeEvent(obras(),{
    if(!c('Tipo_for', 'Subtipo_fo', 'Regulacion') %in% names(LB())){
      shinyalert(
        title = "Listo!", 
        text = "Shapefile sin los campos requeridos",
        type = "error",
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    }
  })
  predios <- callModule(module = leer_sf, id = "predios")
  observeEvent(predios(),{
    if(!c('Obra','Tipo') %in% names(LB())){
      shinyalert(
        title = "Listo!", 
        text = "Shapefile sin los campos requeridos",
        type = "error",
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    }
  })
  hidro <- callModule(module = leer_sf, id = "hidro")
  observeEvent(hidro(),{
    if(!c('Nombre', 'Tipo', 'Permanencia') %in% names(LB())){
      shinyalert(
        title = "Listo!", 
        text = "Shapefile sin los campos requeridos",
        type = "error",
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    }
  })
  DEM <- reactive({
    req(input$dem)
    dem <- read_stars(input$dem) 
    names(dem) <- "elev"
    return(dem)
  })
  BN <- reactive({
    req(LB())
    LB() %>% 
      filter(regulacion = "Bosque nativo")
  })
  
  # Chequeo de cartografía ----
  shp_check <- callModule(module = leer_sf, id = "sf_check")
  observeEvent(input$check_carto,{
    req(shp_check(), input$select_sf_check)
    check_carto(x = shp_check(), id = input$select_sf_check)
  })
})








