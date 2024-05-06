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
  LB <- leer_sf(id = "linea_base",fx = function(x){
    x %>% rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII"))))
  })
  observeEvent(LB(),{
    if(!all(c('Tipo_for', 'Subtipo_fo', 'Regulacion') %in% names(LB()))){
      shinyalerta()
    }
  })
  obras <- leer_sf(id = "obras", fx = function(x){
    x %>% rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII"))))
  })
  observeEvent(obras(),{
    if(!all(c('Obra', 'Tipo') %in% names(obras()))){
      shinyalerta()
    }
  })
  predios <- leer_sf(id = "predios")
  observeEvent(predios(),{
    if(!all(c('Nom_Predio','Rol','Propietario') %in% names(predios()))){
      shinyalerta()
    }
  })
  hidro <- leer_sf(id = "hidro", fx = function(x){
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
  
  BN <- reactive({
    req(LB())
    LB() %>% 
      filter(regulacion = "Bosque nativo")
  })
  
  
  # Ordenar shapefile ----
  shp_to_order <- leer_sf(id = "sf_order")
  
  observeEvent(shp_to_order(),{
    updatePickerInput(
      session = session,
      inputId = "select_field_order",
      choices = names(shp_to_order())
    )
  })
  
  shp_ordered <- eventReactive(input$apply_order,{
    req(shp_to_order())
    shp_to_order() %>% 
      mutate(ID_ord = st_order(geometry)) 
  })
  
  observeEvent(input$apply_order,{
    req(shp_ordered())
    shinyalert(
      title = "Listo", 
      text = "Su shapefile ha sido ordenado",
      type = "success",
      timer = 1000,
      animation = TRUE
    )
  })
  
  output$downloadData <- downloadHandler( ## Solucionar problema para descargar
    filename = function() {
      "shp_ord.zip"
    },
    content = function(file) {
      temp_dir <- tempdir()
      write_sf(shp_to_order(), file.path(temp_dir,"asd.shp"))
      list_files <- list.files(temp_dir,
                               ".dbf$|.prj$|.shp$|.shx$",
                               full.names = TRUE)
      zip::zipr(zipfile = file, files = Sys.glob(list_files))
    }
  )
  
  # Chequeo de cartografía ----
  shp_check <- leer_sf(id = "sf_check")
  observeEvent(input$check_carto,{
    req(shp_check(), input$select_sf_check)
    check_carto(x = shp_check(), id = input$select_sf_check)
  })
})




