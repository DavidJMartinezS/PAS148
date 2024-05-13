# Modulo lectura de sf

## UI
leer_sfUI <- function(id, ...){
  ns <- NS(id)
  
  fileInput(
    ns("sf_file"),
    ..., 
    accept = c('.shp','.dbf','.shx',".prj"), 
    multiple = TRUE,
    buttonLabel = "Seleccionar",
    placeholder = "Archivo no seleccionado"
  )
}

# Server
leer_sf <- function(id, crs = NULL, fx = NULL){
  moduleServer(id, function(input, output, session){
    reactive({
      req(input$sf_file)
      shpdf <- input$sf_file
      tempdirname <- dirname(shpdf$datapath[1])
      for (i in 1:nrow(shpdf)) {
        file.rename(
          shpdf$datapath[i],
          paste0(tempdirname, "/", shpdf$name[i])
        )
      }
      shp <- read_sf(paste(tempdirname, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/")) %>%
        st_zm() %>%
        st_make_valid()
      if (!is.null(fx)){
        shp <- shp %>% fx()
      } 
      if (!is.null(crs)){
        shp <- shp %>% st_transform(crs)
      }
      shp
    })
  })
}
