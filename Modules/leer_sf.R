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
leer_sf <- function(id, crs = NULL, fx = NULL, path = F, ...){
  moduleServer(id, function(input, output, session){
    reactive({
      req(input$sf_file)
      if(!(input$sf_file$datapath %>% tools::file_ext() %in% c("dbf", "prj", "shp", "shx") %>% all() &
         input$sf_file$name %>% tools::file_path_sans_ext() %>% unique() %>% length() == 1 &
         length(input$sf_file$datapath) >= 4)) {
        unlink(input$sf_file$datapath)
        reset(id = "sf_file")
        return(NULL)
      } else {
        if (path) {
          return(input$sf_file$name[1] %>% tools::file_path_sans_ext())
        } else {
          shpdf <- input$sf_file
          tempdirname <- dirname(shpdf$datapath[1])
          for (i in 1:nrow(shpdf)) {
            file.rename(
              shpdf$datapath[i],
              paste0(tempdirname, "/", shpdf$name[i])
            )
          }
          shp <- read_sf(paste(tempdirname, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"), ...) %>%
            st_zm() %>%
            st_make_valid() %>% 
            {if (!is.null(fx)) .[] %>% fx() else .} %>% 
            {if (!is.null(crs)) .[] %>% st_transform(crs) else .}
          return(shp)
        }
      }
    })
  })
}
