# Modulo lectura de sf

## UI
leer_sfUI <- function(id, ...){
  ns <- NS(id)
  
  fileInput(ns("sf_file"),..., accept=c('.shp','.dbf','.shx',".prj"), multiple=TRUE)
}

## Server
leer_sf <- function(input, output, session){
  return(
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
      map <- read_sf(paste(tempdirname, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/")) %>% 
        st_zm() %>% 
        mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
        drop_units()
      map
    })
  )
}

