downfile_ui <- function(id,...){
  ns <- NS(id)
  div(
    downloadBttn(
      outputId = ns("downfile"), 
      style = "material-circle", 
      size = "sm",
      color = "success",
      icon = icon("download"),
      ...
    ),
    style = "margin-left: 15px;"
  )
}

downfile <- function(id, x, name_save){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shinyjs::disable("downfile_bttn")
    observeEvent(x,{shinyjs::enable("downfile_bttn")})

    filetype <- reactive({
      x %>% 
        {if(any(class(.) == "list")) . else list(.)} %>% 
        map(~if_else(
          "wbWorkbook" %in% class(.),
          "wb",
          if_else(
            "sf" %in% class(.),
            "sf",
            if_else("data.frame" %in% class(.) & !"sf" %in% class(.), "xlsx", "")
          )
        )) %>% 
        {if(length(.) == 1) unlist(.) else .} 
    })
    
    output$downfile <- downloadHandler(
      filename = function(){
        if(length(filetype()) > 1){
          "Archivos_comprimidos.zip"
        } else {
          paste0(as.character(name_save), if_else(filetype() == "sf", ".shp", ".xlsx"))
        }
      },
      content = function(file){
        temp_dir <- tempdir()
        setwd(tempdir())
        pwalk(
          if(length(filetype()) == 1) list(list(x), list(filetype()), list(name_save)) else list(x, filetype, name_save),
          .f = function(x, y, z) {
            switch(
              y,
              sf = sf::write_sf(x, paste0(file_path_sans_ext(z), ".shp")),
              wb = openxlsx2::wb_save(x, paste0(file_path_sans_ext(z), ".xlsx"), overwrite = T),
              xlsx = writexl::write_xlsx(x, paste0(file_path_sans_ext(z), ".xlsx"))
            )
          }
        ) 
        list_files <- unlist(map(name_save, function(x){list.files(temp_dir, pattern = x)}))
        zip(zipfile = file, files = list_files)
        file.remove(list_files)
      }
    )
  })
}

