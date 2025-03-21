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
    shinyjs::disable("downfile_bttn")
    observeEvent(x,{shinyjs::enable("downfile_bttn")})

    filetype <- reactive({
      x %>% map(~if_else(
        "wbWorkbook" %in% class(.),
        "wb",
        if_else(
          "sf" %in% class(.),
          "sf",
          if_else("data.frame" %in% class(.) & !"sf" %in% class(.), "xlsx", "")
        )
      ))
    })
    
    name_zip <- reactive({
      if(length(name_save) > 1) "Archivos_comprimidos" else as.character(name_save)
    })
    
    output$downfile <- downloadHandler(
      filename = function(){
        paste(name_zip(), "zip", sep = ".")
      },
      content = function(file){
        temp_dir <- tempdir()
        setwd(tempdir())
        pmap(
          list(x, filetype(), name_save),
          .f = function(x, y, z) {
            switch(
              y,
              sf = write_sf(x, file.path(temp_dir, paste0(file_path_sans_ext(z), ".shp"))),
              wb = wb_save(x, file.path(temp_dir, paste0(file_path_sans_ext(z), ".xlsx"))),
              xlsx = write_xlsx(x, file.path(temp_dir, paste0(file_path_sans_ext(z), ".xlsx")))
            )
          }
        ) 
        list_files <- map(name_save, function(x){list.files(temp_dir, pattern = paste0(x), full.names = T, recursive = T)})
        zip(zipfile = file, files = list_files)
        file.remove(unlist(list_files))
      }
    )
  })
}

