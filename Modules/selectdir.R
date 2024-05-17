# Module directory select

## UI
selectdirUI <- function(id, ...){
  ns <- NS(id)
  splitLayout(
    shinyDirButton(
      ns("directory"),
      label = NULL,
      title = "Seleccionar directorio",
      multiple = FALSE,
      icon = icon("folder"),
      viewtype = "detail",
      style = "padding: 7px 10px; background-color: #FFF066; border-radius: 10px;"
    ),
    downloadBttn(
      outputId = "down_bttn",
      label = NULL,
      style = "material-circle",
      size = "sm",
      color = "success"
    ),
    verbatimTextOutput("dir"), 
    cellWidths = c("10%","10%", "80%")
  )
}

# Server
selectdir <- function(id, name_zip, save_function){
  moduleServer(id, function(input, output, session){
    roots <- c(wd = path.expand("~"))
    shinyDirChoose(
      input,
      id = "directory",
      roots = roots,
      updateFreq = 0,
      session,
      defaultPath = "",
      defaultRoot = NULL,
      allowDirCreate = TRUE
    )
    
    directorio <- reactive({
      if(all(c("root", "path") %in% names(input$directory))){
        selected_path <- do.call(file.path, c(roots[input$directory$root], input$directory$path))
      } else {
        selected_path <- nullfile()
      }
    })
    
    output$dir <- renderPrint({
      if (dir.exists(directorio())) {
        directorio()
      } else {
        "Directorio no seleccionado"
      }
    })
    
    observeEvent(input$directory,{
      req(directorio())
      if (dir.exists(directorio())) {
        temp_dir <- tempdir()
        
        eval(save_function)
        
        zip_file <- file.path(temp_dir, paste0(name_zip, ".zip"))
        list_files <- list.files(
          temp_dir,
          ".dbf$|.prj$|.shp$|.shx$|.kml&|.xlsx$",
          full.names = TRUE
        )
        zip::zipr(zipfile = zip_file, files = Sys.glob(list_files))
        file.copy(zip_file, directorio(), overwrite = T)
        if (length(Sys.glob(list_files)) > 0) file.remove(Sys.glob(list_files))
        
        shinyalert(
          title = "Listo!", 
          text = paste0("Archivo descargado en:\n", directorio()),
          type = "success",
          timer = 2000, 
          closeOnEsc = T,  
          showConfirmButton = T,
          animation = TRUE
        )
      }
    })
  })
}
