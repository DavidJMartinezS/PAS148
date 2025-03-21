# Module directory select
css_downui <- HTML("
  .downui {
    display: flex;
    margin-left: 15px;
    align-items: center;
  }
  .downui button {
    margin-right: 15px;
  }
  .downui pre {
    margin-top: 10px;
  }
")

## UI
downUI <- function(id, ...){
  ns <- NS(id)
  tags$span(
    tags$div(
      class = "downui",
      shinyDirButton(
        ns("directory"),
        label = NULL,
        title = "Seleccionar directorio",
        multiple = FALSE,
        icon = icon("folder"),
        viewtype = "detail",
        style = "padding: 7px 10px; background-color: #FFF066; border-radius: 10px;"
      ),
      actionBttn(
        ns("downloadData"),
        label = NULL,
        style = "material-circle", 
        size = "sm",
        color = "success",
        icon = icon("download")
      ),
      verbatimTextOutput(ns("dir"),placeholder = T)
    ),
    tags$style(css_downui)
  )
}

# Server
down <- function(id, x, name_save, filetype = c("sf","xlsx_sheet","sf_wb")){
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
      return(selected_path)
    })
    
    observe({
      if (dir.exists(directorio())) {
        enable(id = "downloadData")
      } else {
        disable(id = "downloadData")
      }
    })
    
    output$dir <- renderPrint({
      if (dir.exists(directorio())) {
        directorio()
      } else {
        "Directorio no seleccionado"
      }
    })
    
    observeEvent(input$downloadData,{
      
      req(directorio())
      
      if (dir.exists(directorio())) {
        
        temp_dir <- tempdir()
        zip_file <- file.path(temp_dir, paste0(file_path_sans_ext(name_save), ".zip"))
        
        show_modal_spinner(
          spin = "flower",
          color = "#35978F",
          text = div(br(),p("Descargando en la ruta...",br()," Por favor espere, esto puede tardar unos segundos"))
        )
        
        if (filetype == "sf") {
          
          map2(x, file.path(temp_dir, paste0(file_path_sans_ext(name_save), ".shp")), .f = ~write_sf(.x, .y))
          list_files <- map(name_save, function(x){
            list.files(temp_dir, pattern = paste0(x,"(.dbf$|.prj$|.shp$|.shx$)"),full.names = T)
          })
          
        } else {
          
          if (filetype == "xlsx_sheet") {
            
            map2(x, file.path(temp_dir, paste0(file_path_sans_ext(name_save), ".shp")), .f = ~write.xlsx(.x, .y))
            
          } else if (filetype == "xlsx_wb") {
            
            map2(x, file.path(temp_dir, paste0(file_path_sans_ext(name_save), ".shp")), .f = ~saveWorkbook(.x, .y,overwrite = TRUE))
            
          }
          
          list_files <- map(name_save, function(x){
            
            list.files(temp_dir, pattern = paste0(x,"(.xlsx$)"),full.names = T)
            
          })
        }
        
        map2(zip_file, lapply(list_files,Sys.glob), .f = ~zip::zipr(.x,.y))
        file.copy(zip_file, directorio(), overwrite = T)
        list_files_rm <- map(name_save, function(x){
          list.files(temp_dir, pattern = paste0(x),full.names = T,recursive = T)
        })
        file.remove(unlist(list_files_rm))
        
        remove_modal_spinner()
        
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

# EXAMPLE #
# ui <- shinyUI(
#   dashboardPage(
#     dashboardHeader(),
#     dashboardSidebar(),
#     dashboardBody(
#       box(
#         width = 12,
#         title = "ads",
#         solidHeader = T,
#         status = "success",
#         div(
#           actionBttn("asd",label = "button"),
#           downUI("id"),
#           style = "display: flex; align-items: center;"
#         )
#       )
#     )
#   )
# )
# server <- shinyServer(function(input,output,session){
#   down("id",x = caminos[1:5],name_save = "caminos",filetype = "sf")
# })
# shinyApp(ui,server)


