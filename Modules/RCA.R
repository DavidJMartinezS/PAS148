RCA_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    tags$head(
      tags$style(
        type = "text/css", 
        "#qwe {display: flex; margin-left: 15px}
        #inline label{display: table-cell; text-align: center; vertical-align: middle;} 
        #inline .form-group{display: table-row;}"
      )
    ),
    span(
      id = "qwe",
      materialSwitch(
        inputId = ns("from_rca"),
        label = "¿misma capa de la RCA?",
        status = "success"
      ),
      div(id = "inline", uiOutput(ns("rca_ui")))
    )
  )
} 

RCA_SRV <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$from_rca,{
      output$rca_ui <- renderUI({
        if (input$from_rca) {
          div(
            textInput(inputId = ns("nrca"), label = "N°RCA", width = "50px"), 
            style = "margin-top: -8px; margin-left: 20px;"
          )
        }
      })
    })
    return(reactive({input$nrca}))
  })
}

# ui <- fluidPage(
#   fileInput("asd", label = "ingrese!"),
#   RCA_UI("rca_area"),
#   verbatimTextOutput("zxc")
# )
# server <- function(input, output){
#   n_rca <- RCA_SRV("rca_area")
#   output$zxc <- renderPrint({
#     paste0("RCA N°", n_rca() %>% str_extract("\\d+"))
#   })
# }
# 
# shinyApp(ui = ui, server = server)
