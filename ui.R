shinyUI(
  dashboardPage(
    skin = "green",
    dashboardHeader(
      title = "PAS 148",
      userOutput("user")
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Importante", tabName = "importante", icon = icon("circle-info")),
        # menuItem("Chequeo de cartografía", tabName = "check", icon = icon("circle-check")),
        menuItem("Inputs", tabName = "inputs_check", icon = icon("file-import")),
        menuItem("Insumos Cartográficos", id = "info", icon = icon("layer-group"),
          menuSubItem("Accesos al Predio", tabName = "uso", icon = icon("route")),
          menuSubItem("Cartografía base", tabName = "uso", icon = icon("water"))
        )
      )
    ),
    dashboardBody(
      shinyEffects::setShadow(class = "dropdown-menu"),
      shinyEffects::setShadow(class = "box"),
      tags$head(tags$style(HTML(".small-box {height: 120px;}"))),
      tags$head(tags$style(
        ".progress-bar{background-color:#3c763d;}"
      )),
      tabItems(
        tabItem(
          tabName = "importante",
          fluidRow(
            box(
              title = "Uso adecuado del Dashboard",
              width = 12,
              info_dashboard()
            )
          )
        ),
        tabItem(
          tabName = "inputs_check",
          fluidRow(
            column(width = 5,
              box(
                width = 12,
                title = "Input PAS 148",
                solidHeader = T,
                status = "success",
                leer_sfUI("linea_base", "Ingrese cartografía de linea base"),
                leer_sfUI("obras", "Ingrese shp de obras"),
                leer_sfUI("predios", "Ingrese shp de predios")
              ),
              box(
                width = 12,
                solidHeader = T,
                status = "success",
                title = "Chequeo de cartografía",
                leer_sfUI("sf_check","Ingrese Shapefile"),
                pickerInput(
                  inputId = "select_sf_check",
                  label = "Seleccione la capa a la que corresponde el shapefile",
                  choices = c("Área","Caminos","Curvas de nivel","Hidrografía","Límite predial","Parcelas","Puntos de referencia","Rangos de pendiente","Rodales","Suelos"),
                  options = list(title = "Selecciona una opción")
                )
              ),
              box(
                width = 12,
                solidHeader = T,
                status = "success",
                title = "Ayudas cartográficas",
                h3("Ordenar shapefile N->S & O->E"),
                leer_sfUI("sf_order","Ingrese Shapefile que desea ordenar"),
                pickerInput(
                  inputId = "select_field_order",
                  label = "Agregue el o los campos a incluir en el orden (Opcional)",
                  choices = c(NA), #Agregar updatePickerInput() en el server como un reactivo al shp para ordenar
                  options = list(title = "Selecciona una opción")
                ),
                hr(),br(),
                h3("Generar áreas de corta"),
                switchInput(
                  value = TRUE,
                  label = "Incluir CUS",
                  labelWidth = "80px",
                  inputId = "Id013",
                  onLabel = "Si",
                  offLabel = "No",
                  onStatus = "success", 
                  offStatus = "danger"
                )
              )
            ),
            column(
              width = 7,
              box(
                width = 12,
                solidHeader = T,
                status = "success",
                title = "Mapa de áreas"
              )
            )
          )
        )
      )
    )
  )
)