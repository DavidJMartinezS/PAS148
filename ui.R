shinyUI(
  dashboardPage(
    skin = "green",
    dashboardHeader(
      title = tagList(
        span(class = "logo-lg", "PAS 148"),
        img(src = "./logo.png")
      ),
      userOutput("user")
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Importante", tabName = "importante", icon = icon("circle-info")),
        menuItem("Ayuda cartográfica", tabName = "ayuda", icon = icon("circle-check")),
        # menuItem("Inputs y outputs", tabName = "inputs_outputs", icon = icon("file-import")),
        menuItem("Insumos Cartográficos", id = "info", icon = icon("layer-group"),
          menuSubItem("Accesos al Predio", tabName = "access", icon = icon("route")),
          menuSubItem("Cartografía base", tabName = "carto", icon = icon("water"))
        )
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      use_bs_popover(),
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
          tabName = "ayuda",
          fluidRow(
            column(width = 5,
              box(
                width = 12,
                solidHeader = T,
                status = "success",
                title = "Ayudas cartográficas",
                h3("Ordenar shapefile N->S & O->E"),
                leer_sfUI("sf_order","Ingrese Shapefile que desea ordenar"),
                pickerInput(
                  inputId = "select_field_order",
                  label = "Agrupar por (Opcional):",
                  multiple = T,
                  choices = c(NULL), # Agregar en updatePickerInput() en el server como un reactivo al shp para ordenar
                  options = list(title = "Selecciona una opción")
                ),
                actionBttn(
                  inputId = "apply_order",
                  label = "Ordenar capa",
                  style = "unite",
                  size = "sm",
                  color = "success"
                ),
                actionBttn(
                  inputId = "down_shp_order",
                  label = NULL,
                  icon = icon("download"),
                  style = "material-circle",
                  size = "sm",
                  color = "warning"
                ),
                hr(), hr(),
                h3("Generar rodales y áreas de corta"),
                checkboxGroupButtons(
                  inputId = "grpup_area",
                  label = "Divdir areas por:",
                  choices = c("Tipo forestal", "Subtipo forestal", "CUS"),
                  selected = c("Tipo forestal", "Subtipo forestal", "CUS"),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: green"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: green"))
                ),
                actionBttn(
                  inputId = "get_area",
                  label = "Generar areas de corta", 
                  style = "unite",
                  size = "sm",
                  color = "success"
                ),
                actionBttn(
                  inputId = "down_shp_areas",
                  label = NULL,
                  icon = icon("download"),
                  style = "material-circle",
                  size = "sm",
                  color = "warning"
                ),
                hr(), hr(),
                h3("Chequeo de cartografía"),
                leer_sfUI("sf_check","Ingrese Shapefile"),
                pickerInput(
                  inputId = "select_sf_check",
                  label = "Seleccione la capa a la que corresponde el shapefile",
                  choices = c("Área","Caminos","Curvas de nivel","Hidrografía","Límite predial","Parcelas","Puntos de referencia","Rangos de pendiente","Rodales","Suelos"),
                  options = list(title = "Selecciona una opción")
                ),
                actionBttn(
                  inputId = "check_carto",
                  label = "Chequear", 
                  style = "unite",
                  color = "success"
                )
              )
            ),
            column(
              width = 7,
              box(
                width = 12,
                title = "Inputs",
                solidHeader = T,
                status = "success",
                leer_sfUI("linea_base", "Ingrese cartografía de linea base") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'Tipo_for', 'Subtipo_fo', 'Regulacion'"),
                leer_sfUI("obras", "Ingrese shp de obras") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'Obra','Tipo'"),
                leer_sfUI("predios", "Ingrese shp de predios") %>%
                  add_help_text(title = "Campos minimos requeridos:\n'Nom_Predio', 'Rol', 'Propietario'"),
                leer_sfUI("hidro", "Ingrese shp de hidrografía") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'Nombre', 'Tipo', 'Permanencia'"),
                fileInput(
                  inputId = "dem",
                  label = "DEM",
                  multiple = F,
                  accept = c(".tif",".png"),
                  buttonLabel = "Seleccionar",
                  placeholder = "Archivo no seleccionado"
                ) %>% 
                  add_help_text("Peso del archivo debe ser menor a XX Mb")
              )
            )
          )
        ),
        tabItem(
          tabName = "access",
          fluidRow(
            box(
              width = 6,
              title = "Inputs finales",
              solidHeader = T,
              status = "success",
              leer_sfUI("cart_area", "Ingrese shapefile de areas de corta"),
              leer_sfUI("cart_rodales", "Ingrese shapefile de rodales"),
              leer_sfUI("cart_predios", "Ingrese shapefile de limites prediales"),
              textInput("NOMPREDIO", "Ingrese nombre sufijo para el archivo")
            ),
            box(
              width = 6,
              title = "Outputs",
              solidHeader = T,
              status = "success",
              h3("Cartografía digital"),
              bs_button("obtener cartografía", button_type = "success")
            )
          )
        )
      )# fin tabitems
    ) # fin dashboardbody
  )# fin dashboardpage
)