shinyUI(
  dashboardPage(
    skin = "green",
    dashboardHeader(
      title = tagList(
        span(class = "logo-lg", "PAS 148"),
        img(src = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABwAAAAcCAMAAABF0y+mAAAAYFBMVEXWx7jXyLnYybrOwLGyp5upnpTQwrPLva/BtKejmY5oY19TUU9NTEpsaGOck4k/Pz88PD04OTo0NTdiXlssLzK+sKR0bmkxMzWPh39GRkV8dm+GfneMg3xZVlS4rKDfz78rckUCAAAA1UlEQVR4AbTSVQKEIBAAUGbEoWs2bL3/LRc/rc81qEeD+P8DiAg1gjtqZCsbJKWvRsY676yRwWk8mYgpF+biX/79OaMsOQVjvimn7oRA35J6gQhxuGIzZLsPBWrMF1RcJoKawniDXZlrvGO6YimjggekJfsVAeCK9YklO0mkbsYUoELOxbmwXbGq/g6Fu3vETZl+6tsd9RnbKEmQQDEXVnCa7ZiWdUPcPktZCM4bz3mJn09ccmeux9mnzF3HJc0EV431sOtpR3q6JkY2CI8X7IF+oyoAAGbZDRcCw+B1AAAAAElFTkSuQmCC")
      ),
      tags$li(class = "dropdown",
              id = "logo",
              tags$a(tags$img(height = "40px",
                              src="https://geobiota.com/img/logo-header.svg")
              ),
              tags$style(
                HTML(
                "/* move logo to center */
                  #logo {
                      position: absolute;
                      left: 50%;
                      top: 50%;
                      transform: translate(-50%, -50%);
                  }
                /* remove hover effect */
                 #logo > a:hover {
                      background-color: transparent !important;
                      color: transparent !important;
                  }"
                )
              )
      ),
      userOutput("user")
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Importante", tabName = "importante", icon = icon("circle-info")),
        menuItem("Ayuda cartográfica", tabName = "ayuda", icon = icon("circle-check")),
        menuItem("Insumos Cartográficos", id = "info", icon = icon("layer-group"),
          menuSubItem("Cartografía base", tabName = "carto", icon = icon("water")),
          menuSubItem("Accesos al Predio", tabName = "access", icon = icon("route"))
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
            column(
              width = 5,
              box(
                width = 12,
                title = "Inputs",
                solidHeader = T,
                status = "success",
                prettyRadioButtons(
                  inputId = "huso",
                  label = "Huso:", 
                  choices = c("18S", "19S"),
                  selected = "18S",
                  inline = TRUE, 
                  status = "success",
                  fill = TRUE
                ),
                leer_sfUI("linea_base", "Ingrese cartografía de linea base") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'Tipo_for', 'Subtipo_fo', 'Regulacion'"),
                leer_sfUI("obras", "Ingrese shp de obras") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'Obra','Tipo'"),
                leer_sfUI("predios", "Ingrese shp de predios") %>%
                  add_help_text(title = "Campos minimos requeridos:\n'Nom_Predio', 'Rol', 'Propietario'"),
                leer_sfUI("suelos", "Ingrese shp de suelos") %>%
                  add_help_text(title = "Campos minimos requeridos:\n'Textcaus'")
              )     
            ),
            column(
              width = 7,
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
                  choices = c(NULL), 
                  options = list(title = "Selecciona una o más opciones")
                ),
                div(
                  actionBttn(
                    inputId = "apply_order",
                    label = "Ordenar capa",
                    style = "unite",
                    size = "sm",
                    color = "success"
                  ),
                  downUI("down_sf_ordered"),
                  style = "display: flex; align-items: center;"
                ),
                hr(style="height:2px;border-width:0;color:gray;background-color:gray"), 
                h3("Generar rodales y áreas de corta"),
                pickerInput(
                  inputId = "grpup_area",
                  label = "Agrupar por (Opcional):",
                  choices = c(NULL),
                  multiple = T,
                  options = list(title = "Selecciona una o más opciones")
                ),
                materialSwitch(
                  inputId = "group_by_dist",
                  label = "¿Agrupar por distancia?",
                  status = "success"
                ),
                materialSwitch(
                  inputId = "sep_by_CUS",
                  label = "¿Separar geometrías por CUS?",
                  value = T,
                  status = "success"
                ),
                uiOutput("distanceUI"),
                div(
                  actionBttn(
                    inputId = "get_area",
                    label = "Ordenar capa",
                    style = "unite",
                    size = "sm",
                    color = "success"
                  ),
                  downUI("down_areas"),
                  style = "display: flex; align-items: center;"
                ),
                hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
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
                  size = "sm",
                  color = "success"
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "access",
          box(
            title = "Crear Puntos de referencia",
            status = "warning",
            width = 12,
            fluidRow(
              column(
                width = 4,
                boxPad(
                  pickerInput(
                    inputId = "select_nom_pto",
                    label = "Seleccione nombre del punto de referencia",
                    choices = c("Acceso al predio","Portón de entrada al predio","Otro"),
                    selected = "Acceso al predio",
                    options = list(title = "Selecciona una opción")
                  ),
                  renderUI("otro_nom_pto"),
                  # sliderInput(
                  #   "n_predio", 
                  #   "Numero del predio:",
                  #   min = 1, max = nrow(), value = 1
                  # ),
                  checkboxGroupInput(
                    "variable", 
                    "Variables to show:",
                    c("Cylinders" = "cyl",
                      "Transmission" = "am",
                      "Gears" = "gear")
                  ),
                  actionBttn(
                    inputId = "iniciar",
                    label = "Iniciar", 
                    style = "material-flat",
                    size = "sm",
                    color = "success"
                  ),
                  actionBttn(
                    inputId = "crear",
                    label = "Crear", 
                    style = "material-flat",
                    size = "sm",
                    color = "success"
                  ),
                  actionBttn(
                    inputId = "finalizar",
                    label = "Finalizar", 
                    style = "material-flat",
                    size = "sm",
                    color = "success"
                  )
                )
              ),
              column(
                width = 8,
                editModUI("leaf_pto_ref")
              )
            )
          )
        ),
        tabItem(
          tabName = "carto",
          fluidRow(
            box(
              width = 6,
              title = "Inputs finales",
              solidHeader = T,
              status = "success",
              leer_sfUI("cart_area", "Ingrese shapefile de areas de corta") %>% 
                add_help_text(title = "Campos minimos requeridos:\n'Nom_Predio', 'N_a', 'Sup_ha'"),
              leer_sfUI("cart_rodales", "Ingrese shapefile de rodales") %>%
                add_help_text(title = "Campos minimos requeridos:\n'N_Rodal', 'Tipo_For', 'Sup_ha'"),
              leer_sfUI("cart_predios", "Ingrese shapefile de limites prediales") %>% 
                add_help_text(title = "Campos minimos requeridos:\n'N_Predio', 'Nom_Predio', 'Rol'"),
              leer_sfUI("hidro", "Ingrese shp de hidrografía (Opcional)") %>% 
                add_help_text(title = "Campos minimos requeridos:\n'Nombre', 'Tipo', 'Permanencia'"),
              fileInput(
                inputId = "dem",
                label = "DEM",
                multiple = F,
                accept = c(".tif",".png"),
                buttonLabel = "Seleccionar",
                placeholder = "Archivo no seleccionado"
              ) %>% 
                add_help_text("Peso del archivo debe ser menor a XX Mb"),
              h5("Caminos", style = "font-weight: bold;"),
              p("Caminos serán creados a partir de la red vial del MOP actualizado al 18-07-2023 ¿Desea agregar caminos que se puedan obtener desde google?"),
              switchInput(
                inputId = "add_osm",
                onLabel = "SI",
                offLabel = "NO",
                onStatus = "success"
              ),
              uiOutput("osmUI"),
              textInput("NOMPREDIO", "Ingrese un sufijo para el nombre de los archivos"),
              actionBttn(
                inputId = "get_carto_btn",
                label = "Obtener cartografía", 
                style = "unite",
                size = "sm",
                color = "success"
              )
            ),
            box(
              width = 6,
              title = "Outputs",
              solidHeader = T,
              status = "success"
            )
          )
        )
      )# fin tabitems
    ) # fin dashboardbody
  )# fin dashboardpage
)