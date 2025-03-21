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
              width = 6,
              box(
                width = 12,
                title = "Generar rodales y áreas de corta",
                solidHeader = T,
                status = "success",
                leer_sfUI("linea_base", "Ingrese cartografía de linea base") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'Tipo_for', 'Subtipo_fo', 'Regulacion'"),
                leer_sfUI("obras", "Ingrese shp de obras") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'Obra','Tipo'"),
                leer_sfUI("predios", "Ingrese shp de predios") %>%
                  add_help_text(title = "Campos minimos requeridos:\n'N_Predio','Nom_Predio', 'Rol', 'Prop'"),
                leer_sfUI("suelos", "Ingrese shp de suelos") %>%
                  add_help_text(title = "Campos minimos requeridos:\n'Textcaus'"),
                pickerInput(
                  inputId = "group_by_LB",
                  label = "Agrupar por (Opcional):",
                  choices = c(NULL),
                  multiple = T,
                  options = list(title = "Selecciona una o más opciones")
                ),
                materialSwitch(
                  inputId = "sep_by_CUS",
                  label = "¿Separar geometrías por CUS?",
                  value = T,
                  status = "success"
                ),
                materialSwitch(
                  inputId = "group_by_dist",
                  label = "¿Agrupar áreas por distancia?",
                  status = "success"
                ),
                uiOutput("distanceUI"),
                materialSwitch(
                  inputId = "cut_by_prov",
                  label = "¿Cortar áreas por una provincia?",
                  status = "success"
                ),
                uiOutput("select_prov_UI"),
                div(
                  materialSwitch(
                    inputId = "ord_rodales",
                    label = "¿Ordenar rodales espacialmente?",
                    status = "success"
                  ),
                  uiOutput("ord_rodales_UI")
                ),
                div(
                  actionBttn(
                    inputId = "get_area",
                    label = "Generar capas",
                    style = "unite",
                    size = "sm",
                    color = "success"
                  ),
                  downfile_ui("down_areas"),
                  style = "display: flex; align-items: center;"
                )
              )     
            ),
            column(
              width = 6,
              box(
                width = 12,
                solidHeader = T,
                status = "success",
                title = "Ayudas cartográficas",
                h3("Ordenar shapefile"),
                leer_sfUI("sf_order","Ingrese Shapefile que desea ordenar"),
                pickerInput(
                  inputId = "orden",
                  label = "Ordenar de:",
                  selected = "NS-OE",
                  choices = c("NS-OE","NS-EO","SN-EO","SN-OE","EO-NS","EO-SN","OE-NS","OE-SN")
                ),
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
                  downfile_ui("down_sf_ordered"),
                  style = "display: flex; align-items: center;"
                ),
                hr(style="height:2px;border-width:0;color:gray;background-color:gray"), 
                h3("Chequeo de cartografía"),
                leer_sfUI("sf_check","Ingrese Shapefile"),
                pickerInput(
                  inputId = "select_sf_check",
                  label = "Seleccione la capa a la que corresponde el shapefile",
                  choices = c(
                    "Área",
                    "Caminos",
                    "Curvas de nivel",
                    "Hidrografía",
                    "Límite predial",
                    "Parcelas",
                    "Puntos de referencia",
                    "Rangos de pendiente",
                    "Rodales",
                    "Suelos"
                  ),
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
          tabName = "carto",
          fluidRow(
            box(
              width = 6,
              title = "Inputs finales",
              solidHeader = T,
              status = "success",
              leer_sfUI("cart_area", "Ingrese shapefile de areas de corta") %>% 
                add_help_text(title = "Campos minimos requeridos:\n'Nom_Predio', 'N_a'"),
              leer_sfUI("cart_rodales", "Ingrese shapefile de rodales") %>%
                add_help_text(title = "Campos minimos requeridos:\n'N_Rodal', 'Tipo_For'"),
              prettyToggle(
                inputId = "tipo_for",
                label_on = "Tipo_For (numerico): ej., 11", 
                label_off = "Tipo_For (caracter): ej., 'Esclerófilo'"
              ),
              br(),
              leer_sfUI("cart_predios", "Ingrese shapefile de limites prediales") %>% 
                add_help_text(title = "Campos minimos requeridos:\n'N_Predio', 'Nom_Predio', 'Rol'"),
              # leer_sfUI("hidro", "Ingrese shp de hidrografía (Opcional)") %>% 
              #   add_help_text(title = "Campos minimos requeridos:\n'Nombre', 'Tipo', 'Perma'"),
              fileInput(
                inputId = "dem",
                label = "DEM",
                multiple = F,
                accept = c(".tif",".jp2"),
                buttonLabel = "Seleccionar",
                placeholder = "Archivo no seleccionado"
              ) %>% 
                add_help_text("Por favor utilizar DEM acotado al área de estudio"),
              h5("Caminos", style = "font-weight: bold;"),
              p("Caminos serán creados a partir de la red vial del MOP actualizado al 18-07-2023 ¿Desea agregar caminos que se puedan obtener desde google?"),
              switchInput(
                inputId = "add_osm",
                onLabel = "SI",
                offLabel = "NO",
                onStatus = "success"
              ),
              uiOutput("osmUI"),
              textInput("NOMPREDIO", "Ingrese un sufijo para el nombre de los archivos", placeholder = "Ej: CHILICAUQUENALTO, KIMAL, etc"),
              div(
                actionBttn(
                  inputId = "get_carto_btn",
                  label = "Obtener cartografía", 
                  style = "unite",
                  size = "sm",
                  color = "success"
                ),
                renderUI("down_carto_ui"),
                # downfile_ui("down_carto"),
                style = "display: flex; align-items: center;"
              )
            ),
            box(
              width = 6,
              title = "Outputs",
              solidHeader = T,
              status = "success"
            )
          )
        ),
        tabItem(
          tabName = "access"
          
        )
      )# fin tabitems
    ) # fin dashboardbody
  )# fin dashboardpage
)