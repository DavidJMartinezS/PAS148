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
                  ) %>%  bs_embed_tooltip(title = "Crea campo 'ID_ord' con el orden"),
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
              title = "Generar cartografía",
              solidHeader = T,
              status = "success",
              # HUSO 
              prettyRadioButtons(
                inputId = "huso",
                label = "Huso:", 
                choices = c("18S", "19S"),
                selected = "19S",
                inline = TRUE, 
                status = "success",
                fill = TRUE
              ),
              
              # AREAS DE CORTA
              leer_sfUI("cart_area", "Ingrese shapefile de áreas de corta") %>% 
                add_help_text(title = "Campos minimos requeridos:\n'Nom_Predio', 'N_Area'"),
              div(style = "margin-top: -10px"),
              RCA_UI("rca_areas"),
              div(style = "margin-top: -30px"),
              hr(),
              
              # RODALES
              leer_sfUI("cart_rodales", "Ingrese shapefile de rodales") %>%
                add_help_text(title = "Campos minimos requeridos:\n'N_Rodal', 'Tipo_For'"),
              div(style = "margin-top: -10px"),
              prettyToggle(
                inputId = "tipo_for",
                label_on = "Tipo_For (numerico): ej., 11",
                label_off = "Tipo_For (caracter): ej., 'Esclerófilo'"
              ),
              RCA_UI("rca_rodales"),
              div(style = "margin-top: -30px"),
              hr(),
              
              # PREDIOS
              leer_sfUI("cart_predios", "Ingrese shapefile de limites prediales") %>% 
                add_help_text(title = "Campos minimos requeridos:\n'N_Predio', 'Nom_Predio', 'Rol'"),
              div(style = "margin-top: -10px"),
              hr(),
              
              # CAMINOS 
              h5("Caminos", style = "font-weight: bold;"),
                p("Caminos serán creados a partir de la red vial del MOP actualizado al 07-02-2024 (descargar ",
                  a("aqui", .noWS = "outside", href = "https://mapas.mop.gov.cl/red-vial/Red_Vial_Chile.zip"),
                  ") ¿Desea crear otra capa de caminos a partir de información de Google?"),
              switchInput(
                inputId = "add_cam_osm",
                size = "mini",
                onLabel = "Si",
                offLabel = "No",
                onStatus = "success"
              ),
              uiOutput("add_cam_osm_ui"),
              div(style = "margin-top: -10px"),
              hr(),
              
              # HIDROGRAFÍA
              h5("Hidrografía", style = "font-weight: bold;"),
              p("Hidrografía será creada a partir de la hidrografía de IDE chile actualizada al 31-12-2022 (link ", 
                a("aquí", .noWS = "outside", href = "https://www.geoportal.cl/geoportal/catalog/36436/Hidrograf%C3%ADa%20de%20la%20regi%C3%B3n%20de%20Arica%20a%20la%20regi%C3%B3n%20de%20Los%20Lagos"),
                ") ¿Desea crear capa hidrografíca a partir de información de Google? (De lo contrario )"),
              switchInput(
                inputId = "add_hidro_osm",
                size = "mini",
                onLabel = "Si",
                offLabel = "No",
                onStatus = "success"
              ),
              uiOutput("add_hidro_osm_ui"),
              div(style = "margin-top: -10px"),
              hr(),
              
              # DEM
              fileInput(
                inputId = "dem",
                  label = "Ingresar DEM de Alos Palsar",
                multiple = F,
                accept = c(".tif",".jp2"),
                buttonLabel = "Seleccionar",
                placeholder = "Archivo no seleccionado"
              ) %>% 
                add_help_text("Por favor utilizar DEM acotado al área de estudio"),
              div(style = "margin-top: -10px"),
              hr(),
              
              # NOMBRE PREDIO
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
                style = "display: flex; align-items: center;"
              )
            ),
            box(
              width = 6,
              title = "Generar Anexos",
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