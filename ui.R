shinyUI(
  dashboardPage(
    skin = "green",
    # HEADER ----
    dashboardHeader(
      leftUi = tagList(
        div(
          radioGroupButtons(
            inputId = "PAS",
            choiceNames = c("PAS 148", "PAS 151"),
            choiceValues = c(148, 151),
            selected = 148,
            status = "warning"
          ),
          style = "margin-left: 10px;"
        ),
        # verbatimTextOutput("out"),
        dropdownButton(
          label = "Ajustes bases",
          icon = icon("gear"),
          status = "warning",
          circle = F,
          span(
            id = "flex",
            prettyRadioButtons(
              inputId = "huso",
              label = "Huso:  ", 
              choices = c("18S", "19S"),
              selected = "19S",
              inline = TRUE, 
              status = "success",
              fill = TRUE, 
              animation = "smooth"
            ),
            div(
              numericInput(
                inputId = "n_dec",
                label = "N° decimales superficie:", 
                value = 2, 
                min = 0, 
                max = 4
              ),
            style = "margin-left: 20px;"
            )
          )
        ),
        div(
          id = "inline",
          pickerInput(
            inputId = "provincia",
            label = "Provincia:", 
            choices = provincias_list,
            selected = NULL,
            options = pickerOptions(container = "body", style = "btn-warning"),
          ),
          style = "color: white; margin-left: 10px;"
        )
      ),
      title = tagList(
        span(class = "logo-lg", "PAS 148"),
        img(src = "https://github.com/DavidJMartinezS/PAS148/blob/main/www/logo_geobiota.png?raw=true")
      ),
      tags$li(
        class = "dropdown",
        id = "logo",
        tags$a(tags$img(height = "40px", src="https://raw.githubusercontent.com/DavidJMartinezS/PAS148/2285bc0c6ad63ade23e8c97d264844972905e38e/www/logo-header.svg")),
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
    # SIDEBAR ----
    dashboardSidebar(
      sidebarMenu(
        menuItem("Importante", tabName = "importante", icon = icon("circle-info")),
        menuItem("Ayuda cartográfica", tabName = "ayuda", icon = icon("circle-check")),
        menuItem("Cartografía y Anexos", tabName = "carto", icon = icon("layer-group"))
      )
    ),
    # BODY ----
    dashboardBody(
      shinyjs::useShinyjs(),
      use_bs_popover(),
      shinyEffects::setShadow(class = "dropdown-menu"),
      shinyEffects::setShadow(class = "box"),
      tags$head(tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=News+Cycle');


                        h1 {
                            font-family:'News Cycle',sans-serif;
                            font-size: 48px;
                            font-weight: 1000;
                            line-height: 1.1;
                            color: 'slategrey';
                        }"))),
      tags$head(tags$style(
        HTML(
          ".small-box {height: 120px;}
          .progress-bar{background-color:#3c763d;}
          #flex {display:flex;}
          #inline label{display: table-cell; text-align: center; vertical-align: middle;}
          #inline .form-group{display: table-row;}"
        )
      )),
      tabItems(
        tabItem(
          tabName = "importante",
          fluidRow(
            column(
              width = 7,
              box(
                title = "Uso adecuado del Dashboard",
                width = 12,
                info_dashboard()
              )
            ),
            column(
              width = 5,
              box(
                width = 12,
                title = "Tipos de corte para caminos, hidrografía y curvas de nivel",
                solidHeader = T,
                status = "success",
                height = "400px",
                info_cut_buffer()
              )
            )
          )
        ),
        # Predios rodales y áreas preliminares ----
        tabItem(
          tabName = "ayuda",
          fluidRow(
            column(
              width = 6,
              box(
                width = 12,
                title = "Generar Predios, rodales y áreas de corta",
                solidHeader = T,
                status = "success",
                leer_sfUI("linea_base", "Ingrese cartografía de linea base") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'Tipo_for', 'Subtipo_fo', 'Tipo_veg', 'Regulacion'"),
                leer_sfUI("obras", "Ingrese shp de obras"),
                leer_sfUI("predios", "Ingrese shp de predios") %>%
                  add_help_text(title = "Campos minimos requeridos:\n'N_Predio','Nom_Predio', 'Rol', 'Propietari'"),
                leer_sfUI("suelos", "Ingrese shp de suelos") %>%
                  add_help_text(title = "Campos minimos requeridos:\n'TEXTCAUS'"),
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
                  id = "flex",
                  actionBttn(
                    inputId = "get_area",
                    label = "Generar capas",
                    style = "unite",
                    size = "sm",
                    color = "success"
                  ),
                  downfile_ui("down_areas")
                )
              )     
            ),
            # Ayudas cartográficas ----
            column(
              width = 6,
              box(
                width = 12,
                solidHeader = T,
                status = "success",
                title = "Ayudas cartográficas",
                div(style = "margin-top: -10px"),
                h3("Ordenar shapefile"),
                leer_sfUI("sf_order","Ingrese Shapefile que desea ordenar"),
                div(style = "margin-top: -10px"),
                pickerInput(
                  inputId = "orden",
                  label = "Ordenar de:",
                  selected = "NS-OE",
                  choices = c("NS-OE","NS-EO","SN-EO","SN-OE","EO-NS","EO-SN","OE-NS","OE-SN")
                ),
                div(style = "margin-top: -10px"),
                pickerInput(
                  inputId = "select_field_order",
                  label = "Agrupar por (Opcional):",
                  multiple = T,
                  choices = c(NULL), 
                  options = list(title = "Selecciona una o más opciones")
                ),
                div(
                  id = "flex",
                  actionBttn(
                    inputId = "apply_order",
                    label = "Ordenar capa",
                    style = "unite",
                    size = "sm",
                    color = "success"
                  ) %>%  bs_embed_tooltip(title = "Crea campo 'ID_ord' con el orden"),
                  downfile_ui("down_sf_ordered")
                ),
                hr(style="height:2px;border-width:0;color:gray;background-color:gray"), 
                # Chequeo cartografía
                h3("Chequeo de cartografía"),
                leer_sfUI("sf_check","Ingrese Shapefile"),
                div(style = "margin-top: -10px"),
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
                ),
                hr(style="height:2px;border-width:0;color:gray;background-color:gray"), 
                # Agregar atributos pendientes e hidrografía
                h3("Agregar Atributos"),
                leer_sfUI("sf_to_attr","Ingrese Shapefile al cual añadir los atributos"),
                div(style = "margin-top: -10px"),
                materialSwitch(
                  inputId = "add_pend_info",
                  label = "¿Agregar pendiente media y rangos de pendientes?",
                  status = "success"
                ) %>% add_help_text("Crea o actualiza los campos 'Pend_media' y 'Ran_Pend'"),
                uiOutput("add_pend_info_ui"),
                materialSwitch(
                  inputId = "add_hidro_info",
                  label = "¿Agregar distancia a los cursos de agua?",
                  status = "success"
                ) %>% add_help_text("Crea o actualiza los campos 'Distancia' e incluye campos a elección"),
                uiOutput("add_hidro_info_ui"),
                div(
                  id = "flex",
                  actionBttn(
                    inputId = "add_attr",
                    label = "Atributar", 
                    style = "unite",
                    size = "sm",
                    color = "success"
                  ),
                  downfile_ui(id = "down_sf", style = "material-flat", label = "Shapefile"),
                  div(style = "margin-left: 10px"),
                  downfile_ui(id = "down_xlsx", style = "material-flat", label = "Excel")
                )
              )
            )
          )
        ),
        # Carto Digital ----
        tabItem(
          tabName = "carto",
          fluidRow(
            column(
              width = 6, 
              box(
                width = 12,
                title = "Generar cartografía digital",
                solidHeader = T,
                status = "success",
                leer_sfUI("cart_area", "Ingrese shapefile de áreas de corta") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'Nom_Predio', 'N_Area', 'Clase_Uso'"),
                div(style = "margin-top: -10px"),
                # RCA_UI("rca_areas"),
                leer_sfUI("cart_rodales", "Ingrese shapefile de rodales") %>%
                  add_help_text(title = "Campos minimos requeridos:\n'N_Rodal', 'Tipo_For'"),
                div(style = "margin-top: -10px"),
                # RCA_UI("rca_rodales"),
                div(style = "margin-top: -10px"),
                leer_sfUI("cart_predios", "Ingrese shapefile de limites prediales") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'N_Predio', 'Nom_Predio', 'Rol', 'Propietari"),
                div(style = "margin-top: -10px"),
                fileInput(
                  inputId = "dem",
                  label = "Ingresar DEM de Alos Palsar (12,5 x 12,5m)",
                  multiple = F,
                  accept = c(".tif",".jp2"),
                  buttonLabel = "Seleccionar",
                  placeholder = "Archivo no seleccionado"
                ) %>% 
                  add_help_text("Por favor utilizar DEM acotado al área de estudio"),
                div(style = "margin-top: -10px"),
                hr(),
                # Bd flora
                div(
                  fileInput(
                    inputId = "bd_flora",
                    label = "Ingresar BD de parcelas (Solo datos de las parcelas que se desean incluir)",
                    multiple = F,
                    accept = c(".xlsx"),
                    buttonLabel = "Seleccionar",
                    placeholder = "Archivo no seleccionado"
                  ) %>% 
                    add_help_text(
                      title = "Campos minimos requeridos:\n
                'Parcela', 'UTM_E', 'UTM_N', 'Especie', 'Copa_NS', 'Copa_EO', 'Habito'"
                    )
                ),
                div(style = "margin-top: -10px"),
                materialSwitch(
                  inputId = "add_parcelas",
                  label = "¿Crear capa de parcelas?",
                  status = "success"
                ),
                div(style = "margin-top: -10px"),
                hr(),
                # Uso actual 
                materialSwitch(
                  inputId = "add_uso_actual",
                  label = "¿Crear capa de uso actual?",
                  status = "success"
                ) %>% 
                  add_help_text(title = "Puede hacer que tarde mucho en ejecutarse la función"),
                uiOutput("add_uso_actual_ui"),
                div(style = "margin-top: -10px"),
                hr(),
                # BASES CARTOGRAFICAS
                h4("Bases cartográficas", style = "font-weight: bold;"),
                materialSwitch(
                  inputId = "add_cam",
                  label = "¿Crear capa de caminos?",
                  status = "success"
                ),
                uiOutput("add_cam_ui"),
                div(style = "margin-top: -10px"),
                materialSwitch(
                  inputId = "add_hidro",
                  label = "¿Crear capa de Hidrografía?",
                  status = "success"
                ),
                uiOutput("add_hidro_ui"),
                div(style = "margin-top: -10px"),
                materialSwitch(
                  inputId = "add_CN",
                  label = "¿Crear capa de curvas de nivel?",
                  status = "success"
                ) %>% 
                  add_help_text(title = "Puede hacer que tarde mucho en ejecutarse la función"),
                uiOutput("add_CN_ui"),
                div(style = "margin-top: -10px"),
                hr(),
                
                # NOMBRE PREDIO
                textInput("NOMPREDIO", "Ingrese un sufijo para el nombre de los archivos", placeholder = "Ej: CHILICAUQUENALTO, KIMAL, etc"),
                
                # GET AND DOWNLOAD CARTOGRAFIA 
                div(
                  id = "flex",
                  actionBttn(
                    inputId = "get_carto_btn",
                    label = "Obtener cartografía", 
                    style = "unite",
                    size = "sm",
                    color = "success"
                  ),
                  downfile_ui("down_carto")
                )
              ) # fin box
            ), # fin 1r columna
            column(
              width = 6,
              box(
                width = 12,
                title = "Generar Apéndices",
                solidHeader = T,
                status = "success",
                # GET APÉNDICES
                div(style = "margin-top: -10x"),
                h3("Apéndices", style = "font-weight: bold;"),
                div(
                  id = "inline",
                  virtualSelectInput(
                    inputId = "portada",
                    label = "Seleccionar portada :", 
                    choices = c("default", "KIMAL"),
                    selected = "default",
                    width = "200px",
                    dropboxWrapper = "body"
                  ),
                  style = "margin-bottom: 10px"
                ),
                h4("Apéndices 2 y 3 (Densidad de especies y ubicación de parcela)", style = "font-weight: bold;"),
                materialSwitch(
                  inputId = "add_bd_pcob",
                  label = "¿Desea incluir parcelas de cobertura?",
                  status = "success"
                ),
                uiOutput("add_bd_pcob_ui"),
                div(style = "margin-top: -5px"),
                div(
                  id = "flex",
                  actionBttn(
                    inputId = "get_apendices_2y3_btn",
                    label = "Obtener Apéndices 2 y 3", 
                    style = "unite",
                    size = "sm",
                    color = "success"
                  ),
                  downfile_ui("down_apendices_2", label = "Apéndice 2", style = "material-flat"),
                  downfile_ui("down_apendices_3", label = "Apéndice 3", style = "material-flat")
                ),
                br(),
                h4("Apéndice 5 - Tablas formulario CONAF", style = "font-weight: bold;"),
                div(style = "margin-top: 10px"),
                div(
                  style = "margin-left: -15px",
                  downfile_ui("tabla_attr_rodal_0",label = "Tabla atributación de rodales", style = "material-flat", icon = "file-excel"),
                ),
                div(style = "margin-top: 10px"),
                fileInput(
                  inputId = "tabla_attr_rodal",
                  label = "Ingresar tabla con los atributos de rodal definitiva y revisada",
                  multiple = F,
                  accept = c(".xlsx"),
                  buttonLabel = "Seleccionar",
                  placeholder = "Archivo no seleccionado"
                ) %>% 
                  add_help_text(title = "Cargar con los mismos atributos de como se descargó"),
                div(style = "margin-top: -5px"),
                leer_sfUI("obras_ap5", "Ingresar obras (opcional)") %>% 
                  add_help_text(title = "Campos minimos requeridos:\n'Tipo', 'Obra'"),
                div(style = "margin-top: -5px"),
                div(
                  fileInput(
                    inputId = "bd_fauna",
                    label = "Ingresar BD de fauna (opcional)",
                    multiple = F,
                    accept = c(".xlsx"),
                    buttonLabel = "Seleccionar",
                    placeholder = "Archivo no seleccionado"
                  ) %>% 
                    add_help_text(
                      title = "Campos minimos requeridos:\n'Nombre_cientifico', 'UTM_E', 'UTM_N', 'Categoria', 'Decreto'"
                    )
                ),
                div(style = "margin-top: -10px"),
                div(
                  id = "flex",
                  actionBttn(
                    inputId = "get_apendice_5_btn",
                    label = "Obtener tablas formulario", 
                    style = "unite",
                    size = "sm",
                    color = "success"
                  ),
                  downfile_ui("down_apendice_5", label = "Apéndice 5", style = "material-flat")
                )
              )
            ) # fin 2da columna
          )
        )
      )# fin tabitems
    ) # fin dashboardbody
  )# fin dashboardpage
)