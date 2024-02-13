info_dashboard <- function(){
  bs_accordion(id = "accordion") %>%
    bs_set_opts(use_heading_link = T,
                panel_type = "success") %>%
    bs_append(title = "Objetivos e instrucciones del Dashboard",
              content = tags$p("Este Dashboard esta elaborado con el objetivo de apoyar la elaboración del PAS 148, proporcionando 
                               resultados descargables de capas y tablas (formatos .shp y .xlsx respectivamente).")) %>%
    bs_append(title = "Inputs y chequeo de datos",
              content = tags$div(
                tags$p(
                  "En esta sección debe cargar los shp de línea base, obras, y predios. Para ingresar los shapefiles, debe cargar
                  los 4 archivos necesarios para su lectura, que son: 'dbf', 'prj' 'shp' y 'shx'. De otro modo no se reconocerá el
                  shapefile."
                ),
                tags$p("cada uno de estos debe cumplis con los siguientes requisitos:"),
                tags$ul(
                  tags$li("Shapefile de línea base",
                          tags$ul(
                            tags$li(
                              "Debe presentar al menos los siguientes campos:",
                              tags$br(),
                              "Formacion', 'Tipo_Veg', 'Tipo_For', 'Subtipo_Fo', 'Regulacion'"
                            ),
                            tags$li(
                              "Si bien no es necesario que coincidan las mayúsculas o tildes, si lo deben hacer los caractéres alfa-numéricos"
                            ),
                            tags$li(
                              "En el campo 'Regulacion' debe señalar si el poligono corresponde a bosque nativo, formación xerofítica,
                        o alguna otra formación regulada por la ley"
                            )
                          )),
                  tags$li("Shapefile layout de obras",
                          tags$ul(
                            tags$li(
                              "Cargar un shapefile del compilado de obras. Este debe ser de geometría tipo polígono, el cual debe contener
                              al menos el campo 'Obra' que señale el nombre de la obra"
                            )
                          )),
                  tags$li("Shapefile de predios",
                          tags$ul(
                            tags$li(
                              "Cargar un shapefile con la información predial de las áreas de intervención.
                              Este debe contener al menos los siguientes campos:",
                              tags$br(),
                              "'Propietario','Nom_Predio','Rol'"
                            )
                          )),
                )
              )
    ) %>%
    bs_append(title = "Intervención",
              content = "Para poder ver las tablas en esta pestaña debe haber generado previamente la cartografía digital en la pestaña 'Cartografía digital'") %>%
    bs_append(title = "Análisis de amenazas",
              content = "Seleccione los subusos de suelo que no correspondan a vegetación. Incluir en ese listado los terrenos agrícolas. Luego cargue el XLS que genera el equipo de Cartografía, en el cual se encuentran los resultados del análisis en FragStats con las condiciones antes y despues del proyecto en las hojas 1 y 2 del XLS, respectivamente. Al presionar el boton 'Generar análisis', podrá descargar el excel con los resultados y calculos, además de cargarle el contenido de los cuadros que aparecen en la página") 
}
