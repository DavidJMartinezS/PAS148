info_dashboard <- function(){
  bs_accordion(id = "accordion") %>%
    bs_set_opts(
      use_heading_link = T,
      panel_type = "success"
    ) %>%
    bs_append(
      title = "Objetivos e instrucciones del Dashboard",
      content = tags$p("Este Dashboard esta elaborado con el objetivo de apoyar la elaboración del PAS 148, proporcionando 
                               resultados descargables de capas y tablas (formatos .shp y .xlsx respectivamente).")
    ) %>%
    bs_append(
      title = "Consideraciones generales",
      content = tags$div(
        tags$ul(
          tags$li(
            "Para ingresar los shapefiles, debe cargar los 4 archivos necesarios para su lectura, que son: 'dbf', 'prj' 'shp' y 'shx'. 
            De otro modo no se reconocerá el shapefile.",
          ),
          tags$li(
            "Los shapefile ingresados deben contener los campos requeridos que se señalan. Deben contener los mismo caracteres que se 
            indican, pero pueden diferir en los tildes y mayúsculas",
          ),
          tags$li(
            "Por favor ingresar los shapefiles con los campos que se requieren para un buen funcionamiento del dashboard y evitar 
            errores en la obtención de los resultados",
          ),
          tags$li(
            "No confiar 100% en los resultados, haga siempre un chequeo de los resultados, tanto shapefiles como tablas",
          )
        )
      )
    ) %>%
    bs_append(
      title = "Consideraciones particulares",
      content = tags$div(
        tags$ul(
          tags$li(
            "Ayudas cartográficas",
            tags$ul(
              tags$li(
                "Ordenar Shapefiles",
                tags$ul(
                  tags$li(
                    "Esta funcion crea una columna llamada 'ID_ord' el cual enumera los poligonos trantando de ordenarlos de 
                    norte a sur y de oeste considerando la contiguidad de los poligonos"
                  ),
                  tags$li(
                    "Puede crear una enumeración por grupo. Por ejemplo si ingresa un shapefile de áreas de corta y quiere enumerarlas 
                    por predio, entonces seleccione el campo que indentifica cada predio. Puede agrupar por multiples campos"
                  ),
                  tags$li(
                    "Tener en consideración que los resultados pueden variar mucho dependiendo de la disposición espcial y 
                    forma de los poligonos. Funciona mejor en poligonos dispuestos de forma más lineal"
                  )
                )
              ),
              tags$li(
                "Generar rodales y áreas de corta",
                tags$ul(
                  tags$li(
                    "Esta función es para generar los rodales y áreas de corta a partir de los shp cargador anteriormente.
                    La app da diferentes opciones de como generar las áreas de corta"
                  ),
                  tags$li(
                    "Puede agrupar o no los rodales de linea base. Si agrupa por Tipo forestal, entonces dos poligonos contiguos 
                    del mismo tipo forestal serán unidas sus geometrías"
                  ),
                  tags$li(
                    "Si elige la opción de separar por clase de uso de suelo (CUS) entonces un área de corta con 2 CUS será 
                    dividida por su geometría, de lo contrario no se divide el poligono pero queda el campo con las dos CUS"
                  )
                )
              )
            )
          ),
          tags$li(
            "Cartografía"
          )
        )
      )
    ) 
}

info_cut_buffer <- function(){
  bs_carousel(id = "hidro_example", use_indicators = T, use_controls = T) %>%
    bs_set_data(interval = FALSE) %>%
    bs_append(content = bs_carousel_image(src = "https://github.com/DavidJMartinezS/PAS148/blob/main/www/clip.png?raw=true")) %>%
    bs_append(content = bs_carousel_image(src = "https://github.com/DavidJMartinezS/PAS148/blob/main/www/buffer_2000.png?raw=true")) %>%
    bs_append(content = bs_carousel_image(src = "https://github.com/DavidJMartinezS/PAS148/blob/main/www/crop.png?raw=true")) %>%
    bs_append(content = bs_carousel_image(src = "https://github.com/DavidJMartinezS/PAS148/blob/main/www/crop_2000.png?raw=true")) %>%  
    bs_append(content = bs_carousel_image(src = "https://github.com/DavidJMartinezS/PAS148/blob/main/www/crop_by_row.png?raw=true")) %>%  
    bs_append(content = bs_carousel_image(src = "https://github.com/DavidJMartinezS/PAS148/blob/main/www/crop_by_row_2000.png?raw=true"))
}