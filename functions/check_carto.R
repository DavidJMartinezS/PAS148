check_carto_vieja <- function(x, id){
  list_check <- list(
    "Áreas" = c("Nom_Predio", "N_a", "Tipo_Bos", "Sup_ha"),
    "Caminos" = c("Nom_Predio", "Tipo_Cam"),
    "Curvas de nivel" = c("Nom_Predio","Cot_curva"),
    "Hidrografía" = c("Nom_Predio", "Tipo_Dren", "Tipo_Perma"),
    "Límite predial" = c("Nom_Predio", "Rol", "Sup_ha"), 
    "Parcelas" = c("Nom_Predio", "N_Rodal", "N_Parc", "Coord_X", "Coord_Y"),
    "Puntos de referencia" = c("Nom_Predio", "Nom_pto", "Coord_X", "Coord_Y"),
    "Rangos de pendiente" = c("Nom_Predio", "Ran_Pend", "Sup_ha"),
    "Rodales" = c("Nom_Predio", "N_Rodal", "Tipo_Bos", "Tipo_For", "Sup_ha"),
    "Señaletica de incendios" = c("Nom_Predio", "Nom_pto", "Coord_X", "Coord_Y"),
    "Suelos" = c("Nom_Predio", "Clase_uso", "Sup_ha"),
    "Uso actual" = c("Nom_Predio", "Uso_actual", "Sup_ha")
  )
  names_req <- list_check[[id]]
  names_act <- x %>% st_drop_geometry() %>% names()
  if((names_req %in% names_act) %>% all()){
    if((names_req == names_act) %>% all()){
      shinyalert(
        title = "OK!", 
        text = "Campos en concordancia con los requerimientos de CONAF",
        type = "success",
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    } else if(length(names_act) > length(names_req)){
      shinyalert(
        title = "Problemas!", 
        text = str_c("Shapefile con otros campos adicionales a los requeridos ",
                     "\nActual: ",str_c(names_act %>% shQuote(), collapse = ", "),
                     "\nCorrecto: ",str_c(names_req %>% shQuote(), collapse = ", ")
                     ),
        type = "warning",
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    } else {
      shinyalert(
        title = "Problemas!", 
        text = str_c("Shapefile con los campos requeridos pero desordenados",
                     "\nActual: ", str_c(names_act %>% shQuote(), collapse = ", "),
                     "\nCorrecto: ", str_c(names_req %>% shQuote(), collapse = ", ")
        ),
        type = "warning",
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    }
  } else {
    shinyalert(
      title = "Error!", 
      text = "Shapefile sin los campos requeridos",
      type = "error",
      closeOnEsc = T, 
      showConfirmButton = T,
      animation = TRUE
    )
  }
}

check_carto_nueva <- function(x, id){
  list_check <- list(
    "Áreas" = c("Nom_Predio", "N_Area", "Tipo_Bos", "Sup_ha", "Fuente"),
    "Caminos" = c("Nom_Predio", "Tipo_Cam", "Fuente"),
    "Curvas de nivel" = c("Nom_Predio","Cot_Curva", "Fuente"),
    "Hidrografía" = c("Nom_Predio", "Tip_Dren", "Tipo_Perma", "Fuente"),
    "Límite predial" = c("Nom_Predio", "Rol", "Sup_ha", "Fuente"), 
    "Parcelas" = c("Nom_Predio", "N_Rodal", "N_Parc", "Coord_X", "Coord_Y", "Fuente"),
    "Puntos de referencia" = c("Nom_Predio", "Nom_Pto", "Coord_X", "Coord_Y", "Fuente"),
    "Rangos de pendiente" = c("Nom_Predio", "Ran_Pend", "Sup_ha", "Fuente"),
    "Rodales" = c("Nom_Predio", "N_Rodal", "Tipo_Bos", "Tipo_For", "Sup_ha", "Fuente"),
    "Señaletica de incendios" = c("Nom_Predio", "Nom_Pto", "Coord_X", "Coord_Y", "Fuente"),
    "Suelos" = c("Nom_Predio", "Clase_Uso", "Sup_ha", "Fuente"),
    "Uso actual" = c("Nom_Predio", "Uso_Actual", "Sup_ha", "Fuente")
  )
  names_req <- list_check[[id]]
  names_act <- x %>% st_drop_geometry() %>% names()
  if((names_req %in% names_act) %>% all()){
    if((names_req == names_act) %>% all()){
      shinyalert(
        title = "OK!", 
        text = "Campos en concordancia con los requerimientos de CONAF",
        type = "success",
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    } else if(length(names_act) > length(names_req)){
      shinyalert(
        title = "Problemas!", 
        text = str_c(
          "Shapefile con otros campos adicionales a los requeridos ", br(), br(),
          tags$b("Requeridos: "), str_c(names_req %>% shQuote(), collapse = ", "), br(), br(),
          tags$b("Faltan: "), str_c(setdiff(names_req, names_act) %>% shQuote(), collapse = ", ")
        ),
        type = "warning",
        html = TRUE,
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    } else {
      shinyalert(
        title = "Problemas!", 
        text = str_c(
          "Shapefile con los campos requeridos pero desordenados", br(), br(),
          tags$b("\nActual: "), str_c(names_act %>% shQuote(), collapse = ", "), br(), br(),
          tags$b("\nCorrecto: "), str_c(names_req %>% shQuote(), collapse = ", ")
        ),
        type = "warning",
        html = TRUE,
        closeOnEsc = T, 
        showConfirmButton = T,
        animation = TRUE
      )
    }
  } else {
    shinyalert(
      title = "Error!", 
      text = "Shapefile sin los campos requeridos",
      type = "error",
      closeOnEsc = T, 
      showConfirmButton = T,
      animation = TRUE
    )
  }
}