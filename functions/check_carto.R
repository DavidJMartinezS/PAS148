check_carto <- function(x, id){
  list_check <- list(
    "Áreas" = c("Nom_Predio","N_a","Tipo_Bos","Sup_ha"),
    "Caminos" = c("Nom_Predio","Tipo_Cam"),
    "Curvas de nivel" = c("Nom_Predio","Cot_curva"),
    "Hidrografía" = c("Nom_Predio","Tipo_Dren","Tipo_Perma"),
    "Límite predial" = c("Nom_Predio","Rol","Sup_ha"),
    "Parcelas" = c("N_Rodal","N_Parc","Coord_X","Coord_Y"),
    "Puntos de referencia" = c("Nom_Predio","Nom_pto","Coord_X","Coord_Y"),
    "Rangos de pendiente" = c("Nom_Predio","Ran_Pend","Sup_ha"),
    "Rodales" = c("Nom_Predio","N_a","Tipo_Bos","Sup_ha"),
    "Suelos" = c("Nom_Predio","N_Rodal","Tipo_Bos","Tipo_For","Sup_ha")
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
                     "\nActual: ",str_c(names_act %>% shQuote(), collapse = ", "),
                     "\nCorrecto: ",str_c(names_req %>% shQuote(), collapse = ", ")
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

