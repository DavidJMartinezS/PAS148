check_bd_flora <- function(x, y = NULL, shinyalert = F){
  if(!is.null(y) & any(st_is(y, "POLYGON") | st_is(y, "MULTIPOLYGON")) & all(c('UTM_E', 'UTM_N') %in% names(x))){
    x <- x %>% 
      st_as_sf(coords = c("UTM_E", "UTM_N"), crs = st_crs(y), remove = F) %>% 
      st_intersection(st_union(st_collection_extract(y, "POLYGON"))) %>% 
      st_drop_geometry()
  } %>% suppressWarnings()
  # Verificar que estén los campos mínimos ----
  if (!all(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB', 'Tipo_veg') %in% names(x))) {
    if (condition) {
      shinyalerta(names_act = names(x), names_req = c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB'))
    } else {
      cat(
        "\033[31mProblemas!","\U0001FAE0","Shapefile sin los campos requeridos","\n",
        "\033[34mRequeridos: ", str_c(names_req %>% shQuote(), collapse = ", "), "\n",
        "Faltan: ", str_c(setdiff(names_req, names_act) %>% shQuote(), collapse = ", ")
      )
    }
  } else {
    # Verificar coberturas ----
    if (!x$Cob_BB %in% c("fp", "r", "+", "1", "2", "3", "4", "5", "---") %>% all()) {
      if (shinyalert) {
        shinyalert(
          title = "Coberturas de Braun-Blanquet que no corresponden", 
          html = TRUE,
          text = tags$p(
            "Las coberturas de Braun-Blanquet deben limitarse a las siguientes nomenclaturas:", 
            br(), br(),
            str_c(
              c("fp", "r", "+", "1", "2", "3", "4", "5", "---") %>% shQuote(), 
              collapse = ", "
            )
          ),
          type = "error",
          closeOnEsc = T, 
          showConfirmButton = T,
          animation = T
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0","Coberturas de Braun-Blanquet que no corresponden","\n",
          "\033[34mLas coberturas de Braun-Blanquet deben limitarse a las siguientes nomenclaturas:","\n",
          str_c(
            c("fp", "r", "+", "1", "2", "3", "4", "5", "---") %>% shQuote(), 
            collapse = ", "
          ),"\n"
        )
      }
    }
    # Verificar coordenadas
    if (!x$UTM_E %>% str_count() %>% all(. == 6) | !x$UTM_N %>% str_count() %>% all(. == 7)) {
      if (shinyalert) {
        shinyalert(
          title = "Error en coordenadas", 
          html = TRUE,
          text = tags$p(
            "Revisar las coordenadas de las siguientes parcelas:", br(), br(),
            str_c(
              x$Parcela[which(!x$UTM_E %>% str_count() %>% .[] == 6)] %>% 
                c(x$Parcela[which(!x$UTM_N %>% str_count() %>% .[] == 7)]) %>% 
                unique() %>% shQuote(), 
              collapse = ", "
            )
          ),
          type = "error",
          closeOnEsc = T, 
          showConfirmButton = T,
          animation = T
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0", "Error en coordenadas", "\n",
          "\033[34mRevisar las coordenadas de las siguientes parcelas:","\n",
          str_c(
            x$Parcela[which(!x$UTM_E %>% str_count() %>% .[] == 6)] %>% 
              c(x$Parcela[which(!x$UTM_N %>% str_count() %>% .[] == 7)]) %>% 
              unique() %>% shQuote(), 
            collapse = ", "
          ),"\n"
        )
      }
    }
    if (c("Campana", "Cuadrilla") %in% names(x) %>% all()) {
      # Verificar coordenadas repetidas
      if (
        x %>% 
        count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
        count(Campana, Cuadrilla, Parcela, sort = T) %>% 
        filter(n > 1) %>% nrow() %>% .[] >= 1
      ) {
        if (shinyalert) {
          shinyalert(
            title = "Mismas parcela, diferentes coordenadas!", 
            html = TRUE,
            text = tags$p(
              "Las siguientes parcelas presentan mas de una coordenada teniendo la misma campaña y cuadrilla:", 
              br(), br(),
              str_c(
                x %>% 
                  count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
                  count(Campana, Cuadrilla, Parcela, sort = T) %>% 
                  filter(n > 1) %>% pull(Parcela) %>% shQuote(), 
                collapse = ", "
              )
            ),
            type = "error",
            closeOnEsc = T, 
            showConfirmButton = T,
            animation = T
          )
        } else {
          cat(
            "\033[31mProblemas!","\U0001FAE0", "Mismas parcela, diferentes coordenadas!", "\n",
            "\033[34mLas siguientes parcelas presentan mas de una coordenada teniendo la misma campaña y cuadrilla:","\n",
            str_c(
              x %>% 
                count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
                count(Campana, Cuadrilla, Parcela, sort = T) %>% 
                filter(n > 1) %>% pull(Parcela) %>% shQuote(), 
              collapse = ", "
            ),"\n"
          )
        }
      }
      # Verificar cuadrillas repetidas
      if (
        x %>% 
        count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
        count(Campana, Parcela, UTM_E, UTM_N, sort = T) %>% 
        filter(n > 1) %>% nrow() %>% .[] >= 1
      ) {
        if (shinyalert) {
          shinyalert(
            title = "Mismas parcela, diferentes cuadrillas!", 
            html = TRUE,
            text = tags$p(
              "Las siguientes parcelas presentan mas de una cuadrilla teniendo la misma campaña y coordenada:", 
              br(), br(),
              str_c(
                x %>% 
                  count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
                  count(Campana, Parcela, UTM_E, UTM_N, sort = T) %>% 
                  filter(n > 1) %>% pull(Parcela) %>% shQuote(), 
                collapse = ", "
              )
            ),
            type = "error",
            closeOnEsc = T, 
            showConfirmButton = T,
            animation = T
          )
        } else {
          cat(
            "\033[31mProblemas!","\U0001FAE0", "Mismas parcela, diferentes cuadrillas!", "\n",
            "\033[34mLas siguientes parcelas presentan mas de una cuadrilla teniendo la misma campaña y coordenada:", "\n",
            str_c(
              x %>% 
                count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
                count(Campana, Parcela, UTM_E, UTM_N, sort = T) %>% 
                filter(n > 1) %>% pull(Parcela) %>% shQuote(), 
              collapse = ", "
            ),"\n"
          )
        }
      }
      # Verificar campañas repetidas
      if (
        x %>% 
        count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
        count(Cuadrilla, Parcela, UTM_E, UTM_N, sort = T) %>% 
        filter(n > 1) %>% nrow() %>% .[] >= 1
      ) {
        if (condition) {
          shinyalert(
            title = "Mismas parcela, diferentes campañas!", 
            html = TRUE,
            text = tags$p(
              "Las siguientes parcelas se repiten en más de una campaña:", 
              br(), br(),
              str_c(
                x %>% 
                  count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
                  count(Cuadrilla, Parcela, UTM_E, UTM_N, sort = T) %>% 
                  filter(n > 1) %>% pull(Parcela) %>% shQuote(),
                collapse = ", "
              )
            ),
            type = "error",
            closeOnEsc = T, 
            showConfirmButton = T,
            animation = T
          )
        } else {
          cat(
            "\033[31mProblemas!","\U0001FAE0", "Mismas parcela, diferentes campañas!", "\n",
            "\033[34mLas siguientes parcelas se repiten en más de una campaña:", "\n",
            str_c(
              x %>% 
                count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
                count(Cuadrilla, Parcela, UTM_E, UTM_N, sort = T) %>% 
                filter(n > 1) %>% pull(Parcela) %>% shQuote(),
              collapse = ", "
            ),"\n"
          )
        }
      }
    } else {
      # Verificar coordenadas repetidas
      if (!x %>% count(Parcela, UTM_E, UTM_N) %>% count(Parcela, sort = T) %>% pull(n) %>% all(.==1)) {
        if (shinyalert) {
          shinyalert(
            title = "Mismas parcela, diferentes coordenadas!", 
            html = TRUE,
            text = tags$p(
              "Las siguientes parcelas presentan mas de una coordenada:", br(), br(),
              str_c(
                x %>% 
                  count(Parcela, UTM_E, UTM_N) %>% count(Parcela) %>% 
                  filter(Parcela > 1) %>% pull(Parcela) %>% shQuote(), 
                collapse = ", "
              )
            ),
            type = "error",
            closeOnEsc = T, 
            showConfirmButton = T,
            animation = T
          )
        } else {
          cat(
            "\033[31mProblemas!","\U0001FAE0", "Mismas parcela, diferentes coordenadas!", "\n",
            "\033[34mLas siguientes parcelas presentan mas de una coordenada:", "\n",
            str_c(
              x %>% 
                count(Parcela, UTM_E, UTM_N) %>% count(Parcela) %>% 
                filter(Parcela > 1) %>% pull(Parcela) %>% shQuote(), 
              collapse = ", "
            ),"\n"
          )
        }
      }
    }
  } %>% suppressWarnings()
}

check_bd_pcob <- function(x){
  
}