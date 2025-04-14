check_bd_flora <- function(x){
  # Verificar que estén los campos mínimos ----
  if (!all(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB') %in% names(x))) {
    shinyalerta(names_act = names(x), names_req = c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB'))
  } else {
    # Verificar coberturas ----
    if (!x$Cob_BB %in% c("fp", "r", "+", "1", "2", "3", "4", "5", "---")) {
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
    }
    # Verificar coordenadas
    if (!x$UTM_E %>% str_count() %>% all(. == 6) | !x$UTM_N %>% str_count() %>% all(. == 7)) {
      shinyalert(
        title = "Error en coordenadas", 
        html = TRUE,
        text = tags$p(
          "Revisar las coordenadas de las siguientes parcelas ", br(), br(),
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
    }
    if (c("Campana", "Cuadrilla") %in% names(x) %>% all()) {
      # Verificar coordenadas repetidas
      if (
        x %>% 
        count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
        count(Campana, Cuadrilla, Parcela, sort = T) %>% 
        filter(n > 1) %>% nrow() %>% .[] >= 1
      ) {
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
      }
      # Verificar cuadrillas repetidas
      if (
        x %>% 
        count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
        count(Campana, Parcela, UTM_E, UTM_N, sort = T) %>% 
        filter(n > 1) %>% nrow() %>% .[] >= 1
      ) {
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
      }
      # Verificar campañas repetidas
      if (
        x %>% 
        count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>% 
        count(Cuadrilla, Parcela, UTM_E, UTM_N, sort = T) %>% 
        filter(n > 1) %>% nrow() %>% .[] >= 1
      ) {
        shinyalert(
          title = "Mismas parcela, diferentes campañas!", 
          html = TRUE,
          text = tags$p(
            "Las siguientes parcelas se repiten en más de una campaña", 
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
      }
    } else {
      # Verificar coordenadas repetidas
      if (!x %>% count(Parcela, UTM_E, UTM_N) %>% count(Parcela, sort = T) %>% pull(n) %>% all(.==1)) {
        shinyalert(
          title = "Mismas parcela, diferentes coordenadas!", 
          html = TRUE,
          text = tags$p(
            "Las siguientes parcelas presentan mas de una coordenada ", br(), br(),
            str_c(x %>% count(Parcela, UTM_E, UTM_N) %>% count(Parcela) %>% filter(Parcela > 1) %>% pull(Parcela) %>% shQuote(), collapse = ", ")
          ),
          type = "error",
          closeOnEsc = T, 
          showConfirmButton = T,
          animation = T
        )
      }
    }
  }
}

check_bd_pcob <- function(x){
  
}