# funciones de apooyo
cup_coverage <- function(x, y, method = "circle", Na.value = 0){
  stopifnot(method %in% c("circle", "ellipse"))
  
  if (method == "circle") {
    val <- pi*(mean(c(x,y), na.rm = T)/2)^2
  } 
  if (method == "ellipse"){
    val <- pi * x/2 * y/2
  }
  if (is.na(val)) {
    return(Na.value)
  } else {
    return(val)
  }
}

plot_coverage <- function(x, plot_size = 500, percent = F, digits = 1, ...){
  x <- sum(x, na.rm = T)/plot_size
  if (percent) {
    paste0(formatC(x * 100, format = "f", digits = digits, ...), "%")
  } else {
    formatC(x, digits = digits, format = "f", ...)
  }
}


# Funciones Apénices
apendice_2_bd <- function(bd_flora, bd_pcob = NULL, rodales, portada = "default", provincia){
  bd_flora <- bd_flora %>% 
    rename_if(names(.) %>% stri_trans_general("Latin-ASCII") %>% stri_cmp_equiv("campana", strength = 1), ~ "Campana") 
  stopifnot(all(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB') %in% names(bd_flora)))
  stopifnot(all(c('Parcela', 'Especie', 'Copa_NS', 'Copa_EO') %in% names(bd_pcob)))
  stopifnot(all(c('Nom_Predio', 'N_Rodal') %in% names(rodales)))
  
  parcelas_rodales <- bd_flora %>% 
    filter(
      Habito %>% stri_trans_general("Latin-ASCII") %>% stri_detect_regex("arbol", case_insensitive = T),
      !Cob_BB %>% str_to_lower() %in% c(NA_character_, "fp")
    ) %>% 
    st_as_sf(coords = c("UTM_E","UTM_N"), crs = st_crs(rodales), remove = F) %>% 
    st_intersection(st_union(rodales)) %>%
    st_join(rodales %>% select(Nom_Predio, N_Rodal)) %>% 
    mutate_at("N_Rodal", as.integer) %>% 
    st_drop_geometry() %>% 
    mutate_at("N_ind", as.integer) %>% 
    mutate(Nha = N_ind * 20) %>% 
    arrange(N_Rodal) %>%
    group_by(Parcela, UTM_E, UTM_N) %>%
    arrange(N_Rodal) %>% 
    mutate(N = cur_group_id()) %>% 
    group_by(N_Rodal, N) %>% 
    mutate(N_Parc = cur_group_id()) %>% 
    ungroup() 
  
  # parcelas <- parcelas_rodales %>% 
  #   count(Nom_Predio, N_Rodal, N_Parc, UTM_E, UTM_N) %>% select(-n) %>% 
  #   mutate(Fuente = "Elaboracion propia") %>% 
  #   rename(Coord_X = UTM_E, Coord_Y = UTM_N) %>% 
  #   mutate_at(vars(starts_with("Coord")), round_half_up) %>% 
  #   arrange(N_Parc) %>% 
  #   select(Nom_Predio, N_Rodal, N_Parc, Coord_X, Coord_Y, Fuente)
  
  if (!is.null(bd_pcob)) {
    df_cob <- bd_pcob %>% 
      select(-starts_with("UTM")) %>% 
      inner_join(
        parcelas_rodales %>% count(Campana, Parcela, N_Parc, UTM_E, UTM_N) %>% select(-n)
      ) %>% 
      mutate(
        Cob_arb = map2_dbl(Copa_NS, Copa_EO, cup_coverage, method = "ellipse")
      ) %>% 
      group_by(N_Parc, UTM_E, UTM_N, Especie) %>% 
      summarise(
        Cob_arb = sum(Cob_arb, na.rm = T),
        Nha = n() * 20
      ) %>% 
      group_by(N_Parc) %>% 
      mutate(
        Cob_parc_p = plot_coverage(Cob_arb, percent = T, digits = 1, decimal.mark = ","),
        Cob_parc = map_dbl(Cob_parc_p, ~as.numeric(str_extract(str_replace(., ",", "\\."),"\\d+\\.\\d"))),
        Fuente = "PCob"
      ) %>% 
      ungroup() %>% 
      mutate_at("Cob_arb", ~round_half_up(. / 500, 2)) %>% 
      select(N_Parc, UTM_E, UTM_N, Especie, Nha, Cob_arb, Cob_parc, Fuente) %>% 
      {if(nrow(.[]) == 0) NULL else .}
  }
  
  df_flora <- parcelas_rodales %>% 
    {if(count(.[],N_Parc, Especie, sort = T) %>% filter(n > 1) %>% nrow() >= 1){
      .[] %>% group_by(N_Parc, UTM_E, UTM_N, Especie) %>% slice_sample() %>% ungroup()
    } else .[]} %>% 
    mutate(
      Cob_arb = case_when(
        Cob_BB %in% c("r", "+", "1") ~ 2.5,
        Cob_BB == "2" ~ 15,
        Cob_BB == "3" ~ 37.5,
        Cob_BB == "4" ~ 62.5,
        Cob_BB == "5" ~ 87.5,
        .default = 0
      ),
      Fuente = "Flora"
    ) %>% 
    group_by(N_Parc) %>% 
    mutate(Cob_parc = sum(Cob_arb, na.rm = T)) %>%
    ungroup() %>% 
    select(N_Parc, UTM_E, UTM_N, Especie, Nha, Cob_arb, Cob_parc, Fuente)
  
  ft_ap2 <- bind_rows(df_cob, df_flora) %>% 
    pivot_wider(
      names_from = Fuente,
      values_from = c(Nha, Cob_arb, Cob_parc)
    ) %>% 
    arrange(N_Parc, Especie) %>%  
    select(N_Parc, UTM_E, UTM_N, Especie, ends_with("PCob"), ends_with("Flora")) %>% 
    `names<-`(
      c(
        "Parcela",
        str_c("Coordenadas UTM Datum WGS84 Huso ", huso, "_Coordenada Este"), 
        str_c("Coordenadas UTM Datum WGS84 Huso ", huso, "_Coordenada Norte"), 
        "Especie",
        {if(!is.null(df_cob)){
          c("P.COB_NHA\n(árb/ha)",
            "P.COB_Cobertura por especie (%)",
            "P.COB_Cobertura por parcela (%)")
        }},
        "FLORA_NHA\n(árb/ha)",
        "FLORA_Cobertura por especie (%)",
        "FLORA_Cobertura por parcela (%)"
      )
    ) %>% 
    flextable() %>% 
    merge_v(j = c(1:3, 7, 10)) %>% 
    separate_header(split = "_") %>% 
    italic(j = 4) %>% 
    autofit() %>% 
    theme_box() %>% 
    valign(part = "header", valign = "center") %>% 
    align(part = "header", align = "center") %>% 
    bg(bg = "#bcc5d4", part = "header")
  
  wb_ap2 <- wb_workbook(theme = "Integral") %>% 
    {if(portada == "KIMAL"){
      wb_portada_148_kimal(., apendice = 2, provincia = provincia)
    } else {
      wb_portada_148_default(., apendice = 2, provincia = provincia)
    }} %>% 
    wb_add_worksheet("SP_Nha_y_Cobertura_Parcelas", grid_lines = F) %>% 
    wb_add_flextable(sheet = "SP_Nha_y_Cobertura_Parcelas", ft = ft_2, start_col = 1, start_row = 1)
  
  return(wb_ap2)
}
apendice_3_bd <- function(bd_flora){
  
}
apendice_5_bd <- function(bd_flora){
  
}
get_apendices <- function(bd_flora){
  
}