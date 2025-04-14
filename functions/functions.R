add_help_text <- function(x, ...){
  x %>% shinyInput_label_embed(
    shiny_iconlink() %>%
      bs_embed_tooltip(
        ..., placement = "left"
      )
  )
}

shinyalerta <- function(names_act, names_req){
  shinyalert(
    title = "Ups!", 
    html = TRUE,
    text = tags$p(
      "Shapefile sin los campos requeridos ", br(), br(),
      tags$b("Requeridos: "), str_c(names_req %>% shQuote(), collapse = ", "), br(), br(),
      tags$b("Faltan: "), str_c(setdiff(names_req, names_act) %>% shQuote(), collapse = ", ")
    ),
    type = "error",
    closeOnEsc = T, 
    showConfirmButton = T,
    animation = T
  )
}

wb_portada_148_kimal <- function(wb, apendice, provincia) {
  mes <- Sys.Date() %>% format('%B') %>% str_to_sentence()
  yr <- Sys.Date() %>% format('%Y') %>% str_to_sentence()
  
  nom_apendice <- if_else(
    apendice == 2, "APÉNDICE 2. Densiadad de especies",
    if_else(
      apendice == 3, "APÉNDICE 3. Coordenadas ubicación de parcelas",
      if_else(
        apendice == 5, "APENDICE 5. Tablas formulario CONAF",
        ""
      )
    )
  )
    
  wb <- wb %>% wb_add_worksheet("Portada", grid_lines = F) %>% 
    wb_page_setup(paper_size = 1) %>% 
    # EIA
    wb_add_data(x = "ESTUDIO DE IMPACTO AMBIENTAL", start_col = 2, start_row = 16) %>% 
    wb_add_font(dims = "B16", bold = T, size = 14) %>% 
    wb_add_cell_style(dims = "B16", horizontal = "center", vertical = "center") %>% 
    wb_merge_cells(dims = wb_dims(rows = 16, cols = 2:5), solve = T) %>% 
    # proyecto
    wb_add_data(x = str_to_upper("LÍNEA DE TRANSMISIÓN ELÉCTRICA HVDC KIMAL - LO AGUIRRE"), dims = "A18") %>% 
    wb_add_font(dims = "A18",bold = T, size = 16) %>% 
    wb_add_cell_style(dims = "A18", horizontal = "center", vertical = "center") %>% 
    wb_merge_cells(dims = wb_dims(rows = 18:19, cols = 1:6), solve = T) %>% 
    # apéndice
    wb_add_data(x = nom_apendice, dims = "B22") %>% 
    wb_add_cell_style(dims = "B22", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 22, cols = 2:5), solve = T) %>% 
    wb_add_data(x = str_c("PAS 148 - Provincia de ", provincia, ", ", provincias_list[grep(provincia, provincias_list)] %>% names()), dims = "A24") %>% 
    wb_add_cell_style(dims = "A24", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 24, cols = 1:6), solve = T) %>% 
    # Elaborado por
    wb_add_data(x = "Elaborado por Geobiota para:", start_col = 2, start_row = 33) %>% 
    wb_add_font(dims = "B33", bold = T, size = 14) %>% 
    wb_add_cell_style(dims = "B33", horizontal = "center", vertical = "center") %>% 
    wb_merge_cells(dims = wb_dims(rows = 33, cols = 2:5), solve = T) %>% 
    wb_add_image(dims = "C35",file = "./www/CONEXION.png", width = 6.5, height = 2, units = 'cm') %>%
    # Fecha
    wb_add_data(x = str_c(mes,yr, sep = ", "),dims = "C43") %>% 
    wb_add_cell_style(dims = "C43", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 43, cols = 3:4), solve = T) %>% 
    wb_set_col_widths(cols = 1:6, widths = 13)  
  return(wb)
}

wb_portada_148_default <- function(wb, apendice, nom_proj = NULL, provincia){
  mes <- Sys.Date() %>% format('%B') %>% str_to_sentence()
  yr <- Sys.Date() %>% format('%Y') %>% str_to_sentence()
  
  if (is.null(nom_proj)) {
    nom_proj <- "INGRESE NOMBRE DEL PROYECTO"
  }
  
  nom_apendice <- if_else(
    apendice == 2, "APÉNDICE 2. Densiadad de especies",
    if_else(
      apendice == 3, "APÉNDICE 3. Coordenadas ubicación de parcelas",
      if_else(
        apendice == 5, "APENDICE 5. Tablas formulario CONAF",
        ""
      )
    )
  )
  
  wb <- wb %>% wb_add_worksheet("Portada", grid_lines = F) %>% 
    wb_page_setup(paper_size = 1) %>% 
    # EIA
    wb_add_data(x = "ESTUDIO DE IMPACTO AMBIENTAL", start_col = 2, start_row = 16) %>% 
    wb_add_font(dims = "B16", bold = T, size = 14) %>% 
    wb_add_cell_style(dims = "B16", horizontal = "center", vertical = "center") %>% 
    wb_merge_cells(dims = wb_dims(rows = 16, cols = 2:5), solve = T) %>% 
    # proyecto
    wb_add_data(x = str_to_upper(nom_proj), dims = "A18") %>% 
    wb_add_font(dims = "A18",bold = T, size = 16) %>% 
    wb_add_cell_style(dims = "A18", horizontal = "center", vertical = "center") %>% 
    wb_merge_cells(dims = wb_dims(rows = 18:19, cols = 1:6), solve = T) %>% 
    # apéndice
    wb_add_data(x = nom_apendice, dims = "B22") %>% 
    wb_add_cell_style(dims = "B22", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 22, cols = 2:5), solve = T) %>% 
    wb_add_data(x = str_c("PAS 148 - Provincia de ", provincia, ", ", provincias_list[grep(provincia, provincias_list)] %>% names()), dims = "A24") %>% 
    wb_add_cell_style(dims = "A24", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 24, cols = 1:6), solve = T) %>% 
    # Elaborado por
    wb_add_data(x = "Elaborado por Geobiota para:", start_col = 2, start_row = 31) %>% 
    wb_add_font(dims = "B31", bold = T, size = 14) %>% 
    wb_add_cell_style(dims = "B31", horizontal = "center", vertical = "center") %>% 
    wb_merge_cells(dims = wb_dims(rows = 31, cols = 2:5), solve = T) %>% 
    wb_add_image(dims = "C33",file = "./www/logo_default.png", width = 135.2899, height = 141.7323, units = 'px', dpi = 72) %>%
    # Fecha
    wb_add_data(x = str_c(mes,yr, sep = ", "),dims = "C43") %>% 
    wb_add_cell_style(dims = "C43", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 43, cols = 3:4), solve = T) %>% 
    wb_set_col_widths(cols = 1:6, widths = 13)  
  return(wb)
}

# wb <- wb_workbook() %>%
#   wb_portada_148_default(apendice = 2, provincia = provincias_list$Coquimbo[2])
# wb$open()
