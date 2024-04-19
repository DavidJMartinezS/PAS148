shinyServer(function(input,output,session){
  # Author ----
  output$user <- renderUser({
    dashboardUser(
      name = "David Martínez",
      image = "https://avatars.githubusercontent.com/u/74486319?s=400&u=c277213b232af5e7710bebdc7a50bb9426ab9a62&v=4",
      title = "Dashboard PAS 148",
      subtitle = "Autor",
      footer = fluidRow(
        tags$p("Mi Github", socialButton(href = "https://github.com/DavidJMartinezS", icon = icon("github")), class = "text-center"),
        tags$p("Geobiota", socialButton(href = "https://google.com", icon = icon("globe")), class = "text-center"),
      ),
      "Especialista en plantas de Geobiota"
    )
  })
  
  LB <- callModule(module = leer_sf, id = "linea_base")
  obras <- callModule(module = leer_sf, id = "obras")
  predio <- callModule(module = leer_sf, id = "predio")
  
  predio <- callModule(module = leer_sf, id = "predio")
  predio <- callModule(module = leer_sf, id = "predio")
  
  
  
})