shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "PAS 148",
      userOutput("user")
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Importante", tabName = "importante", icon = icon("circle-info")),
        menuItem("Inputs", tabName = "inputs", icon = icon("file-import"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "importante",
          fluidRow(
            box(
              title = "Uso adecuado del Dashboard",
              width = 12,
              info_dashboard()
            )
          )
        )
      ),
      tabItems(
        tabItems(
          tabName = "inputs",
          fluidRow(
            box(
              title = "Input",
              solidHeader = T,
              status = "success",
              leer_sfUI("linea_base", "Ingrese cartografía de linea base"),
              leer_sfUI("obras", "Ingrese shp de obras"),
            )
          )
        )
      )
    )
  )
)