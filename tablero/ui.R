library(shinydashboard)

dashboardPage(
  dashboardHeader(title =  "Producción Pesquera"),
  dashboardSidebar(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    sidebarMenu(
      menuItem("Producción", tabName = "country-view", icon = icon("fish"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("country-view",
              fluidRow(column(12, textOutput("tab1_title"), class = "title")),
              fluidRow(
                column(2, selectInput("state", "Selecciona el estado", c())),
                column(3, selectInput("filter_by", "Filtro",
                                      c("Toneladas producidas" = "weight",
                                        "Valor (millones de pesos)" = "value")))
              ),
              fluidRow(
                column(6,
                       fluidRow(plotOutput("bar_plot", height = "250px")),
                       fluidRow(plotOutput("map", height = "250px")),
                       fluidRow(textOutput("contrib"), class = "caption")
                ),
                column(6, align = "center",
                       fluidRow(plotOutput("dounut", height = "250px")),
                       fluidRow(h4("Contribución por especie"),
                                tableOutput("top_species"))
                ))
      )
    )
  )
)