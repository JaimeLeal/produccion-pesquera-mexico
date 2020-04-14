library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(mxmaps)

source("scripts/plots.R", encoding = "UTF-8")

states <- df_mxstate %>% 
  select(state_name, region) %>%
  mutate(region = as.numeric(region)) %>%
  unique(.)

server <- function(input, output, session) { 
  data <- read.csv("datos/produccion_pesquera.csv", stringsAsFactors = FALSE)
  
  # Update select input with state names
  state_choices <- c(0, states$region)
  names(state_choices) <- c("Todo el país", states$state_name)
  updateSelectInput(session, "state", choices = state_choices)
  
  observeEvent(c(input$state, input$filter_by), {
    
    # Tab 1 title
    if (input$state == 0) {
      state_name <- "todo el país"
    } else{
      state_name <- states$state_name[match(input$state, states$region)]
    }
    tab1_title <- sprintf("Producción pesquera en %s", state_name)
    output$tab1_title <- renderText(tab1_title)
    
    if (input$state != 0) {
      df_pct <- data %>%
        group_by(clave_de_entidad) %>%
        summarise(value = sum(valor_pesos, na.rm = TRUE))
      df_pct <- df_pct %>% mutate(value = round(value / sum(value) * 100, 1))
      
      contrib <- df_pct %>% filter(clave_de_entidad == input$state) %>% pull(value)
    } else {
      contrib <- 0
    }
    
    # Tab 1 map
    if (input$state == 0) {
      data2 <- data
    } else {
      data2 <- data %>% filter(clave_de_entidad == input$state)
    }
    
    df_by_year <- data2 %>% 
      group_by(ejercicio_fiscal, origen) %>%
      summarise(peso_ton = sum(peso_vivo_kilogramos, na.rm =  TRUE) / 1000,
                valor_millones_pesos = sum(valor_pesos, na.rm = TRUE) /1E6)
    
    df_by_origin <- data2 %>% 
      group_by(origen) %>%
      summarise(peso_ton = sum(peso_vivo_kilogramos, na.rm =  TRUE) / 1000,
                valor_millones_pesos = sum(valor_pesos, na.rm = TRUE) / 1E6)
    
    df_map <- data2 %>%
      group_by(clave_de_entidad) %>%
      summarise(value = sum(valor_pesos, na.rm = TRUE))
    df_map <- df_map %>% mutate(value = round(value / sum(value) * 100, 1))
    
    output$bar_plot <- renderPlot(bar_plot(df_by_year, input$filter_by, state_name))
    
    if (input$state == 0) {
      output$map <- renderPlot(mapa_pesca(df_map,
                                          my_title = "Contribucción de cada estado"))
    } else {
      output$map <- renderPlot(mapa_pesca(df_map, hide_legend = TRUE))
    }
    
    if (input$filter_by == "weight") {
      output$dounut <- renderPlot(dounut_plot(df_by_origin[,c("origen", "peso_ton")], "Producción por origen"))
    } else {
      output$dounut <- renderPlot(dounut_plot(df_by_origin[,c("origen", "valor_millones_pesos")], "Producción por origen"))
    }
    
    if (input$filter_by == "weight"){
      top_species <- data2 %>%
        filter(ejercicio_fiscal == 2014) %>%
        group_by(nombre_principal, origen) %>%
        summarise(cantidad = sum(peso_vivo_kilogramos, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(cantidad = round(cantidad / sum(cantidad, na.rm = TRUE) * 100)) %>%
        top_n(n = 5) %>%
        arrange(-cantidad)
    }else{
      top_species <- data2 %>%
        filter(ejercicio_fiscal == 2014) %>%
        group_by(nombre_principal, origen) %>%
        summarise(cantidad = sum(valor_pesos, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(cantidad = round(cantidad / sum(cantidad, na.rm = TRUE) * 100)) %>%
        top_n(n = 5) %>%
        arrange(-cantidad)
    }

    colnames(top_species) <- c("Nombre", "Origen", "Porcentaje")

    output$top_species <- renderTable({
      top_species 
    })
    
    if (input$state != 0) {
      output$contrib <- renderText(
        sprintf("%s representa el %s %% de la producción nacional",
                state_name, contrib))
    }else{
      output$contrib <- renderText("")
    }
    
    
  })
  
}