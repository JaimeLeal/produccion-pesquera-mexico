bar_plot <- function(df, type, state_name) {
  if (type == "weight") {
    p <- ggplot(data = df, aes(x = ejercicio_fiscal, y = peso_ton)) +
      ggtitle(sprintf("Producción pesquera en %s en toneladas", state_name)) +
      ylab("Peso (toneladas)")
  } else {
    p <- ggplot(data = df, aes(x = ejercicio_fiscal, y = valor_millones_pesos)) +
      ggtitle(sprintf("Producción pesquera en %s en millones de pesos", state_name)) +
      ylab("Valor (millones MXN)") +
      labs(caption = "Valor nominal, no ajustado por inflación.") 
  }
  p <- p +
    geom_col(fill = "#1C4E80",
             width = 0.8) +
    xlab("Año") +
    theme_classic() + 
    scale_y_continuous(label = scales::comma) +
    scale_x_continuous(breaks =  2008:2014)
  p
}


#' dounut_plot()
#' 
#' @param dat A DataFrame with two columns "count" and "category"
#' @param my_title Title
#' @return dounut plot
dounut_plot <- plot <- function(dat, my_title = "") {
  colnames(dat) <- c("category", "count")
  dat$fraction <- dat$count / sum(dat$count)
  dat <- dat[order(dat$fraction), ]
  dat$ymax <- cumsum(dat$fraction)
  dat$ymin <- c(0, head(dat$ymax, n = -1))
  # Source: https://stackoverflow.com/a/13636037
  p <- ggplot(dat, aes(fill = category,
                       ymax = ymax,
                       ymin = ymin,
                       xmax = 4,
                       xmin = 3)) +
    geom_rect(colour = "white") +
    coord_polar(theta = "y") +
    xlim(c(0, 4)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          legend.title = element_blank()) +
  labs(title = my_title) + 
    scale_fill_manual(values = c("#EA6A47","#0091D5"))
  p
}

#' mapa_pesca()
#' 
#' @param df A Data Frame with "clave_entidad", "value"
#' @param my_title
#' @return Map
mapa_pesca <- function(df, my_title = "", hide_legend = FALSE) {
  
  mx_state <- df_mxstate %>% 
    mutate(clave_de_entidad = as.numeric(region)) 
  
  mx_state <- merge(mx_state, df, by = "clave_de_entidad", all.x = TRUE)
  mx_state$value[is.na(mx_state$value)] <- 0
  
  p <- mxstate_choropleth(mx_state, 
                          num_colors = 9,
                          title = my_title,
                          legend = "") + 
    scale_fill_brewer(palette = "YlGnBu",
                      name = "Porcentaje (%)")
  
  if (hide_legend) {
    p <- p + theme(legend.position = "none")
  }
  p
}