library(dplyr)

# Download files
if (!dir.exists("datos")) {
  dir.create("datos")
}
base_url <-  "http://www.conapesca.gob.mx/work/sites/cona/datosabiertos/"
for (y in 2008:2014) {
  file_name <- sprintf("Produccion_Pesquera_%s.csv", y)
  download.file(paste0(base_url, file_name),
                file.path("datos", file_name))
}

# Read files and process them
files <- list.files("datos/", full.names = TRUE) %>% 
  grep("Produccion_Pesquera_20", ., fixed = TRUE, value = TRUE)

dfs <- lapply(files, read.csv)
data <- Reduce(rbind, dfs)

remove_commas <- function(x) {
  gsub(",", "", x)
}

data <- data %>%
  mutate(NOMBRE.PRINCIPAL = as.character(NOMBRE.PRINCIPAL)) %>%
  mutate(NOMBRE.PRINCIPAL = if_else(grepl("TILAPIA", NOMBRE.COMUN),
                                   "TILAPIA",
                                   NOMBRE.PRINCIPAL,
                                   NOMBRE.PRINCIPAL))
data <- data %>% 
  mutate_at(c("PESO.DESEMBARCADO.KILOGRAMOS",
              "PESO.VIVO.KILOGRAMOS",
              "VALOR.PESOS"), 
            remove_commas) %>%
  mutate_at(c("PESO.DESEMBARCADO.KILOGRAMOS",
              "PESO.VIVO.KILOGRAMOS",
              "VALOR.PESOS"), 
            as.numeric) %>%
  select(EJERCICIO.FISCAL, 
         CLAVE.DE.ENTIDAD,
         ENTIDAD.FEDERATIVA, 
         NOMBRE.PRINCIPAL,
         ORIGEN, 
         PESO.DESEMBARCADO.KILOGRAMOS, 
         PESO.VIVO.KILOGRAMOS,
         VALOR.PESOS) %>%
  group_by(EJERCICIO.FISCAL,
           CLAVE.DE.ENTIDAD,
           ENTIDAD.FEDERATIVA,
           NOMBRE.PRINCIPAL,
           ORIGEN) %>%
  summarise(PESO.VIVO.KILOGRAMOS = sum(PESO.VIVO.KILOGRAMOS, na.rm = TRUE),
            PESO.DESEMBARCADO.KILOGRAMOS = sum(PESO.DESEMBARCADO.KILOGRAMOS, na.rm = TRUE),
            VALOR.PESOS = sum(VALOR.PESOS, na.rm = TRUE))

colnames(data) <- gsub(".", "_", tolower(colnames(data)), fixed = TRUE)

write.csv(data, "datos/produccion_pesquera.csv", row.names = FALSE)
