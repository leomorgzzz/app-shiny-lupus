# Lista de paquetes requeridos en CRAN
paquetes_cran <- c(
  "shiny", "bslib", "tidyverse", "tidytext", "dplyr", "ggplot2", 
  "viridis", "bnlearn", "sf", "wordcloud2", "visNetwork", 
  "leaflet", "RColorBrewer"
)

# Función para instalar paquetes faltantes de CRAN
instalar_faltantes <- function(paquetes) {
  nuevos_paquetes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
  if(length(nuevos_paquetes)) {
    message("Instalando paquetes faltantes: ", paste(nuevos_paquetes, collapse = ", "))
    install.packages(nuevos_paquetes, dependencies = TRUE)
  } else {
    message("Todos los paquetes de CRAN ya están instalados.")
  }
}

instalar_faltantes(paquetes_cran)

# Nota: El paquete 'gRain' a veces requiere BiocManager dependiendo de la versión de R
if (!requireNamespace("gRain", quietly = TRUE)) {
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install("gRain")
}

message("¡Dependencias instaladas con éxito!")