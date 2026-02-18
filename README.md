# Plataforma - Registro Mexicano de Lupus (LupusRGMX)

Aplicación web interactiva desarrollada en R (Shiny) para la visualización de datos geoespaciales, análisis sociodemográfico, minería de texto e inferencia de riesgo clínico de pacientes en el Registro Mexicano de Lupus.

## Requisitos y Dependencias

El proyecto está construido en **R**. Para garantizar la reproducibilidad y el correcto funcionamiento de la aplicación, es necesario instalar las dependencias gráficas y estadísticas.

Puedes instalar todas las librerías necesarias ejecutando el script incluido:
```R
source("dependencies.R")
```
## Instalación y Uso
1. Clona este repositorio en tu máquina local:
```bash
git clone https://github.com/leomorgzzz/app-shiny-lupus.git
```
2. Abre tu entorno de R o RStudio y establece tu directorio de trabajo en la carpeta clonada.

3. Asegúrate de haber instalado las dependencias (paso anterior).

4. Ejecuta la aplicación abriendo el archivo principal y corriendo:
```R
shiny::runApp("App.R")
```
