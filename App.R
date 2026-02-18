#--------------------------------------------------------------------------------
# PROYECTO: Plataforma - Registro Mexicano de Lupus (LupusRGMX)
# MEJORAS: Dashboard Interactivo y Red Bayesiana
# AUTOR: Leonardo Morales Rodríguez
# -----------------------------------------------------------------------------

library(shiny)
library(bslib)
library(tidyverse)
library(tidytext)
library(dplyr)
library(ggplot2)
library(viridis) 
library(bnlearn)
library(gRain)
library(sf)
library(utils)
library(wordcloud2)
library(visNetwork)
library(leaflet) 
library(RColorBrewer)

#-------------------------------------------------------------------------------
# 0. INICIALIZACIÓN Y EXTRACCIÓN DE DATOS
#-------------------------------------------------------------------------------

# 0.1 Carga de mapa (Shapefile INEGI)
file_id_mapa <- "1onC2e_hRX9ZqOUBJhji15teRT1sWnDPH"  
url_mapa <- sprintf("https://drive.google.com/uc?export=download&id=%s", file_id_mapa)
temp_zip <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

download.file(url_mapa, destfile = temp_zip, mode = "wb", quiet = TRUE)
unzip(temp_zip, exdir = temp_dir)

shape_estados <- read_sf(dsn = temp_dir, layer = "00ent")
shape_estados <- st_simplify(shape_estados, dTolerance = 0.005, preserveTopology = TRUE)

regiones_lista <- list(
  "A. Región Norte" = c(2, 3, 5, 8, 10, 19, 25, 26, 28, 32),
  "B. Región Centro" = c(1, 6, 11, 13, 14, 15, 16, 18, 22, 24, 29),
  "C. Región Sur" = c(4, 7, 12, 17, 20, 21, 23, 27, 30, 31),
  "D. Ciudad de México" = 9
)

estados_mexico <- data.frame(
  state_code = 1:32,
  state_name = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                 "Coahuila", "Colima", "Chiapas", "Chihuahua", 
                 "Ciudad de México", "Durango", "Guanajuato", "Guerrero", 
                 "Hidalgo", "Jalisco", "México", "Michoacán", "Morelos", 
                 "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", 
                 "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                 "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"),
  stringsAsFactors = FALSE
)

mexico_sf_global <- shape_estados %>%
  mutate(cve_ent_num = as.numeric(CVE_ENT)) %>%
  left_join(estados_mexico, by = c("cve_ent_num" = "state_code")) %>%
  st_transform(crs = 4326) %>%
  mutate(
    region = case_when(
      cve_ent_num %in% regiones_lista[["A. Región Norte"]] ~ "A. Región Norte",
      cve_ent_num %in% regiones_lista[["B. Región Centro"]] ~ "B. Región Centro",
      cve_ent_num %in% regiones_lista[["C. Región Sur"]] ~ "C. Región Sur",
      cve_ent_num == 9 ~ "D. Ciudad de México",
      TRUE ~ "Otra"
    )
  )

# 0.2 Carga del modelo de Red Bayesianas
file_id_modelo <- "1Okx_7vl4fzbYcF74g25HLZaR7pdXB0At" 
url_modelo <- sprintf("https://drive.google.com/uc?export=download&id=%s", file_id_modelo)
temp_rds <- tempfile(fileext = ".rds")

download.file(url_modelo, destfile = temp_rds, mode = "wb", quiet = TRUE)
modelo_bn_global <- readRDS(temp_rds)
modelo_grain_global <- as.grain(modelo_bn_global)

#-------------------------------------------------------------------------------
# 1. DICCIONARIOS Y FUNCIONES DE TRANSFORMACIÓN
#------------------------------------------------------------------------------

diccionario_vars <- c(
  "Sexo" = "sexo", "Grupo Etario" = "grupo_etario", "Rango de Edad (Modelo)" = "age_group",
  "Estado de Residencia" = "estado", "Ocupación" = "ocupacion", "Proveedor de Salud" = "salud",
  "Escolaridad" = "escolaridad", "Diagnóstico de Lupus" = "lupus", "Nefritis" = "nephritis",
  "Ancestría Originaria" = "ancestria___2", "Tromboembolia Pulmonar" = "tep___1",
  "Enf. Arterial Periférica" = "enf_arterial___1", "Infarto" = "infarto___1",
  "Angina de Pecho" = "angina___1", "Insuf. Cardiaca" = "insuf_cardiaca___1",
  "Diabetes Gestacional" = "diabetes_gestacional___1", "Aborto" = "aborto___1",
  "Embarazo Ectópico" = "ectopico___1", "Obesidad" = "obesidad___1",
  "Sobrepeso" = "sobrepeso___1", "Ansiedad" = "ansiedad___1", "Depresión" = "depresion___1",
  "Hipertensión Arterial" = "hipertension", "Consumo de Alcohol" = "adic_alcohol___1",
  "Consumo de Tabaco" = "adic_tabaco___1", "Calidad de Vida" = "cal_totalcv_group",
  "Clasificación Alimentación" = "clasi_ali", "Comorbilidades" = "comorbidities",
  "Degeneración Macular" = "degeneracion_macular___1", "Actividad de la Enf. (SLEDAI)" = "Disease.activity.classification",
  "Hipertensión (Secundaria)" = "hipertension___1", "Nacimiento Prematuro" = "prematuro___1",
  "Daño Acumulado (SLICC)" = "SLICC.clasification", "Retraso Diagnóstico" = "dx_time_group",
  "Tiempo Síntomas-Diagnóstico" = "time_symptoms_group", "Dislexia" = "dislexia___1",
  "Tratamientos Diarios" = "daily_treatments___7"
)

mis_stopwords <- c(
  "el", "la", "los", "las", "un", "una", "unos", "unas", "y", "o", "de", "del", "al", "a", "en", "con", "sin", 
  "por", "para", "si", "no", "ni", "pero", "aunque", "mas", "mi", "mis", "tu", "tus", "su", "sus", "me", "te", 
  "se", "nos", "le", "les", "lo", "que", "cual", "quien", "donde", "cuando", "como", "tan", "muy", "mucho", 
  "mucha", "todo", "toda", "este", "esta", "esto", "ese", "esa", "eso", "es", "son", "fue", "era", "estaba", 
  "estoy", "tengo", "tiene", "haber", "hay", "habia", "ser", "estar", "ir", "voy", "van", "hacer", "hago", 
  "hace", "dia", "dias", "vez", "veces", "año", "años", "tiempo", "parte", "cosas", "algo", "nada", "pues", 
  "entonces", "luego", "asi", "bien", "mal", "ahi", "alli", "entre", "hacia", "hasta", "sobre", "tras", 
  "durante", "mediante", "contra", "desde", "lupus", "tener", "diagnostico", "enfermedad", "porque", "solo", 
  "sido", "gran", "puedo", "puede", "ella", "ello", "ellas", "nosotros", "usted", "cada", "mismo", "otro", 
  "otra", "otros", "otras", "fui", "yo", "ya", "ha", "he"
)

traducir_valores_internos <- function(val, variable_name = "") {
  val <- trimws(as.character(val))
  if (!grepl("age|etario|grupo", variable_name, ignore.case = TRUE) && val %in% c("0", "0.0", "No")) return("No / Ausente")
  if (!grepl("age|etario|grupo", variable_name, ignore.case = TRUE) && val %in% c("1", "1.0", "Si", "Yes", "Sí")) return("Sí / Presente")
  if (!grepl("age|etario|grupo", variable_name, ignore.case = TRUE) && val == "2") return("No / Ausente")
  
  res <- case_when(
    val == "No damage" ~ "Sin daño", 
    val == "Mild damage" ~ "Daño leve", 
    val == "Moderate damage" ~ "Daño moderado", 
    val %in% c("High damage", "Severe damage") ~ "Daño severo",
    val == "No activity" ~ "Sin actividad", 
    val == "Mild activity" ~ "Actividad leve", 
    val == "Moderate activity" ~ "Actividad moderada", 
    val == "High activity" ~ "Alta actividad", 
    val == "Very high activity" ~ "Actividad muy alta",
    val == "low" ~ "Baja", 
    val %in% c("mean", "medium") ~ "Promedio", 
    val == "high" ~ "Alta",
    val == "within 2 years" ~ "< 2 años", 
    val == "2 to 5 years" ~ "2 a 5 años", 
    val == "more than 5 years" ~ "> 5 años", 
    val == "less than 1 year" ~ "< 1 año", 
    val == "1 to 2 years" ~ "1 a 2 años", 
    val == "6 to 10 years" ~ "6 a 10 años",
    val == "more than 10 years" ~ "> 10 años", 
    variable_name == "age_group" & val == "1" ~ "18-25 años", 
    variable_name == "age_group" & val == "2" ~ "26-31 años", 
    variable_name == "age_group" & val == "3" ~ "32-45 años", 
    variable_name == "age_group" & val == "4" ~ "46-50 años", 
    variable_name == "age_group" & val == "5" ~ "51-60 años", 
    variable_name == "age_group" & val == "6" ~ "Más de 60 años", 
    variable_name == "age_group" & val == "9" ~ "No especificado",
    TRUE ~ val
  )
  return(res)
}

ordenar_niveles <- function(niveles) {
  orden_logico <- c("No / Ausente", "Sin daño", "Sin actividad", "Baja", "Sí / Presente", 
                    "Daño leve", "Actividad leve", "Promedio", "Daño moderado", 
                    "Actividad moderada", "Alta", "Daño severo", "Alta actividad", 
                    "Actividad muy alta")
  existentes <- intersect(orden_logico, niveles)
  resto <- setdiff(niveles, orden_logico)
  return(c(existentes, resto))
}

#-------------------------------------------------------------------------------
# 2. UI
#--------------------------------------------------------------------------------

mi_tema <- bs_theme(
  version = 5,
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat"),
  primary = "#7B157A", 
  secondary = "#5e105e"
)

css_personalizado <- "
  body { background-color: #f2f2f2; }
  
  /* Configuraciones de navegación */
  .navbar { min-height: 80px; padding: 0 1.5rem; box-shadow: 0 2px 8px rgba(0,0,0,0.08); display: flex; align-items: center; }
  .navbar-brand { padding: 0; margin-right: 30px; display: flex; align-items: center; }
  .navbar-brand img { height: 55px; width: auto; object-fit: contain; }
  .navbar-nav { display: flex; align-items: center; gap: 0; }
  .nav-link { font-size: 1.05rem; font-weight: 500; padding: 8px 12px !important; margin: 0 2px; border-radius: 6px; transition: all 0.2s ease-in-out; }
  .nav-link:hover { background-color: rgba(123, 21, 122, 0.08); color: #7B157A !important; }
  .nav-link.active { font-weight: 700; color: #7B157A !important; }
  
  /* Banner Principal */
  .hero-banner {
    background-image: url('banner_lupus.png'); 
    background-size: 100% 100%; 
    background-position: center;
    background-repeat: no-repeat;
    height: 300px; 
    display: flex;
    align-items: center;
    color: white; 
    padding: 0 3rem; 
    margin-bottom: 2rem;
    box-shadow: 0 4px 6px rgba(0,0,0,0.15);
  }
  .hero-title { font-weight: 700; font-size: 2.8rem; text-shadow: 0 2px 4px rgba(0,0,0,0.5); }
  .hero-subtitle { font-size: 1.3rem; opacity: 0.95; margin-bottom: 1.5rem; text-shadow: 0 1px 3px rgba(0,0,0,0.5); }
  
  /* Componentes Visuales */
  .card { border: none; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.08); background-color: white; margin-bottom: 20px; }
  .card:hover { box-shadow: 0 8px 15px rgba(0,0,0,0.12); transform: translateY(-3px); transition: all 0.3s ease; }
  .card-header { background-color: white; border-bottom: 1px solid #eee; font-weight: bold; color: #444; padding: 1rem; }
  .purple-text { color: #7B157A; font-weight: 700; }
  .js-plotly-plot .plotly .modebar { display: none !important; }
  .counter-box { padding: 15px; border-radius: 8px; text-align: center; margin-bottom: 15px; font-weight: bold; color: white; font-size: 1.2em; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }
  .leaflet-container { height: 100%; width: 100%; }
"

ui <- page_navbar(
  title = div(class = "d-flex align-items-center", 
              img(src = "logo_lupus.png", style = "max-height: 55px; margin-right: 10px;")), 
  theme = mi_tema,
  header = tags$head(tags$style(HTML(css_personalizado))),
  id = "nav",
  window_title = "Lupus RGMX",
  
  # --- Pestaña: Visión General ---
  nav_panel(title = "Visión General", icon = icon("house"),
            div(class = "hero-banner",
                div(class = "container-fluid",
                    div(style = "max-width: 60%;", 
                        h1("Registro Mexicano de Lupus", class = "hero-title"),
                        p("Plataforma de inteligencia de datos para pacientes, médicos e investigadores.", class = "hero-subtitle"),
                        tags$a(href = "https://lupusrgmx.liigh.unam.mx/quienes-somos.html", 
                               target = "_blank", 
                               class = "btn btn-light text-primary fw-bold", 
                               icon("info-circle"), " Sobre el proyecto")
                    )
                )
            ),
            div(class = "container-fluid",
                layout_column_wrap(width = "18rem", heights_equal = "row",
                                   card(card_header(icon("map-location-dot"), " Análisis Espacial"), 
                                        card_body(p("Visualización de la distribución geográfica de los pacientes registrados en el sistema. Permite observar la concentración de casos por ", span("entidad federativa", class="purple-text"), " y región sanitaria.")), 
                                        card_footer(actionButton("go_mapa", "Ver Mapa", class = "btn-primary w-100"))),
                                   card(card_header(icon("chart-pie"), " Perfil Sociodemográfico"), 
                                        card_body(p("Herramientas visuales para analizar el perfil de la cohorte. Incluye comparativas de variables como ", span("grupos de edad, ocupación y proveedor de salud", class="purple-text"), ".")), 
                                        card_footer(actionButton("go_stats", "Ver Gráficos", class = "btn-primary w-100"))),
                                   card(card_header(icon("comments"), " Minería de Texto"), 
                                        card_body(p("Análisis exploratorio de texto basado en las experiencias compartidas por los pacientes. Identifica ", span("términos frecuentes y temáticas", class="purple-text"), " en los testimonios.")), 
                                        card_footer(actionButton("go_word", "Ver Análisis Narrativo", class = "btn-primary w-100")))
                ),
                br(), 
                layout_column_wrap(width = "25rem", heights_equal = "row",
                                   card(card_header(icon("user-doctor"), " Modelo Predictivo (Experimental)"), 
                                        card_body(p("Estimación estadística experimental basada en ", span("Algoritmos de Aprendizaje Automatizado", class="purple-text"), " (Redes Bayesianas)."), 
                                                  p("Calcula probabilidades condicionales basadas en factores causales.", style = "font-size: 0.9rem; color: #666;"), 
                                                  p(strong("Nota legal:"), " Herramienta de apoyo a la investigación. Estrictamente no sustituye el juicio ni diagnóstico clínico.", style = "font-size: 0.85rem; color: #888;")), 
                                        card_footer(actionButton("go_calc", "Ingresar al Modelo", class = "btn-primary w-100"))),
                                   card(card_header(icon("laptop"), " Portal Institucional"), 
                                        card_body(p("Acceso al portal oficial del ", span("Lupus RGMX", class="purple-text"), "."), 
                                                  p("Recursos educativos, noticias institucionales, publicaciones científicas y vías de contacto para vinculación.")), 
                                        card_footer(tags$a(href = "https://lupusrgmx.liigh.unam.mx/index.html", target = "_blank", class = "btn btn-primary w-100", icon("external-link-alt"), " Visitar Sitio Web")))
                )
            )
  ),
  
  # --- Pestaña: Módulo Espacial ---
  nav_panel(title = "Análisis Espacial", value = "tab_mapa", icon = icon("map"),
            layout_sidebar(
              sidebar = sidebar(width = 300, 
                                title = "Filtros Geográficos", 
                                selectInput("region", "Región Sanitaria:", choices = c("Nacional", "A. Región Norte", "B. Región Centro", "C. Región Sur", "D. Ciudad de México"), selected = "Nacional"), 
                                hr(), 
                                h6("Desglose por Entidad"), 
                                div(style = "max-height: 500px; overflow-y: auto;", tableOutput("resumen_tabla_detalle"))),
              card(full_screen = TRUE, card_header("Distribución Espacial de la Cohorte"), leafletOutput("mapa_interactivo", height = "650px"))
            )
  ),
  
  # --- Pestaña: Módulo Demográfico ---
  nav_panel(title = "Perfil Sociodemográfico", value = "tab_stats", icon = icon("chart-column"),
            layout_sidebar(
              sidebar = sidebar(title = "Parámetros de Visualización", 
                                selectInput("x_var_2", "Variable Eje X:", choices = NULL), 
                                selectInput("fill_var_2", "Variable de Agrupación (Color):", choices = NULL)),
              card(card_header("Análisis Comparativo"), plotOutput("barplot_2", height = "600px"))
            )
  ),
  
  # --- Pestaña: Minería de Textos ---
  nav_panel(title = "Análisis Narrativo", value = "tab_word", icon = icon("comments"),
            layout_sidebar(
              sidebar = sidebar(title = "Filtros de Cohorte", 
                                uiOutput("record_counter_ui"), 
                                selectizeInput("flt_home", "Entidad de Residencia:", choices = NULL, multiple = TRUE, options = list(placeholder = "Todas las entidades...")), 
                                sliderInput("flt_age", "Rango de Edad:", min = 0, max = 100, value = c(0, 100)), 
                                selectInput("flt_sex", "Sexo Asignado:", choices = c("Todos", "Mujer", "Hombre"), selected = "Todos"), 
                                hr(), 
                                actionButton("generar", "Procesar Análisis Textual", class = "btn-primary w-100", icon = icon("rotate"))),
              card(card_header("Frecuencia de Términos Clínicos y Emocionales"), wordcloud2Output("wordcloud", height = "700px"))
            )
  ),
  
  # --- Pestaña: Modelo Estadístico ---
  nav_panel(title = "Inferencia Clínica", value = "tab_calc", icon = icon("user-doctor"),
            layout_sidebar(
              sidebar = sidebar(width = 400, 
                                title = "Configuración del Perfil", 
                                selectInput("variable_objetivo", "Condición a Evaluar (Target):", choices = NULL), 
                                p(strong("Evidencia Causal (Variables Independientes):"), style = "margin-bottom: 5px; font-size: 0.9rem; color: #7B157A;"), 
                                p(em("El modelo condiciona sus entradas estrictamente a los nodos dependientes directos de la variable objetivo."), style = "font-size: 0.8rem; color: #666; margin-bottom: 10px;"), 
                                div(style = "max-height: 400px; overflow-y: auto; background: #fff; border: 1px solid #ddd; padding: 10px; border-radius: 5px; margin-bottom: 15px;", checkboxGroupInput("vars_evidencia", label = NULL, choices = NULL)), 
                                uiOutput("controles_evidencia"), 
                                hr(), 
                                actionButton("btn_inferir", "Calcular Probabilidad", icon = icon("calculator"), class = "btn-success w-100 mb-2"), 
                                actionButton("btn_ver_dag", "Inspeccionar Topología de Red", icon = icon("project-diagram"), class = "btn-info w-100")),
              card(card_header("Probabilidad Condicional Estimada"), 
                   div(style = "min-height: 400px; display: flex; align-items: center; justify-content: center;", plotOutput("grafico_inferencia", height = "400px", width = "100%")), 
                   card_footer(strong("Nota legal:"), " Herramienta estadística de apoyo a la investigación. No tiene validez de diagnóstico médico."))
            )
  ),
  
  # --- Pestaña: Formularios Externos ---
  nav_menu(title = "Vinculación", icon = icon("bars"),
           nav_panel("Solicitud de Datos (Investigadores)", card(tags$iframe(src = "https://redcap.link/nqsxtj8n", width = "100%", height = "700px", style = "border:none;"))),
           nav_item(tags$a(icon("github"), " Repositorio de Código", href = "https://github.com/leomorgzzz/app-shiny-lupus", target = "_blank"))
  )
)

#--------------------------------------------------------------------------------
# 3. SERVER
#-------------------------------------------------------------------------------.

server <- function(input, output, session) {
  
  # Rutamiento interno de los botones del banner
  observeEvent(input$go_mapa, { nav_select("nav", "tab_mapa") })
  observeEvent(input$go_stats, { nav_select("nav", "tab_stats") })
  observeEvent(input$go_calc, { nav_select("nav", "tab_calc") })
  observeEvent(input$go_word, { nav_select("nav", "tab_word") })
  
  # 3.1 Extracción de Base de Datos Principal
  Datos <- reactive({ 
    file_id_datos <- "1y-PHjo3fjeX1B_jEOYwhu4I2Rt1NsLdE" 
    url <- sprintf("https://drive.google.com/uc?export=download&id=%s", file_id_datos)
    tryCatch({
      read.csv(url, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    }, error = function(e) {
      warning("Error en conexión a API de Drive: ", e$message)
      return(data.frame())
    })
  })
  
  # 3.2 Pre-procesamiento de datos sociodemográficos
  Datos_2 <- reactive({
    req(Datos())
    df <- Datos()
    df %>%
      rename(sexo = sex___1, grupo_etario = age_group, estado = home, ocupacion = ocupation, salud = health_provider, escolaridad = school) %>%
      mutate(
        sexo = recode_factor(as.character(sexo), "1"="Mujer", "0"="Hombre"),
        lupus = recode_factor(as.character(lupus), "1"="Sí", "2"="No"),
        grupo_etario = recode_factor(as.character(grupo_etario), "1"="18-25", "2"="26-31", "3"="32-45", "4"="46-50", "5"="51-60", "6"=">60", "9"="No especificado"),
        ocupacion = recode_factor(as.character(ocupacion), "1"="Estudiante", "2"="Empleada", "3"="Desempleada", "4"="Jubilada"),
        salud = recode_factor(as.character(salud), "1"="IMSS", "2"="ISSSTE", "3"="PEMEX", "4"="SEDENA", "5"="SEMAR", "6"="SSA", "7"="Estatal", "8"="IMSS-Bienestar", "9"="Otros / Desconocido"), 
        escolaridad = recode_factor(as.character(escolaridad), "1"="Ninguna", "2"="Primaria", "3"="Secundaria", "4"="Prepa/Técnica", "5"="Licenciatura", "6"="Posgrado")
      )
  })
  
  # 3.3 Motor Espacial (Leaflet)
  datos_mapa_proc <- reactive({
    req(Datos())
    conteo <- Datos() %>% mutate(codigo = case_when(home==9 ~ 10, home==10 ~ 9, TRUE ~ home)) %>% count(codigo, name="casos")
    mexico_sf_global %>% left_join(conteo, by = c("cve_ent_num" = "codigo")) %>% mutate(casos = replace_na(casos, 0))
  })
  
  output$mapa_interactivo <- renderLeaflet({
    d <- datos_mapa_proc()
    if(input$region != "Nacional") d <- d %>% filter(region == input$region)
    pal <- colorNumeric(palette = colorRampPalette(c("#BA68C8", "#4A148C"))(10), domain = d$casos)
    leaflet(d) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(fillColor = ~pal(casos), weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7, 
                  highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE), 
                  label = ~paste0(state_name, ": ", casos, " casos"), 
                  popup = ~paste0("<strong>Entidad: </strong>", state_name, "<br><strong>Región: </strong>", region, "<br><strong>Casos Totales: </strong>", casos)) %>% 
      addLegend(pal = pal, values = ~casos, opacity = 0.7, title = "Pacientes Registrados", position = "bottomright")
  })
  
  output$resumen_tabla_detalle <- renderTable({
    d <- datos_mapa_proc() %>% st_drop_geometry()
    if(input$region != "Nacional") d <- d %>% filter(region == input$region)
    d %>% select(Entidad = state_name, Casos = casos) %>% arrange(desc(Casos))
  }, striped=TRUE, hover=TRUE, width="100%")
  
  # 3.4 Motor de Gráficos (ggplot2)
  observe({
    req(Datos_2())
    cols_df <- names(Datos_2())
    diccionario_filtrado <- diccionario_vars[diccionario_vars %in% cols_df]
    vars_clean <- diccionario_filtrado[!grepl("___[0-9]|nephritis|hipertension$", diccionario_filtrado)]
    list_opts <- c("Sexo"="sexo", "Diagnóstico de Lupus"="lupus", "Grupo Etario"="grupo_etario", "Ocupación"="ocupacion", "Nivel de Escolaridad"="escolaridad", "Proveedor de Salud"="salud")
    updateSelectInput(session, "x_var_2", choices = list_opts, selected = "sexo")
    updateSelectInput(session, "fill_var_2", choices = list_opts, selected = "lupus")
  })
  
  output$barplot_2 <- renderPlot({
    req(input$x_var_2, input$fill_var_2, Datos_2())
    df <- Datos_2() %>% 
      filter(!is.na(.data[[input$x_var_2]]), .data[[input$x_var_2]] != "NA") %>% 
      filter(!is.na(.data[[input$fill_var_2]]), .data[[input$fill_var_2]] != "NA")
    
    lbl_x <- names(which(sapply(c("Sexo"="sexo", "Diagnóstico de Lupus"="lupus", "Grupo Etario"="grupo_etario", "Ocupación"="ocupacion", "Nivel de Escolaridad"="escolaridad", "Proveedor de Salud"="salud"), function(x) x == input$x_var_2)))
    if(length(lbl_x)==0) lbl_x <- input$x_var_2
    
    ggplot(df, aes_string(x = input$x_var_2, fill = input$fill_var_2)) + 
      geom_bar(position = "dodge", alpha = 0.95) + 
      scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.85) + 
      theme_minimal(base_size = 14) + 
      labs(x = lbl_x, y = "Frecuencia Absoluta (Pacientes)", fill = "") + 
      theme(axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"), legend.position = "bottom")
  })
  
  # 3.5 Minería de Texto y Procesamiento de Lenguaje Natural
  estados_lista <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")
  updateSelectizeInput(session, "flt_home", choices = estados_lista, server = TRUE)
  
  filtered_wc <- reactive({
    req(Datos())
    df <- Datos()
    if (!is.null(input$flt_home)) { idx <- which(estados_lista %in% input$flt_home); df <- df %>% filter(home %in% idx) }
    if (input$flt_sex != "Todos") { v <- ifelse(input$flt_sex == "Mujer", 1, 0); df <- df %>% filter(sex___1 == v) }
    df %>% filter((is.na(calculated_age) | between(calculated_age, input$flt_age[1], input$flt_age[2])))
  })
  
  output$record_counter_ui <- renderUI({
    n <- nrow(filtered_wc())
    col <- if(n < 10) "#E74C3C" else "#27AE60"
    div(class = "counter-box", style = paste0("background-color:", col, ";"), paste("Total de testimonios válidos:", n))
  })
  
  output$wordcloud <- renderWordcloud2({
    req(input$generar)
    isolate({
      txt <- paste(na.omit(filtered_wc()$inventariopregunta1), collapse=" ")
      if(nchar(txt) < 5) return(NULL)
      tokens <- tibble(text = txt) %>% 
        unnest_tokens(word, text) %>% 
        filter(!word %in% mis_stopwords) %>% 
        anti_join(stop_words, by="word") %>% 
        count(word, sort=TRUE) %>% head(150)
      wordcloud2(tokens, size = 1.0, color = rep_len(c("#7B157A", "#9C449C", "#5e105e"), nrow(tokens)), backgroundColor = "white")
    })
  })
  
  # 3.6 Motor de Inferencia (Redes Bayesianas - bnlearn/gRain)
  observe({
    bn <- modelo_bn_global
    vars_modelo <- names(bn)
    targets_validos <- sapply(vars_modelo, function(nodo) {
      padres <- bnlearn::parents(bn, nodo)
      padres_utiles <- padres[padres != "Socioeconomic.level"] 
      length(padres_utiles) >= 2 
    })
    vars_finales <- vars_modelo[targets_validos]
    opts <- vars_finales
    names(opts) <- sapply(vars_finales, function(x) { if(x %in% diccionario_vars) return(names(diccionario_vars)[which(diccionario_vars == x)]); return(x) })
    opts <- opts[order(names(opts))]
    sel <- if("nephritis" %in% opts) "nephritis" else opts[1]
    updateSelectInput(session, "variable_objetivo", choices = opts, selected = sel)
  })
  
  observeEvent(input$variable_objetivo, {
    req(input$variable_objetivo) 
    bn <- modelo_bn_global
    if(input$variable_objetivo %in% names(bn)) {
      vars_relevantes <- bnlearn::parents(bn, input$variable_objetivo)
      vars_relevantes <- vars_relevantes[vars_relevantes != "Socioeconomic.level"] 
      opts <- vars_relevantes
      names(opts) <- sapply(vars_relevantes, function(x) { if(x %in% diccionario_vars) return(names(diccionario_vars)[which(diccionario_vars == x)]); return(x) })
      opts <- opts[order(names(opts))]
      updateCheckboxGroupInput(session, "vars_evidencia", choices = opts)
    }
  })
  
  output$controles_evidencia <- renderUI({
    req(input$vars_evidencia)
    bn <- modelo_bn_global
    lapply(input$vars_evidencia, function(var_raw) {
      niveles <- dimnames(bn[[var_raw]]$prob)[[1]]
      names_niveles <- sapply(niveles, function(val) traducir_valores_internos(val, var_raw))
      opciones_ordenadas <- setNames(niveles, names_niveles)
      nombres_ordenados <- ordenar_niveles(names_niveles)
      opciones_finales <- opciones_ordenadas[match(nombres_ordenados, names(opciones_ordenadas))]
      faltantes <- setdiff(names(opciones_ordenadas), nombres_ordenados)
      opciones_finales <- c(opciones_finales, opciones_ordenadas[faltantes])
      nombre_bonito <- var_raw
      if(var_raw %in% diccionario_vars) nombre_bonito <- names(diccionario_vars)[which(diccionario_vars == var_raw)]
      selectInput(paste0("ev_", var_raw), paste("Valor observado para:", nombre_bonito), choices = opciones_finales)
    })
  })
  
  calculo_probs <- eventReactive(input$btn_inferir, {
    req(input$variable_objetivo, input$vars_evidencia)
    evidencia <- sapply(input$vars_evidencia, function(var) input[[paste0("ev_", var)]])
    names(evidencia) <- input$vars_evidencia
    modelo_ev <- setEvidence(modelo_grain_global, nodes = names(evidencia), states = as.character(evidencia))
    querygrain(modelo_ev, nodes = input$variable_objetivo, type = "conditional")
  })
  
  output$grafico_inferencia <- renderPlot({
    req(calculo_probs())
    res <- calculo_probs()
    if(is.list(res)) { datos <- res[[which(!sapply(res, is.null))[1]]] } else { datos <- res }
    if(!is.atomic(datos)) datos <- as.numeric(datos)
    probs <- as.numeric(datos)
    etiquetas_raw <- if(!is.null(names(datos))) names(datos) else if(!is.null(dimnames(datos))) dimnames(datos)[[1]] else as.character(1:length(probs))
    etiquetas_finales <- sapply(etiquetas_raw, function(val) traducir_valores_internos(val, input$variable_objetivo))
    df <- data.frame(Etiqueta = etiquetas_finales, Probabilidad = probs) %>% 
      mutate(Color = ifelse(grepl("Sí|Presente|Alta|Severo|High", Etiqueta, ignore.case=T), "#7B157A", "#512DA8"))
    
    titulo <- input$variable_objetivo
    if(titulo %in% diccionario_vars) titulo <- names(diccionario_vars)[which(diccionario_vars == titulo)]
    
    ggplot(df, aes(x = Etiqueta, y = Probabilidad, fill = Color)) + 
      geom_col(width = 0.5) + 
      geom_text(aes(label = scales::percent(Probabilidad, accuracy = 0.1)), vjust = -0.5, size = 6, fontface="bold") + 
      scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) + 
      scale_fill_identity() + 
      theme_minimal() + 
      labs(title = paste("Probabilidad Condicional Estimada:", titulo), x="", y="") + 
      theme(plot.title = element_text(size=18, hjust=0.5, face="bold"), axis.text.x = element_text(size=14, face="bold"))
  })
  
  observeEvent(input$btn_ver_dag, { 
    showModal(modalDialog(title = "Topología de la Red Bayesiana", visNetworkOutput("plot_dag_full", height = "600px"), size = "l", easyClose = TRUE)) 
  })
  
  output$plot_dag_full <- renderVisNetwork({
    bn <- modelo_bn_global
    target <- input$variable_objetivo
    nodes <- data.frame(id = names(bn), label = names(bn), title = names(bn), shape = "ellipse")
    nodes$label <- sapply(nodes$id, function(x) { if(x %in% diccionario_vars) return(names(diccionario_vars)[which(diccionario_vars == x)]); return(x) })
    nodes$color <- "lightblue"; nodes$color[nodes$id == target] <- "#FF8C00"; nodes$value <- 20; nodes$value[nodes$id == target] <- 40
    edges <- bnlearn::arcs(bn); edges <- data.frame(from = edges[,1], to = edges[,2])
    visNetwork(nodes, edges) %>% 
      visEdges(arrows = "to", color = list(color = "#ccc", highlight = "black")) %>% 
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = list(enabled = TRUE, selected = target)) %>% 
      visPhysics(stabilization = TRUE)
  })
}

shinyApp(ui, server)
