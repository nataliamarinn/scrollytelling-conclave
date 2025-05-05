### LIBRERÍAS UTILIZADAS ####

library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(scales)

#### LECTURA DE ARCHIVOS ####

archivo <- "src/conclaves.xlsx"
hojas <- excel_sheets(archivo)
datos <- lapply(hojas, function(hoja) {
  read_excel(archivo, sheet = hoja)
}) %>% bind_rows()

glimpse(datos)


#### Cantidad de cardenales por cónclave #####

# Elegí serie de tiempo también para tener información de qué tan cercanos fueron 
# los cónclaves en sentido temporal


conteo <- datos %>%
  count(Año, name = "Cantidad") %>%
  arrange(Año)


grafico1 <- ggplot(conteo, aes(x = Año, y = Cantidad)) +
  geom_line(color = "#FFD700", size = 1.2) +
  geom_point(size = 3, color = "#FFD700") +
  geom_text_repel(
    aes(label = Cantidad),
    size = 4.5,
    nudge_y = 5,
    max.overlaps = Inf,
    color = "#FFD700"
  ) +
  scale_x_continuous(breaks = conteo$Año) +
  labs(
    title = "Evolución temporal de cardenales por cónclave",
    x = "Año del cónclave",
    y = "Cantidad de cardenales"
  ) +
  theme_minimal(base_family = "Roca Two", base_size = 13) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "#fdf0d5", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#fdf0d5"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title = element_text(color = "#fdf0d5", face = "bold"),
    plot.title = element_text(color = "#fdf0d5", face = "bold", size = 16)
  )

plotly_grafico1 <- ggplotly(grafico1) %>%
  layout(
    paper_bgcolor = 'rgba(0,0,0,0)',
    plot_bgcolor = 'rgba(0,0,0,0)',
    font = list(color = '#fdf0d5'),
    xaxis = list(titlefont = list(color = '#fdf0d5'), tickfont = list(color = '#fdf0d5')),
    yaxis = list(titlefont = list(color = '#fdf0d5'), tickfont = list(color = '#fdf0d5'))
  )

#### Diversidad de los cónclaves a través del tiempo #####
proporciones <- datos %>%
  group_by(Año, Continente) %>%
  summarise(Cardinales = n(), .groups = "drop") %>%
  group_by(Año) %>%
  mutate(Proporcion = Cardinales / sum(Cardinales))

grafico2<-ggplot(proporciones, aes(x = factor(Año), y = Proporcion, fill = Continente)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Evolución de la representación continental",
    x = "Cónclave (año)",
    y = "Participación relativa (%)",
    fill = "Continente"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



# Crear proporciones con columna formateada como texto
proporciones_plotly <- datos %>%
  group_by(Año, Continente) %>%
  summarise(Cardinales = n(), .groups = "drop") %>%
  group_by(Año) %>%
  mutate(
    Proporcion = Cardinales / sum(Cardinales),
    Prop_texto = percent(Proporcion, accuracy = 0.1)  # 1 decimal
  )
# Paso 1: Definir paleta manual
colores_pastel <- c(
  "Norteamérica" = "#bd6809",
  "Europa" = "#548558",
  "África" = "#77722E",
  "Asia" = "#a4485e",
  "Oceanía" = "#283142",
  "Latinoamérica"="#5c95e0"
)

# Paso 2: Crear gráfico ggplot base
ggplot_obj <- ggplot(proporciones_plotly, aes(
  x = factor(Año),
  y = Proporcion,
  fill = Continente,
  text = paste0("Continente: ", Continente,
                "<br>Participación: ", Prop_texto,
                "<br>Año: ", Año)
)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colores_pastel) +
  labs(
    title = "Evolución de la representación continental",
    x = "Cónclave (año)",
    y = "Participación relativa (%)",
    fill = ""
  ) +
  theme_minimal(base_family = "Roca Two", base_size = 13) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "#fdf0d5"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#fdf0d5"),
    axis.title = element_text(color = "#fdf0d5", face = "bold"),
    plot.title = element_text(color = "#fdf0d5", face = "bold")
  )

# Paso 3: Convertir a plotly con fondo transparente y ejes crema
plotly_grafico <- ggplotly(ggplot_obj, tooltip = "text") %>%
  layout(
    paper_bgcolor = 'rgba(0,0,0,0)',
    plot_bgcolor = 'rgba(0,0,0,0)',
    font = list(color = '#fdf0d5'),
    xaxis = list(
      tickfont = list(color = '#fdf0d5'),
      titlefont = list(color = '#fdf0d5')
    ),
    yaxis = list(
      tickfont = list(color = '#fdf0d5'),
      titlefont = list(color = '#fdf0d5')
    ),
    legend = list(
      font = list(color = '#fdf0d5')
    )
  )

# Mostrar
plotly_grafico


##### ¿Qué edad tienen los electores?######


grafico3 <- ggplot(datos, aes(x = factor(Año), y = Edad)) +
  geom_boxplot(
    fill = "#FFD700",     # amarillo dorado
    outlier.shape = NA,
    width = 0.5,
    color = "#FFD5A9"      # borde del boxplot opcional
  ) +
  geom_jitter(
    width = 0.2,
    alpha = 0.4,
    color = "#a4485e"      # mostaza opaco
  ) +
  labs(
    title = "¿A qué edad se elige al Papa?",
    subtitle = "Distribución de edades de los cardenales electores por cónclave",
    x = "Año del cónclave",
    y = "Edad (años)"
  ) +
  theme_minimal(base_family = "Roca Two", base_size = 13) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "#fdf0d5", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#fdf0d5"),
    axis.title = element_text(color = "#fdf0d5", face = "bold"),
    plot.title = element_text(color = "#fdf0d5", face = "bold", size = 16),
    plot.subtitle = element_text(color = "#fdf0d5")
  )

ggsave("grafico3.png", plot = grafico3, width = 8, height = 5, dpi = 300, bg = "transparent")


### Países ####

datos <- datos %>%
  mutate(
    country_en = case_when(
      País %in% c("Italia") ~ "Italy",
      País %in% c("Francia") ~ "France",
      País %in% c("España") ~ "Spain",
      País %in% c("Alemania") ~ "Germany",
      País %in% c("Estados Unidos") ~ "United States",
      País %in% c("Brasil", "Brazil") ~ "Brazil",
      País %in% c("Argentina") ~ "Argentina",
      País %in% c("Polonia", "Poland") ~ "Poland",
      País %in% c("México") ~ "Mexico",
      País %in% c("Chile") ~ "Chile",
      País %in% c("Portugal") ~ "Portugal",
      País %in% c("Colombia") ~ "Colombia",
      País %in% c("Filipinas") ~ "Philippines",
      País %in% c("Canadá") ~ "Canada",
      País %in% c("Inglaterra", "Gran Bretaña", "Reino Unido", "Escocia") ~ "United Kingdom",
      País %in% c("India") ~ "India",
      País %in% c("Vietnam") ~ "Vietnam",
      País %in% c("Bélgica") ~ "Belgium",
      País %in% c("Holanda", "Países Bajos") ~ "Netherlands",
      País %in% c("Suiza") ~ "Switzerland",
      País %in% c("Austria") ~ "Austria",
      País %in% c("Croacia") ~ "Croatia",
      País %in% c("República Checa") ~ "Czech Republic",
      País %in% c("Hungría", "Hugría") ~ "Hungary",
      País %in% c("Suecia") ~ "Sweden",
      País %in% c("Noruega") ~ "Norway",
      País %in% c("Irlanda") ~ "Ireland",
      País %in% c("Malta") ~ "Malta",
      País %in% c("Letonia") ~ "Latvia",
      País %in% c("Lituania") ~ "Lithuania",
      País %in% c("Eslovenia") ~ "Slovenia",
      País %in% c("Ucrania") ~ "Ukraine",
      País %in% c("Rusia") ~ "Russia",
      País %in% c("Nigeria") ~ "Nigeria",
      País %in% c("Sudáfrica") ~ "South Africa",
      País %in% c("Camerún") ~ "Cameroon",
      País %in% c("Etiopía") ~ "Ethiopia",
      País %in% c("Uganda") ~ "Uganda",
      País %in% c("Ghana") ~ "Ghana",
      País %in% c("Congo", "Rep. Dem. del Congo", "República Democrática del Congo") ~ "Democratic Republic of the Congo",
      País %in% c("República de África Central") ~ "Central African Republic",
      País %in% c("Senegal") ~ "Senegal",
      País %in% c("Burkina Faso") ~ "Burkina Faso",
      País %in% c("Ruanda") ~ "Rwanda",
      País %in% c("Sudán") ~ "Sudan",
      País %in% c("Sudan del Sur") ~ "South Sudan",
      País %in% c("Mozambique") ~ "Mozambique",
      País %in% c("Benin") ~ "Benin",
      País %in% c("Tanzania") ~ "Tanzania",
      País %in% c("Madagascar", "Madagscar") ~ "Madagascar",
      País %in% c("Costa de Marfil") ~ "Ivory Coast",
      País %in% c("Marruecos") ~ "Morocco",
      País %in% c("Argelia", "Algeria") ~ "Algeria",
      País %in% c("Egipto") ~ "Egypt",
      País %in% c("Armenia") ~ "Armenia",
      País %in% c("China") ~ "China",
      País %in% c("Japón") ~ "Japan",
      País %in% c("Corea del Sur") ~ "South Korea",
      País %in% c("Indonesia") ~ "Indonesia",
      País %in% c("Tailandia") ~ "Thailand",
      País %in% c("Irán") ~ "Iran",
      País %in% c("Iraq") ~ "Iraq",
      País %in% c("Siria") ~ "Syria",
      País %in% c("Sri Lanka") ~ "Sri Lanka",
      País %in% c("Líbano", "Lebanon") ~ "Lebanon",
      País %in% c("Pakistán") ~ "Pakistan",
      País %in% c("Palestina") ~ "Palestine",
      País %in% c("Australia") ~ "Australia",
      País %in% c("Nueva Zelanda") ~ "New Zealand",
      País %in% c("Fiyi") ~ "Fiji",
      País %in% c("Tonga") ~ "Tonga",
      País %in% c("Samoa") ~ "Samoa",
      País %in% c("Timor-Leste") ~ "Timor-Leste",
      País %in% c("Papúa Nueva Guinea", "Nueva Guinea") ~ "Papua New Guinea",
      País %in% c("Puerto Rico", "Republica Dominicana", "República Dominicana") ~ "Dominican Republic",
      País %in% c("Cuba") ~ "Cuba",
      País %in% c("Honduras") ~ "Honduras",
      País %in% c("Nicaragua") ~ "Nicaragua",
      País %in% c("Guatemala") ~ "Guatemala",
      País %in% c("Uruguay") ~ "Uruguay",
      País %in% c("Venezuela") ~ "Venezuela",
      País %in% c("Paraguay") ~ "Paraguay",
      País %in% c("Bolivia") ~ "Bolivia",
      País %in% c("Perú") ~ "Peru",
      País %in% c("Ecuador") ~ "Ecuador",
      País %in% c("Haïti") ~ "Haiti",
      País %in% c("Birmania") ~ "Myanmar",
      TRUE ~ NA_character_
    )
  )

library(dplyr)
library(plotly)

library(dplyr)
library(plotly)

# 1. Agrupar datos: cantidad de cardenales por país y año
cardenales_por_pais <- datos %>%
  group_by(Año, country_en) %>%
  summarise(Cantidad = n(), .groups = "drop")

# 2. Crear vector de años únicos
anios <- sort(unique(cardenales_por_pais$Año))

# 3. Definir paleta de colores: amarillo claro → bordo
custom_colorscale <- list(
  c(0, "#fb9d0b"),   # Lemon Chiffon (amarillo claro)
  c(0.5, "#e47a24"), # Naranja medio
  c(1, "#800020")    # Burgundy (bordo intenso)
)

# 4. Crear figura vacía
fig <- plot_ly()

# 5. Agregar trazas por cada cónclave
for (i in seq_along(anios)) {
  año_actual <- anios[i]
  df_año <- filter(cardenales_por_pais, Año == año_actual)
  
  fig <- fig %>% add_trace(
    type = "choropleth",
    locations = df_año$country_en,
    locationmode = "country names",
    z = df_año$Cantidad,
    text = paste0(df_año$country_en, ": ", df_año$Cantidad, " cardenales"),
    hoverinfo = "text",
    colorscale = custom_colorscale,
    zmin = 0,
    zmax = max(cardenales_por_pais$Cantidad),
    marker = list(
      line = list(width = 0.5, color = "#fdf0d5")  # fronteras crema
    ),
    name = as.character(año_actual),
    visible = i == 1,
    showscale = TRUE,
    colorbar = list(
      title = "Cantidad",
      len = 0.6,
      thickness = 15,
      tickcolor = "#fdf0d5",
      tickfont = list(color = "#fdf0d5"),
      titlefont = list(color = "#fdf0d5")
    )
  )
}

# 6. Crear los steps del slider
steps <- lapply(seq_along(anios), function(i) {
  list(
    method = "restyle",
    args = list("visible", as.list(seq_along(anios) == i)),
    label = as.character(anios[i])
  )
})

# 7. Layout final
fig <- fig %>% layout(
  title = list(
    text = "¿Desde qué rincones del mundo llegan los cardenales?",
    font = list(color = "#fdf0d5")
  ),
  geo = list(
    showframe = FALSE,
    showcoastlines = TRUE,
    projection = list(type = 'natural earth'),
    bgcolor = 'rgba(0,0,0,0)',      # fondo transparente
    lakecolor = 'rgba(0,0,0,0)',
    landcolor = "#fdf0d5",          # países sin datos → cremita
    showland = TRUE,
    showcountries = TRUE,
    resolution = 50,
    lataxis = list(range = c(-60, 90))  # excluye la Antártida
  ),
  sliders = list(list(
    active = 0,
    steps = steps,
    currentvalue = list(prefix = "Cónclave: ", font = list(color = "#fdf0d5"))
  )),
  paper_bgcolor = 'rgba(0,0,0,0)',
  plot_bgcolor = 'rgba(0,0,0,0)',
  font = list(color = "#fdf0d5")
)

# 8. Mostrar el mapa
fig


# Tabla top 10 países por año
library(dplyr)
library(tidyr)

# 1. Armar ranking por cónclave (año)
ranking <- datos %>%
  group_by(Año, País) %>%
  summarise(Cardenales = n(), .groups = "drop") %>%
  arrange(Año, desc(Cardenales)) %>%
  group_by(Año) %>%
  slice_head(n = 6) %>%
  mutate(Rank = paste0("Top", row_number()),
         Etiqueta = paste0(País, " (", Cardenales, ")")) %>%
  ungroup()

# 2. Reestructurar a formato ancho
tabla_top <- ranking %>%
  select(Año, Rank, Etiqueta) %>%
  pivot_wider(names_from = Rank, values_from = Etiqueta) %>%
  arrange(Año)

library(knitr)
library(kableExtra)




