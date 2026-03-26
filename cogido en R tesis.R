# ==============================================================================
# 0. PAQUETES (Instalación y carga automática)
# ==============================================================================
paquetes <- c("dplyr", "ggplot2", "scales")

for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# StatsBombR a veces requiere instalación especial desde GitHub
if (!require("StatsBombR", character.only = TRUE)) {
  if (!require("devtools")) install.packages("devtools")
  devtools::install_github("statsbomb/StatsBombR")
  library("StatsBombR")
}

# ==============================================================================
# 1. COMPETICIONES Y PARTIDOS (Mundial 2018)
# ==============================================================================
Comp <- FreeCompetitions()

wc18 <- Comp %>%
  dplyr::filter(
    competition_name == "FIFA World Cup",
    season_name == "2018"
  )

Matches <- FreeMatches(wc18)

# ==============================================================================
# 2. EVENTOS + LIMPIEZA (x,y)
# ==============================================================================
Events_raw <- free_allevents(MatchesDF = Matches, Parallel = FALSE)
Events_clean <- allclean(Events_raw)

eventos_final <- Events_clean %>%
  dplyr::filter(
    !is.na(location.x),
    !is.na(location.y),
    !is.na(player.name)
  ) %>%
  dplyr::transmute(
    jugador     = player.name,
    equipo      = team.name,
    partido_id  = match_id,
    minuto      = minute,
    segundo     = second,
    x           = location.x,
    y           = location.y,
    tipo_evento = type.name
  )

# Guardar CSV (Asegúrate de que la ruta exista en tu PC)
ruta_salida <- "C:\\Users\\ASUS\\Downloads\\eventos_wc2018_xy_statsbomb.csv"
write.csv(eventos_final, ruta_salida, row.names = FALSE)
message("✅ Archivo CSV guardado con éxito en: ", ruta_salida)

# ==============================================================================
# 3. FUNCIÓN PARA DIBUJAR CANCHA + HEATMAP (ESTILO DASHBOARD HTML)
# ==============================================================================
circleFun <- function(center = c(60, 40), radius = 10, npoints = 200) {
  theta <- seq(0, 2 * pi, length.out = npoints)
  data.frame(
    x = center[1] + radius * cos(theta),
    y = center[2] + radius * sin(theta)
  )
}

plot_heatmap_jugador <- function(eventos_final, jugador_sel,
                                 tipos_evento = NULL, 
                                 titulo_extra = NULL) {
  
  # Filtrar datos del jugador
  datos_jug <- eventos_final %>%
    dplyr::filter(jugador == jugador_sel)
  
  if (!is.null(tipos_evento)) {
    datos_jug <- datos_jug %>% dplyr::filter(tipo_evento %in% tipos_evento)
  }
  
  if (nrow(datos_jug) == 0) {
    stop("❌ No hay eventos para ese jugador/filtro. Revisa el nombre exacto.")
  }
  
  centro_circulo <- circleFun(center = c(60, 40), radius = 10, npoints = 200)
  
  tt <- paste("Mapa de Calor (KDE) -", jugador_sel)
  if (!is.null(titulo_extra)) tt <- paste0(tt, " | ", titulo_extra)
  
  # Color del césped idéntico al Dashboard HTML
  color_cesped <- "#14532d" 
  
  ggplot() +
    # 1. Estimación de Densidad (Heatmap difuminado: Transparente -> Rojo -> Naranja -> Amarillo)
    stat_density_2d(
      data = datos_jug,
      aes(x = x, y = y, fill = after_stat(level)),
      geom = "polygon",
      alpha = 0.65, # Transparencia para ver las líneas de la cancha
      bins = 15     # Suavidad del gradiente
    ) +
    
    # 2. Gradiente de colores estilo "Fuego"
    scale_fill_gradientn(
      colors = c(scales::alpha(color_cesped, 0), "#dc2626", "#ea580c", "#eab308"),
      values = scales::rescale(c(0, 0.3, 0.6, 1))
    ) +
    
    # 3. Puntos exactos de los eventos (Opcional, muy tenues)
    geom_point(
      data = datos_jug,
      aes(x = x, y = y),
      size = 0.5,
      alpha = 0.3,
      color = "white"
    ) +
    
    # 4. Dibujo de las líneas de la cancha
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "white", linewidth = 0.8) +
    annotate("segment", x = 60, xend = 60, y = 0, yend = 80, colour = "white", linewidth = 0.8) +
    geom_path(data = centro_circulo, aes(x = x, y = y), colour = "white", linewidth = 0.8) +
    
    annotate("rect", xmin = 0, xmax = 18, ymin = 18, ymax = 62, fill = NA, colour = "white", linewidth = 0.8) +
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", linewidth = 0.8) +
    annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", linewidth = 0.8) +
    annotate("rect", xmin = 114, xmax = 120, ymin = 30, ymax = 50, fill = NA, colour = "white", linewidth = 0.8) +
    annotate("point", x = 11, y = 40, colour = "white", size = 1.5) +
    annotate("point", x = 109, y = 40, colour = "white", size = 1.5) +
    
    # 5. Flecha indicadora de ataque
    annotate("segment", x = 40, xend = 80, y = 85, yend = 85, 
             arrow = arrow(length = unit(0.3, "cm")), colour = "white", linewidth = 1) +
    annotate("text", x = 60, y = 88, label = "Dirección de Ataque", colour = "white", fontface = "bold") +
    
    # 6. Configuración de ejes y tema
    coord_fixed(xlim = c(-5, 125), ylim = c(-5, 95), expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = color_cesped, color = NA),
      plot.background  = element_rect(fill = color_cesped, color = NA),
      panel.grid       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      axis.title       = element_blank(),
      legend.position  = "none", # Ocultamos la leyenda para que se vea más limpio
      plot.title       = element_text(color = "white", face = "bold", hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle    = element_text(color = "#94a3b8", hjust = 0.5)
    ) +
    labs(title = tt, subtitle = paste(nrow(datos_jug), "eventos registrados por StatsBomb"))
}

# ==============================================================================
# 4. EJEMPLOS DE USO
# ==============================================================================

# Mapa de calor de Kylian Mbappé
jugador_1 <- "Kylian Mbappé Lottin"
grafico_mbappe <- plot_heatmap_jugador(eventos_final, jugador_1, titulo_extra = "Influencia Ofensiva")
print(grafico_mbappe)

# Mapa de calor de Neymar
jugador_2 <- "Neymar da Silva Santos Junior"
grafico_neymar <- plot_heatmap_jugador(eventos_final, jugador_2, titulo_extra = "Influencia Ofensiva")
print(grafico_neymar)