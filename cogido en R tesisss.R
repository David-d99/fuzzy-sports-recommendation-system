# ==============================================================================
# SISTEMA DE RECOMENDACIÓN DE PLANIFICACIÓN DEPORTIVA COLABORATIVA DIFUSA
# Autor: Luis David Delgado Solorzano
# ==============================================================================

# -------------------------------
# 1. CARGA DE PAQUETES
# -------------------------------

paquetes <- c(
  "FactoMineR", "factoextra", "ggplot2",
  "plotly", "dplyr", "fmsb", "e1071",
  "FNN", "scales"
)

for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
# -------------------------------
# 2. CARGA DE DATOS
# -------------------------------

ruta <- "data/Datos_futbolistas_con_embeddings.csv"

datos <- read.csv(
  ruta,
  header = TRUE,
  sep = ";",
  dec = ",",
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)

names(datos) <- iconv(names(datos), from = "latin1", to = "UTF-8")

# Identificador
col_nombre <- if ("Nombre" %in% names(datos)) "Nombre" else names(datos)[!sapply(datos, is.numeric)][1]

# Variables numéricas
datos_num <- datos[, sapply(datos, is.numeric), drop = FALSE]

# Limpiar columnas previas
datos_num <- datos_num[, !names(datos_num) %in% c("cluster_kmeans", "cluster_fuzzy"), drop = FALSE]

rownames(datos_num) <- make.unique(as.character(datos[[col_nombre]]))

# Estandarización
datos_est <- scale(datos_num)
# -------------------------------
# 3. CLUSTERING K-MEANS
# -------------------------------

set.seed(123)

fviz_nbclust(datos_est, kmeans, method = "wss") +
  labs(title = "Método del codo")

k_optimo <- 4

km <- kmeans(datos_est, centers = k_optimo, nstart = 25)
datos$cluster_kmeans <- factor(km$cluster)
# -------------------------------
# 4. PCA
# -------------------------------

pca <- PCA(as.data.frame(datos_est), ncp = 4, graph = FALSE)

# Biplot
fviz_pca_biplot(
  pca,
  col.ind = datos$cluster_kmeans,
  palette = c("#E41A1C","#377EB8","#4DAF4A","#984EA3"),
  addEllipses = TRUE,
  repel = TRUE
)

# PCA 3D
coords <- as.data.frame(pca$ind$coord[, 1:3])
colnames(coords) <- c("PC1","PC2","PC3")

coords$cluster <- datos$cluster_kmeans
coords$jugador <- rownames(datos_num)

plot_ly(
  coords,
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d",
  mode = "markers",
  color = ~cluster,
  text = ~jugador
)
# -------------------------------
# 5. RADAR DE CLÚSTERES
# -------------------------------

vars_radar <- intersect(
  c("Goles", "Asistencias", "Partidos.jugados", "Altura", "Amarillas", "Rojas", "Valor.de.mercado"),
  colnames(datos)
)

radar_df <- datos %>%
  select(all_of(vars_radar), cluster_kmeans) %>%
  group_by(cluster_kmeans) %>%
  summarise(across(all_of(vars_radar), mean, na.rm = TRUE))

radar_plot <- as.data.frame(radar_df)
rownames(radar_plot) <- paste("Cluster", radar_plot$cluster_kmeans)
radar_plot$cluster_kmeans <- NULL

radar_plot <- rbind(
  apply(radar_plot, 2, max) * 1.1,
  apply(radar_plot, 2, min) * 0.9,
  radar_plot
)

fmsb::radarchart(radar_plot)
# -------------------------------
# 6. KNN
# -------------------------------

mat <- as.matrix(datos_est)
rownames(mat) <- rownames(datos_num)

get_top_similares <- function(jugador, k = 10) {
  
  if (!jugador %in% rownames(mat)) {
    stop("Jugador no encontrado")
  }
  
  idx <- which(rownames(mat) == jugador)
  
  res <- FNN::get.knnx(mat, mat[idx, , drop = FALSE], k = k + 1)
  
  data.frame(
    jugador = rownames(mat)[res$nn.index[1, -1]],
    distancia = res$nn.dist[1, -1]
  )
}

# Ejemplo
get_top_similares("Kylian Mbappe")
# -------------------------------
# 7. FUZZY C-MEANS
# -------------------------------

set.seed(123)

cm <- e1071::cmeans(
  datos_est,
  centers = k_optimo,
  m = 2
)

datos$cluster_fuzzy <- factor(apply(cm$membership, 1, which.max))

memb_df <- as.data.frame(cm$membership)
colnames(memb_df) <- paste0("Fuzzy_C", 1:k_optimo)

datos <- cbind(datos, memb_df)

# Comparación
table(datos$cluster_kmeans, datos$cluster_fuzzy)
# -------------------------------
# 8. EXPORTAR DATOS
# -------------------------------

write.csv(
  datos,
  "data/datos_dashboard.csv",
  row.names = FALSE
)