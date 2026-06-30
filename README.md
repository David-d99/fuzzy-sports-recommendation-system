# Fuzzy Sports Recommendation System

**Sistema de recomendación de planificación deportiva colaborativa difusa**

---

## 📌 Descripción del proyecto

Este repositorio contiene el código fuente y los datos necesarios para replicar el sistema de recomendación de planificación deportiva colaborativa difusa, desarrollado como parte del trabajo de titulación:

- **Título**: Sistema de recomendación de planificación deportiva colaborativa difusa.
- **Autor**: Luis David Delgado Solorzano.
- **Tutor**: PhD. Emanuel Guillermo Muñoz Muñoz.
- **Institución**: Universidad Técnica de Manabí, Carrera de Estadística.
- **Año**: 2026.

El sistema integra técnicas de aprendizaje automático (K-means, Fuzzy C-means, KNN), representaciones vectoriales semánticas (Word2Vec), análisis de componentes principales (PCA) y análisis espacial mediante mapas de calor (KDE) para recomendar jugadores de fútbol con perfiles similares, apoyando la toma de decisiones en el *scouting* y la planificación táctica.

---

## ⚙️ Tecnologías utilizadas

- **Entorno**: Google Colab (Python 3.11).
- **Librerías principales**: pandas, numpy, matplotlib, seaborn, scikit-learn, gensim, scikit-fuzzy, hdbscan, fpdf2.
- **Técnicas**: Word2Vec (embeddings), PCA, K-means, Fuzzy C-means, KNN, Kernel Density Estimation (KDE).
- **Fuentes de datos**: StatsBomb Open Data (eventos espaciales) y Transfermarkt (características de jugadores).

---

## 📂 Estructura del repositorio

---

## 🚀 Instrucciones de ejecución

Sigue estos pasos para ejecutar el sistema en tu entorno local o en la nube:

1. **Abrir el notebook**:
   - Accede a Google Colab (`https://colab.research.google.com/`).
   - Sube el archivo `Sistema_Recomendacion_Deportiva.ipynb` desde la carpeta `/notebooks`.

2. **Montar Google Drive (opcional)**:
   - Si los archivos CSV están en tu Drive, ejecuta la celda de montaje y ajusta las rutas.
   - Si prefieres trabajar localmente, cambia las rutas a las carpetas correspondientes.

3. **Ejecutar todas las celdas**:
   - El notebook está diseñado para ejecutarse de principio a fin.
   - Las visualizaciones (gráfico PCA, radar, mapas de calor) se generarán automáticamente.

4. **Interactuar con el dashboard**:
   - Al llegar a la sección de sliders, modifica los valores para simular un perfil de jugador.
   - Observa cómo cambia la asignación de clúster, la lista de jugadores similares y los mapas de calor.

5. **Exportar reportes**:
   - Al finalizar, el sistema generará archivos CSV, TXT y un PDF con los resultados.

---

## 📊 Datos

- `recolección de datos.csv`: Contiene información de 604 jugadores de campo (excluyendo porteros) con variables normalizadas al rango [0,100] y etiquetas de clúster.
- `eventos_wc2018_xy_statsbomb.csv`: Registros de eventos de la Copa Mundial de la FIFA 2018 con coordenadas (x,y) y nombre del jugador.

---

## 🧠 Funcionalidades principales

- **Segmentación de jugadores**: Aplicación de K-means (agrupamiento rígido) y Fuzzy C-means (difuso) para identificar perfiles tácticos.
- **Recomendación por similitud**: Módulo KNN que devuelve los 10 jugadores más cercanos al perfil objetivo dentro del mismo clúster.
- **Análisis espacial**: Mapas de calor mediante estimación de densidad por kernel (KDE) para visualizar la ocupación del campo.
- **Dashboard interactivo**: Interfaz con sliders que permite definir perfiles personalizados y explorar resultados en tiempo real.
- **Exportación automatizada**: Generación de reportes en CSV, TXT y PDF.

---

## 🔍 Resultados principales

- Identificación de **4 clústeres tácticos interpretables**: *Creadores de juego posicionales*, *Defensores agresivos y disciplinados*, *Perfil físico y de área*, y *Finalizadores de alto impacto*.
- Concordancia moderada entre K-means y Fuzzy C-means (**ARI = 0.560**), revelando perfiles híbridos.
- Recomendaciones coherentes (caso de estudio: Cristiano Ronaldo → Luis Suárez, Lewandowski, Messi, etc.).
- Mapas de calor que reflejan con precisión los roles tácticos reales de los jugadores.

---

## 📝 Licencia

Este proyecto está bajo la licencia MIT. Consulta el archivo `LICENSE` para más detalles.

---

## 📧 Contacto

- **Autor**: Luis David Delgado Solorzano
- **Correo institucional**: [ldelgado4873@utm.edu.ec]
- **GitHub**: [David-d99](https://github.com/David-d99)

---

**Universidad Técnica de Manabí**  
**Facultad de Ciencias Básicas**  
**Carrera de Estadística**

---
