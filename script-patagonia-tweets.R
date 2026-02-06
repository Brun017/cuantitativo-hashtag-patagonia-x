# 1. Llamo librerias necesarias
library(tidytext) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# 2. Importo csv obtenido con la herramienta Instant Data Scraper con una base de datos de alrededor de 400 tweets sobre el hashtag "patagonia"

datos_crudos <- read_csv("~/Escritorio/Ejercicios R/Analisis cuantitativo/Trabajo final/x.csv")


# 3. Limpio y ordeno la tabla para que sea legible: reconstrucción de los tuits  a analizar (IDS los fragmenta en varias partes) y cambio de nombre a las columnas

datos_limpios <- datos_crudos %>%
  select(
    usuario = `css-1jxf684 2`, 
    mencion = `css-1jxf684 6`,
    texto_parte1 = `css-1jxf684 7`,   
    texto_parte2 = `css-1jxf684 8`,   
    texto_parte3 = `css-1jxf684 9`    
  ) %>%
  mutate(across(starts_with("texto"), ~replace_na(., ""))) %>%
  mutate(tuit_completo = paste(mencion, texto_parte1, texto_parte2, texto_parte3)) %>%
  mutate(tuit_completo = str_squish(tuit_completo)) %>%
  filter(tuit_completo != "") %>%
  select(usuario, tuit_completo)

# 4. Filtro las entradas donde aparece citado @grok. Uso función str_to_lower para que coincidan "grok", "GROK" y "Grok"

tuits_grok <- datos_limpios %>%
  filter(str_detect(str_to_lower(tuit_completo), "grok"))

# 5. Análisis 1: análisis comparativo, ¿qué frecuente es la "invocación" a Grok en la conversación?

# a. Creo un drataframe con dos variables
data_grafico <- data.frame(
  Categoria = c("Total Conversación", "Interacciones con Grok"),
  Cantidad = c(nrow(datos_limpios), nrow(tuits_grok))
)

# b. Creo la visualización en gráfico de barras
ggplot(data_grafico, aes(x = Categoria, y = Cantidad, fill = Categoria)) +
  geom_col(width = 0.6, alpha = 0.8) +  
  geom_text(aes(label = Cantidad), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Total conversación" = "grey70", "Interacciones con Grok" = "steelblue")) +
  labs(
    title = "Llamados a Grok en la tendencia",
    x = "",
    y = "Cantidad de tuits"
  ) +
  theme_minimal() + 
  theme(legend.position = "none")

# c. Calculo la tasa de llamados a Grok en la tendencia
total_tuits <- nrow(datos_limpios)
total_grok <- nrow(tuits_grok)
tasa <- round(total_tuits / total_grok)

datos_tasa <- data.frame(
  categoria = c("Otros tuits", "Mención a Grok"),
  cantidad = c(total_tuits - total_grok, total_grok) 
)

# d. Gráfico de frecuencia de aparición de Grok en la tendencia

datos_tasa$fraccion = datos_tasa$cantidad / sum(datos_tasa$cantidad)
datos_tasa$ymax = cumsum(datos_tasa$fraccion)
datos_tasa$ymin = c(0, head(datos_tasa$ymax, n=-1))

ggplot(datos_tasa, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=categoria)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) + 
  scale_fill_manual(values = c("Mención a Grok" = "#1DA1F2", "Otros tuits" = "grey85")) +
  annotate("text", x = 2, y = 0, 
           label = paste("1 cada", tasa), 
           fontface = "bold", size = 8, color = "#333333") +
    annotate("text", x = 2, y = 0, 
           label = "\ntuits", # Salto de línea para poner "tuits" abajo
           vjust = 2.5, size = 4, color = "grey40") +
  theme_void() +
  labs(title = "Frecuencia de aparición de Grok en la conversación") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

# Análisis 2: análisis cualitativo ¿De qué forma y para qué se lo llama a Grok en la conversación?

# a. Tokenizo el corpus y realizo limpieza de palabras vacías

datos_tokenizados_grok <- tuits_grok %>%
  unnest_tokens(input = tuit_completo, output = palabra)

vacias_es<- get_stopwords(language = "es")
vacias_en <- get_stopwords(language = "en")
vacias_totales <- bind_rows(vacias_es, vacias_en) %>%
  rename(palabra = word)

datos_tokenizados_grok<- datos_tokenizados_grok %>%
  anti_join(vacias_totales, by = "palabra") 

# b. 15 palabras más utilizadas en las interacciones con Grok

top_palabras_grok <- datos_tokenizados_grok %>%
  count(palabra, sort = TRUE) %>%
  head(15)

print("--- ¿QUÉ LE DICEN A GROK? (TOP 15) ---")
print(top_palabras_grok)

# 7. Análisis de co ocurrencias
# a. Tokenizo pares de palabras para generar bigramas
bigramas_grok <- tuits_grok %>%
  unnest_tokens(input = tuit_completo, output = bigrama, token = "ngrams", n = 2) %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  filter(!palabra1 %in% vacias_totales$palabra) %>%
  filter(!palabra2 %in% vacias_totales$palabra) %>%
  filter(!palabra1 %in% c("https", "t.co", "rt")) %>%
  filter(!palabra2 %in% c("https", "t.co", "rt")) %>%
  filter(palabra1 == "grok" | palabra2 == "grok") %>%
  count(palabra1, palabra2, sort = TRUE)

# b. Genero dataframe para distinguir entre palabras previas y palabras siguientes
co_ocurrencias_grok <- bigramas_grok %>%
  mutate(posicion = case_when(
    palabra2 == "grok" ~ "Antes de Grok",   
    palabra1 == "grok" ~ "Después de Grok"  
  )) %>%
  mutate(palabra_asociada = ifelse(palabra1 == "grok", palabra2, palabra1)) %>%
  select(posicion, palabra_asociada, n) %>%
  group_by(posicion) %>%
  slice_max(n, n = 10) %>% 
  ungroup()

# c. Gráfico facetado de co-ocurrencias según sucedan antes o después de la cita a Grok
ggplot(co_ocurrencias_grok, aes(x = reorder_within(palabra_asociada, n, posicion), y = n, fill = posicion)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~posicion, scales = "free_y") + 
  coord_flip() + 
  scale_x_reordered() +
  theme_bw() +
  labs(title = "Contexto de Grok",
       x = "", y = "Frecuencia")

# d. Genero tabla de contextos
tabla_contexto <- tuits_grok %>%
  mutate(id_tuit = row_number()) %>%
  mutate(lista_palabras = str_split(tuit_completo, "\\s+")) %>%
  unnest(lista_palabras) %>%
  group_by(id_tuit) %>%
  mutate(
    palabra_previa = lag(lista_palabras),
    centro = lista_palabras,
    palabra_posterior = lead(lista_palabras)
  ) %>%
  ungroup() %>%
  filter(str_detect(centro, regex("grok", ignore_case = TRUE))) %>%
  select(palabra_previa, centro, palabra_posterior)

# 8. Análisis de palabras distintivas

# a. Creo nuevo objeto con dos obejtos:"Interacciones con Grok" y "Tendencia general"
palabras_comparativas <- datos_limpios %>%
  mutate(tipo = ifelse(str_detect(str_to_lower(tuit_completo), "grok"), 
                       "Interacción Grok", 
                       "Tendencia general")) %>%
  
# b. Tokenizo y limpio
  unnest_tokens(input = tuit_completo, output = palabra) %>%
  anti_join(vacias_totales, by = "palabra") %>%
  filter(!palabra %in% c("https", "t.co", "rt", "twitter", "grok", "arrobagrok")) %>%
  count(tipo, palabra) %>%
  group_by(tipo) %>%
  mutate(proporcion = n / sum(n)) %>% 
  ungroup() %>%
  select(tipo, palabra, proporcion) %>%
  spread(tipo, proporcion) %>%
  na.omit()

# c. Gráfico de tendencias: palabras más probables en todos los tuits vs. los que invocan a Grok

ggplot(palabras_comparativas, aes(x = `Tendencia general`, y = `Interacción Grok`, label = palabra)) +
  geom_abline(color = "red", lty = 2) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_text(aes(label = palabra), check_overlap = TRUE, vjust = 1.5, size = 3.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  theme_light() +
  labs(title = "Tendencia de palabras general vs Grok",
       subtitle = "Arriba de la línea roja: palabras exclusivas del trato con Grok",
       x = "Frecuencia en conversación general",
       y = "Frecuencia con Grok")


