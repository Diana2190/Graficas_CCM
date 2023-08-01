## Loading packages
library("ggplot2")
library("dplyr")
library("vroom")
library("lubridate")

## theme
library("tidyverse")
library("cowplot")
library("showtext")
showtext_auto()

## Reading data
df_maestria <- vroom("datos - cimat-maestria.csv")

## Filter
df_maestria_ccm <- df_maestria %>% 
  filter(instituto == "CCM")

## Change as date format
df_maestria_ccm <- df_maestria_ccm %>% 
  mutate(fecha = ymd(fecha, truncated = 2L))

## Filter
df_maestria_cimat <- df_maestria %>% 
  filter(instituto == "CIMAT")

## Change as date format
df_maestria_cimat <- df_maestria_cimat %>% 
  mutate(fecha = ymd(fecha, truncated = 2L))

## 
# Most basic bubble plot
p <- ggplot() +
  geom_line(data = df_maestria_ccm, aes(x=fecha, y=valor, colour=status)) +
  geom_line(data = df_maestria_cimat, aes(x = fecha, y = valor, color = status)) +
  scale_color_manual(values = c("#258B9D", "#DF7761", "#D8B03C", "#3D4F7DFF")) +
  geom_point(data = df_maestria_ccm, aes(x=fecha, y=valor, colour=status)) +
  geom_point(data = df_maestria_cimat, aes(x = fecha, y = valor, color = status)) +
  xlab("") +
  ylab("No. alumnos") + 
  ggtitle("Ingresos y Egresos CCM-CIMAT Maestria") +
  labs(color=NULL) +
  theme_minimal_grid()
p

## Same doctorado
## Reading data
df_doctorado <- vroom("datos - Doctorado.csv")

## Filter
df_doctorado_ccm <- df_doctorado %>% 
  filter(instituto == "CCM")

## Change as date format
df_doctorado_ccm <- df_doctorado_ccm %>% 
  mutate(fecha = ymd(fecha, truncated = 2L))

## Filter
df_doctorado_cimat <- df_doctorado %>% 
  filter(instituto == "CIMAT")

## Change as date format
df_doctorado_cimat <- df_doctorado_cimat %>% 
  mutate(fecha = ymd(fecha, truncated = 2L))

## 
# Most basic bubble plot
p1 <- ggplot() +
  geom_line(data = df_doctorado_ccm, aes(x=fecha, y=valor, colour=status)) +
  geom_line(data = df_doctorado_cimat, aes(x = fecha, y = valor, color = status)) +
  scale_color_manual(values = c("#258B9D", "#DF7761", "#D8B03C", "#3D4F7DFF")) +
  geom_point(data = df_doctorado_ccm, aes(x=fecha, y=valor, colour=status)) +
  geom_point(data = df_doctorado_cimat, aes(x = fecha, y = valor, color = status)) +
  xlab("") +
  ylab("No. alumnos") + 
  ggtitle("Ingresos y Egresos CCM-CIMAT Doctorado") +
  labs(color=NULL) +
  theme_minimal_grid()
p1

## Reunite plot
p2 <- plot_grid(p,p1, ncol = 1)
p2

##
ggsave(filename = "rladies.png", 
       plot = p2, 
       device = "png",
       width = 7, height = 8, units = "in",
       bg = "white",
       dpi = 300)

### Graph two

## Read data
lolli_df <- vroom("datos - Hoja 7.csv")

## 
lolli_df <- lolli_df %>%
  mutate(impact_factor = as.numeric(gsub(",", ".", lolli_df$impact_factor))) %>% 
  mutate(year = as.character(year))

##
graficable1 <- lolli_df %>%                               # a partir de los datos completos...
  group_by( year ) %>%                                # agrupamos la tabla en subtablas, de acuerdo a la temporada del episodio
  mutate( promedio_year = mean( impact_factor ) ) %>%  # creamos una nueva columna llamada promedio_temporada. El mean( ) se va  a calcular en cada subset del dataframe
  mutate( inicio_temporada = min( month ) ) %>%           # creamos nuevas columnas. La coordenada x 1 sera el numero  global del primer episodio
  mutate( fin_temporada = max( month ) ) %>%              # La coordenada x2 sera el numero global del ultimo episodio de la temporada
  ungroup( )  

##
segmentos_promedio <- graficable1 %>%           # a partir de donde calculamos los promedios
  select( year,                            # seleccionamos solo las columnas temporada,
          promedio_year,                   # y promedio
          inicio_temporada,                     # y el indice del primer episodio
          fin_temporada ) %>%                   # y el indice del ultimo episodio
  unique( )   

# plot de puntos
escalera1 <- ggplot( data = graficable1,                       # a partir del datframe graficable
                     mapping = aes( x = month,                # en el eje x va el numero global del episodio
                                    y = impact_factor,          # en el eje y va la calificacion de la critica
                                    color = year ) ) +    # el color de lo dibujado dependera del valor "temporada"
  geom_segment( mapping = aes( xend = month,                  # dibujamos el palo de paleta, el valor en x es el mismo indice de episodio
                               yend = promedio_year ),    # y el final del palo de paleta sera el promedio de cada temporada
                linetype = "dashed" ) +                        # tiempo de linea "rayada"
  geom_point(  )                                               # dibujamos un punto al final de la paleta

# Visualizamos
escalera1

##
# Agregamos la linea de promedio por temporada
escalera2 <- escalera1 +                                       # a partir del grafico 1
  geom_segment( data = segmentos_promedio,                     # usamos otro dataframe
                mapping = aes( x = inicio_temporada,           # la raya del promedio Empieza en el primer episodio de la temporada
                               xend = fin_temporada,           # la raya del promedio Termina en el primer episodio de la temporada
                               y = promedio_year,         # la raya del promedio, en el eje y empieza en el promedio
                               yend = promedio_year ),    # y termina tambien en el promedio
                size = 1.5,                                    # hacemos la linea poco mas gruesa
                lineend = "round" )                              # el extremo de las lineas sera redondeado

# Vis
escalera2

###############
# Add fonts from Google.
font_add_google("Roboto Mono", "Roboto Mono")
font_add_google("Open Sans", "Open Sans")
font_add_google("Special Elite", "Special Elite")

theme_set(theme_minimal(base_family = "Roboto Mono"))
theme_update(
  plot.background = element_rect(fill = "#fafaf5", color = "#fafaf5"),
  panel.background = element_rect(fill = NA, color = NA),
  panel.border = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 25),
  axis.ticks = element_blank(),
  axis.title.y = element_text(size = 25, margin = margin(r = 10)),
  legend.title = element_text(size = 25),
  plot.title = element_text(face="bold"),
  plot.caption = element_text(
    family = "Special Elite",
    size = 10,
    color = "grey70",
    face = "bold",
    hjust = .5,
    margin = margin(5, 0, 20, 0)
  ),
  plot.margin = margin(10, 25, 10, 25)
)

# Turn on showtext
showtext_auto()

#################

##
escalera3 <- escalera2 +
  ggtitle("Factor de Impacto de Publicaciones CCM") +
  scale_y_continuous( breaks = 0:14,                       # ponemos marcas del 4 al 10, de uno en uno
                      sec.axis = dup_axis(name = NULL) ) +           # duplicamos el eje del otro lado
  scale_color_d3( ) +
  labs(y = "Factor de Impacto",
       color = "Año") + 
  theme(
        text = element_text(size = 30),
        axis.text = element_text(size = 40),
        axis.title.x = element_blank())
escalera3  

##
ggsave(filename = "rladies2.png", 
       plot = escalera3, 
       device = "png",
       width = 9, height = 5, units = "in",
       bg = "white",
       dpi = 300)
