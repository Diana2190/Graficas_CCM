## Loading packages
library("ggplot2")
library("dplyr")
library("vroom")
library("lubridate")

## Reading data
df_maestria <- vroom("C:/Users/52747/Downloads/datos - cimat-maestria.csv", show_col_types = FALSE)
df_doctorado <- vroom("C:/Users/52747/Downloads/datos - Doctorado.csv", show_col_types = FALSE)

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
  xlab("")
p


###DOCTORADO
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
p <- ggplot() +
  geom_line(data = df_doctorado_ccm, aes(x=fecha, y=valor, colour=status)) +
  geom_line(data = df_doctorado_cimat, aes(x = fecha, y = valor, color = status), linetype = "dashed") +
  xlab("") 
p


