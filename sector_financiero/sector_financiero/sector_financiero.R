{
library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
library(psych)
library(ggplot2)
library(stats)
library(modeest)
}

# Limpieza ----------------------------------------------------------------
setwd("C:/Users/USER/Documents/YRF_PROJECT/")

data <- read_excel("sector_financiero/data/modificada/BMS_CAMBIOS.xlsx")

{
colnames(data) = c("año", "mes", "dia","reservas_internacionales", "pasivos_monetarios", "emision_monetaria",
                   "dinero_electronico", "reservas_bancarias", "depositos_a_la_vista", "cuasidinero_total",
                   "cuasidinero_ahorro", "cuasidinero_plazo", "cuasidinero_restringido", "cuasidinero_operaciones_de_reporto",
                   "cuasidinero_otros_depositos", "credito_al_sector_privado_total", "credito_al_sector_privado_cartera",
                   "crédito_al_sector_privado_otros", "tasas_de_interes_referenciales_porcentajes_basica",
                   "tasas_de_interes_referenciales_porcentajes_pasiva", "tasas_de_interes_referenciales_porcentajes_activa",
                   "tasas_de_interes_referenciales_porcentajes_activa_de_corto_plazo_para_el", "inflacion_mensual", "inflacion_anual",
                   "inflacion_acumulada")

}# Nombres de las variables

{
# Reservas internacionales  RI(11) // reservas_internacionales
# Pasivos Monetarios  PM (1) // pasivos_monetarios
# Emisión monetaria EM(2) // emision_monetaria
# Dinero Electrónico DE(3) // dinero_electronico
# Reservas bancarias (Rb) // reservas_bancarias
# Depósitos a la vista (4) // depositos_a_la_vista
# Cuasidinero Total // cuasidinero_total
# Cuasidinero Ahorro // cuasidinero_ahorro
# Cuasidinero Plazo // cuasidinero_plazo
# Cuasidinero Restringido // cuasidinero_restringido
# Cuasidinero Operaciones de reporto // cuasidinero_operaciones_de_reporto
# Cuasidinero Otros depósitos // cuasidinero_otros_depositos
# Crédito al sector privado(6) Total // credito_al_sector_privado_total
# Crédito al sector privado(6) Cartera // credito_al_sector_privado_cartera
# Crédito al sector privado(6) Otros // crédito_al_sector_privado_otros
# TASAS DE INTERÉS REFERENCIALES porcentajes Básica (7) // tasas_de_interes_referenciales_porcentajes_basica
# TASAS DE INTERÉS REFERENCIALES porcentajes Pasiva (8) // tasas_de_interes_referenciales_porcentajes_pasiva
# TASAS DE INTERÉS REFERENCIALES porcentajes Activa (9) // tasas_de_interes_referenciales_porcentajes_activa
# TASAS DE INTERÉS REFERENCIALES porcentajes Activa de Corto Plazo para el segmento Productivo Corporativo (10) // tasas_de_interes_referenciales_porcentajes_activa_de_corto_plazo_para_el segmto_productivo_corporativo
# INFLACIÓN porcentajes Mensual // inflacion_mensual
# INFLACIÓN porcentajes Anual // inflacion_anual
# INFLACIÓN porcentajes Acumulada // inflacion_acumulada
}# Definir nombres

{
data_clean <- data %>% select(1:25) %>% filter(!if_all(everything(), is.na)) # seleccionar todas la variables

data_clean$año = na.locf(data_clean$año) # mezclar todas las variables para formato fecha

# Mapeo de nombres de meses en español a números
month_map <- setNames(1:12, c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",                              
                              "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))

# Transformar `mes` de nombres en español a numéricos y combinar `año`, `mes`, `dia` en una `fecha`

data_clean2 <- data_clean %>%
  mutate(mes = month_map[mes],
         date = make_date(año, mes, dia)) %>%
  select(-año, -mes, -dia) %>%
  rename(fecha = date) %>%
  relocate(fecha, .before = everything())  # Mueve la columna fecha al primer lugar

}# Fechas

{
  sapply(data_clean2, class)
  
  data_clean3 <- data_clean2 %>%
    mutate(across(where(is.character) & !contains("fecha"), as.numeric))
  
  sapply(data_clean3, class)
}# Convertir variables a numericas

# Limpieza de datos atipicos

# Valores "n.d" y "n.d." a NA
colSums(is.na(data_clean3))

data_clean3[] <- lapply(data_clean3, function(x) {
  if (is.character(x)) x[x %in% c("n.d", "n.d.")] <- NA
  return(x)
})

# Rango de fechas
data_clean4 <- subset(data_clean3, fecha >= as.Date("2007-01-01") & fecha <= as.Date("2024-08-31"))

# Eliminar valores NA


# Estadística descriptiva ---------------------------------------------------------
# Reservas internacionales
summary(data_clean3$reservas_internacionales)
describe(data_clean3$reservas_internacionales)
var(data_clean3$reservas_internacionales)

#Pasivos monenetarios
summary(data_clean3$pasivos_monetarios)
describe(data_clean3$pasivos_monetarios)
var(data_clean3$pasivos_monetarios)

# Emisión monetaria
summary(data_clean3$emision_monetaria)
describe(data_clean3$emision_monetaria)
var(data_clean3$emision_monetaria)

# Reservas bancarias
summary(data_clean3$reservas_bancarias)
describe(data_clean3$reservas_bancarias)
var(data_clean3$reservas_bancarias)

# Depositos a la vista
summary(data_clean3$depositos_a_la_vista)
describe(data_clean3$depositos_a_la_vista)
var(data_clean3$depositos_a_la_vista, na.rm = TRUE)

# Cuasidinero total
summary(data_clean3$cuasidinero_total)
describe(data_clean3$cuasidinero_total)
var(data_clean3$cuasidinero_total, na.rm = TRUE)

# Credito al   sector privado total
summary(data_clean3$credito_al_sector_privado_total)
describe(data_clean3$credito_al_sector_privado_total)
var(data_clean3$credito_al_sector_privado_total, na.rm = TRUE)

# Tasas de interés referecniales (%) básicas
summary(data_clean3$tasas_de_interes_referenciales_porcentajes_basica)
describe(data_clean3$tasas_de_interes_referenciales_porcentajes_basica)
var(data_clean3$tasas_de_interes_referenciales_porcentajes_basica, na.rm = TRUE)

# Tasas de interés referecniales (%) pásiva
summary(data_clean3$tasas_de_interes_referenciales_porcentajes_pasiva)
describe(data_clean3$tasas_de_interes_referenciales_porcentajes_pasiva)
var(data_clean3$tasas_de_interes_referenciales_porcentajes_pasiva, na.rm = TRUE)

# Tasas de interés referecniales (%) activa
summary(data_clean3$tasas_de_interes_referenciales_porcentajes_activa)
describe(data_clean3$tasas_de_interes_referenciales_porcentajes_activa)
var(data_clean3$tasas_de_interes_referenciales_porcentajes_activa, na.rm = TRUE)

# Inflación mensual
summary(data_clean3$inflacion_mensual)
describe(data_clean3$inflacion_mensual)
var(data_clean3$inflacion_mensual, na.rm = TRUE)

# Inflación anual
summary(data_clean3$inflacion_anual)
describe(data_clean3$inflacion_anual)
var(data_clean3$inflacion_anual, na.rm = TRUE)

# Inflación acumulada
summary(data_clean3$inflacion_acumulada)
describe(data_clean3$inflacion_acumulada)
var(data_clean3$inflacion_acumulada, na.rm = TRUE)


colSums(is.na(data_clean3))


# Gráfica -----------------------------------------------------------------
# Reservas internacionales
{
mean_1<-mean(data_clean3$reservas_internacionales)/1000
plot_reser_int <- ggplot(data_clean3, aes(x = fecha, y = reservas_internacionales/1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "Rservas Internacionales (En millones de dólares)",
      x = "PERIODO",
      y = "Reservas Internacionales",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(4.554053), linetype = "dashed", color = "red")
  
pdf("plot_reser_int.pdf", height = 5.5, width = 8)
plot_reser_int
dev.off()
}  
  
#Pasivos monenetarios
{
  mean_2<-mean(data_clean3$pasivos_monetarios)/1000
  plot_pasiv_mon <- ggplot(data_clean3, aes(x = fecha, y = pasivos_monetarios/1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "Pasivos monetarios (En millones de dólares)",
      x = "PERIODO",
      y = "Pasivos monetarios",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(3.617957), linetype = "dashed", color = "red")
  
  pdf("plot_pasiv_mon.pdf", height = 5.5, width = 8)
  plot_pasiv_mon
  dev.off()
}

# Emisión monetaria
{
  mean_3<-mean(data_clean3$emision_monetaria)
  plot_emis_mon <- ggplot(data_clean3, aes(x = fecha, y = emision_monetaria)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "Pasivos monetarios (En millones de dólares)",
      x = "PERIODO",
      y = "Pasivos monetarios",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(mean_3), linetype = "dashed", color = "red")
  
  pdf("plot_pasiv_mon.pdf", height = 5.5, width = 8)
  plot_pasiv_mon
  dev.off()
  }


# Reservas bancarias
{
  mean_4<-mean(data_clean3$reservas_bancarias)/1000
  plot_rser_banc <- ggplot(data_clean3, aes(x = fecha, y = reservas_bancarias /1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "Reservas bancarias (En millones de dólares)",
      x = "PERIODO",
      y = "Rservas bancarias",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(mean_4), linetype = "dashed", color = "red")
  
  pdf("plot_rser_banc.pdf", height = 5.5, width = 8)
  plot_rser_banc
  dev.off()
  }

# Depositos a la vista
{
  mean_5<-mean(data_clean3$depositos_a_la_vista, na.rm = TRUE)/1000 
  plot_dep_vista <- ggplot(data_clean3, aes(x = fecha, y = depositos_a_la_vista /1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "Depositos a la vista (En millones de dólares)",
      x = "PERIODO",
      y = "Depositos a la vista",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(mean_5), linetype = "dashed", color = "red")
  
  pdf("plot_rser_banc.pdf", height = 5.5, width = 8)
  plot_rser_banc
  dev.off()
  }


# Cuasidinero total
ggplot(data_clean3, aes(x = fecha, y = cuasidinero_total))+
  geom_line()+
  geom_point()+
  theme_minimal() +
  theme_classic()

# Credito al   sector privado total
ggplot(data_clean3, aes(x = fecha, y = credito_al_sector_privado_total))+
  geom_line()+
  geom_point()+
  theme_minimal() +
  theme_classic()

# Tasas de interés referecniales (%) básicas
ggplot(data_clean3, aes(x = fecha, y = tasas_de_interes_referenciales_porcentajes_basica))+
  geom_line()+
  geom_point()+
  theme_minimal() +
  theme_classic()

# Tasas de interés referecniales (%) activa
ggplot(data_clean3, aes(x = fecha, y = tasas_de_interes_referenciales_porcentajes_activa))+
  geom_line()+
  geom_point()+
  theme_minimal() +
  theme_classic()

# Tasas de interés referecniales (%) pásiva
ggplot(data_clean3, aes(x = fecha, y = tasas_de_interes_referenciales_porcentajes_pasiva))+
  geom_line()+
  geom_point()+
  theme_minimal() +
  theme_classic()

# Inflación mensual
ggplot(data_clean3, aes(x = fecha, y = inflacion_mensual))+
  geom_line()+
  geom_point()+
  theme_minimal() +
  theme_classic()

# Inflación anual
ggplot(data_clean3, aes(x = fecha, y = inflacion_anual))+
  geom_line()+
  geom_point()+
  theme_minimal() +
  theme_classic()


# Inflación acumulada
ggplot(data_clean3, aes(x = fecha, y = inflacion_acumulada))+
  geom_line()+
  geom_point()+
  theme_minimal() +
  theme_classic()




