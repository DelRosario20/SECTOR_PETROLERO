# Limpieza ----------------------------------------------------------------
# Librería
{
  library(readxl)
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(psych)
  library(ggplot2)
  library(stats)
  library(modeest)
  library(stargazer)
}
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

# Eleminar columna
data_clean3 <- data_clean3 %>%
  select(-tasas_de_interes_referenciales_porcentajes_activa_de_corto_plazo_para_el)

# Rango para evitar NA
data_clean3 <- data_clean3 %>%
  filter(!(fecha %in% as.Date(c("2024-09-13", "2024-09-20", "2024-09-27", "2024-09-30","2024-09-06"))))

# Convertir a datos anuales
# Datos anuales
data_clean4 <- data_clean3 %>%
  mutate(fecha = as.Date(paste0(format(fecha, "%Y"), "-01-01"))) %>%  
  group_by(fecha) %>%                                                
  summarise(across(where(is.numeric), sum, na.rm = TRUE))  

data_clean5 <- data_clean4 %>% filter(format(fecha, "%Y") != "2024")

# Limpieza de datos atipicos
{
# Valores "n.d" y "n.d." a NA
colSums(is.na(data_clean3))

data_clean3[] <- lapply(data_clean3, function(x) {
  if (is.character(x)) x[x %in% c("n.d", "n.d.")] <- NA
  return(x)
})

# Rango de fechas
data_clean4 <- subset(data_clean3, fecha >= as.Date("2007-01-01") & fecha <= as.Date("2024-08-31"))

# Eliminar valores NA
}

# Estadística descriptiva ---------------------------------------------------------
generar_resumen <- function(variable, nombre_archivo) {
  # Crear un data frame con las estadísticas descriptivas
  resumen <- data.frame(
    N = length(variable),
    Mean = mean(variable, na.rm = TRUE),
    `St. Dev` = sd(variable, na.rm = TRUE),
    Min = min(variable, na.rm = TRUE),
    Max = max(variable, na.rm = TRUE)
  )
  
  # Exportar la tabla a un archivo .tex
  stargazer(
    resumen,
    summary = FALSE,
    rownames = FALSE,
    type = "latex",
    out = nombre_archivo
  )
}

# Generar tablas individuales para cada variable
generar_resumen(data_clean3$reservas_internacionales, "reser_int.tex")
generar_resumen(data_clean3$depositos_a_la_vista, "dep_vist.tex")
generar_resumen(data_clean3$cuasidinero_total, "cuas_din.tex")
generar_resumen(data_clean3$credito_al_sector_privado_total, "cred_pt.tex")
generar_resumen(data_clean3$tasas_de_interes_referenciales_porcentajes_activa, "tasas_act.tex")
generar_resumen(data_clean3$inflacion_anual, "inflac_anual.tex")

# Gráfica -----------------------------------------------------------------
# Reservas internacionales
{
plot_reser_int <- ggplot(data_clean5, aes(x = fecha, y = log(reservas_internacionales))) +
  geom_line(color = "black") +
  theme_minimal() +
  labs(
    title = "Evolución de las reservas internacionales",
    x = "PERIODOS",
    y = "Reservas Internacionales",
    caption = "Nota: Valores en escala logarítmica"
  ) +
  scale_x_date(
    date_breaks = "1 year",          # Mostrar etiquetas cada año
    date_labels = "%Y"              # Formato del año (e.g., 2007, 2008)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()
  
pdf("plot_reser_int.pdf", height = 5.5, width = 8)
plot_reser_int
dev.off()
}  

# Depositos a la vista
{
  plot_dep_vista <- ggplot(data_clean5, aes(x = fecha, y = log(depositos_a_la_vista))) +
    geom_line(color = "black") +
    theme_minimal() +
    labs(
      title = "Evolución de los depositos a la vista",
      x = "PERIODOS",
      y = "Depositos a la vista",
      caption = "Nota: Valores en escala logarítmica"
    ) +
    scale_x_date(
      date_breaks = "1 year",          # Mostrar etiquetas cada año
      date_labels = "%Y"              # Formato del año (e.g., 2007, 2008)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_classic()
  
  pdf("plot_dep_vista.pdf", height = 5.5, width = 8)
  plot_dep_vista
  dev.off()
}

# Cuasidinero total
{
  plot_cuas_total <- ggplot(data_clean5, aes(x = fecha, y = log(cuasidinero_total))) +
  geom_line(color = "black") +
  theme_minimal() +
  labs(
    title = "Evolución del cuasidinero total",
    x = "PERIODOS",
    y = "Cuasidinero total",
    caption = "Nota: Valores en escala logarítmica"
  ) +
  scale_x_date(
    date_breaks = "1 year",          # Mostrar etiquetas cada año
    date_labels = "%Y"              # Formato del año (e.g., 2007, 2008)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()
  
  pdf("plot_cuas_total.pdf", height = 5.5, width = 8)
  plot_cuas_total
  dev.off()
}

# Credito al   sector privado total
{
  plot_sect_pt<- ggplot(data_clean5, aes(x = fecha, y = log(credito_al_sector_privado_total))) +
  geom_line(color = "black") +
  theme_minimal() +
  labs(
    title = "Evolución de las reservas internacionales",
    x = "PERIODOS",
    y = "Reservas Internacionales",
    caption = "Nota: Valores en escala logarítmica"
  ) +
  scale_x_date(
    date_breaks = "1 year",          # Mostrar etiquetas cada año
    date_labels = "%Y"              # Formato del año (e.g., 2007, 2008)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()
  
  pdf("plot_sect_pt.pdf", height = 5.5, width = 8)
  plot_sect_pt
  dev.off()
}  
  
# Tasas de interés referenciales (%) activa
{
  plot_tas_activa<- ggplot(data_clean5, aes(x = fecha, y = tasas_de_interes_referenciales_porcentajes_activa)) +
  geom_line(color = "black") +
  theme_minimal() +
  labs(
    title = "Evolución de la tasas de interés referenciales activa (%)",
    x = "PERIODOS",
    y = "Tasas de interés referenciales activa (%)",
  ) +
  scale_x_date(
    date_breaks = "1 year",          # Mostrar etiquetas cada año
    date_labels = "%Y"              # Formato del año (e.g., 2007, 2008)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()
  
  pdf("plot_tas_activa.pdf", height = 5.5, width = 8)
  plot_tas_activa
  dev.off()
}    

# Inflación anual
{
  plot_infl_anual<- ggplot(data_clean5, aes(x = fecha, y = inflacion_anual)) +
  geom_line(color = "black") +
  theme_minimal() +
  labs(
    title = "Evolución de la inflación anual",
    x = "PERIODOS",
    y = "Inflación anual",
    caption = "Nota: Valores en escala logarítmica"
  ) +
  scale_x_date(
    date_breaks = "1 year",          # Mostrar etiquetas cada año
    date_labels = "%Y"              # Formato del año (e.g., 2007, 2008)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()
  
  pdf("plot_infl_anual.pdf", height = 5.5, width = 8)
  plot_infl_anual
  dev.off()
}

# Exportación de resultados -----------------------------------------------
# Guarda los dataframes como CSV desde cada proyecto
write.csv(data_clean3,"sector_financiero_mensual.csv")
write.csv(data_clean5,"sector_financiero_anual.csv")

