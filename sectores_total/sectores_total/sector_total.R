
# Limpieza ----------------------------------------------------------------
# Libreria
library(tidyverse)
library(readxl)
library(stargazer)
# Carga de las datas
setwd("C:/Users/USER/Documents/YRF_PROJECT/")

data_financiero_mensual <- read.csv("sectores_total/data/sector_financiero/sector_financiero_mensual.csv")

data_financiero_anual <- read.csv("sectores_total/data/sector_financiero/sector_financiero_anual.csv")

data_petrolero_mensual <- read.csv("sectores_total/data/sector_petrolero/sector_petrolero_mensual.csv")

data_petrolero_anual <- read.csv("sectores_total/data/sector_petrolero/sctor_petrolero_anual.csv")

# Eleminar primera columna
# Sector financiero mensual
data_financiero_mensual <- data_financiero_mensual %>%
  select(-X)

# Sector petrolero mensual
data_petrolero_mensual <- data_petrolero_mensual %>%
  select(-X)

# Formato de fecha para las datas
# Sector financiero mensual
data_financiero_mensual$fecha <- as.Date(data_financiero_mensual$fecha, format = "%Y-%m-%d")

# Sector petrolero mensual
data_petrolero_mensual$fecha <- as.Date(data_petrolero_mensual$fecha, format = "%Y-%m-%d")

# Verificación
str(data_financiero_mensual)
str(data_petrolero_mensual)

# Ambos dataframes desde 2007
data_petrolero_mensual <- data_petrolero_mensual %>%
  filter(fecha >= as.Date("2007-01-01"))

data_financiero_mensual <- data_financiero_mensual %>%
  filter(fecha >= as.Date("2007-01-01"))

# Solo meses
data_petrolero_mensual <- data_petrolero_mensual %>% 
  mutate(periodo = as.Date(paste0(format(fecha, "%Y-%m"), "-01")))

data_financiero_mensual <- data_financiero_mensual %>% 
  mutate(periodo = as.Date(paste0(format(fecha, "%Y-%m"), "-01")))

# Eliminar variable "fecha"
data_financiero_mensual <- data_financiero_mensual %>%
  select(-fecha)

data_petrolero_mensual <- data_petrolero_mensual %>%
  select(-fecha)

# Verificación

data_financiero_mensual$periodo == data_petrolero_mensual$periodo

str(sector_total_mensual)

colSums(is.na(sector_total_mensual))

# Join
sector_total_mensual <- left_join(data_financiero_mensual, data_petrolero_mensual, by = "periodo")

sector_total_mensual

# Renombrar
# Renombrar columnas del data frame sector_total_mensual
{
colnames(sector_total_mensual) <- c(
  "reservas_int",          # reservas_internacionales
  "pasivos_mon",           # pasivos_monetarios
  "emision_mon",           # emision_monetaria
  "dinero_elec",           # dinero_electronico
  "reservas_banc",         # reservas_bancarias
  "depositos_vista",       # depositos_a_la_vista
  "cuasidinero_total",     # cuasidinero_total
  "cuasidinero_ahorro",    # cuasidinero_ahorro
  "cuasidinero_plazo",     # cuasidinero_plazo
  "cuasidinero_restr",     # cuasidinero_restringido
  "cuasidinero_reporto",   # cuasidinero_operaciones_de_reporto
  "cuasidinero_otros",     # cuasidinero_otros_depositos
  "credito_privado_total", # credito_al_sector_privado_total
  "credito_privado_cart",  # credito_al_sector_privado_cartera
  "credito_privado_otros", # crédito_al_sector_privado_otros
  "tasa_int_basica",       # tasas_de_interes_referenciales_porcentajes_basica
  "tasa_int_pasiva",       # tasas_de_interes_referenciales_porcentajes_pasiva
  "tasa_int_activa",       # tasas_de_interes_referenciales_porcentajes_activa
  "inflacion_mensual",     # inflacion_mensual
  "inflacion_anual",       # inflacion_anual
  "inflacion_acum",        # inflacion_acumulada
  "periodo",
  "produccion_pet_crudo",    # TOTAL NACIONAL-Producción de Petróleo Crudo
  "prod_crudo_fisc", 	       # TOTAL.NACIONAL.PRODUCCIÓN.DE.PETRÓLEO.CRUDO.FISCALIZADA..Miles.de.barriles
  "exportaciones_pet_crudo", # TOTAL NACIONAL-EXPORTACIONES DE PETRÓLEO CRUDO (Miles de barriles)
  "crudo_ref_oleo",          # TOTAL.NACIONAL.CRUDO.RECIBIDO.EN.REFINERÍAS.Y.CONSUMO.EN.OLEODUCTO..Miles.de.barriles.
  "materia_prima_refin",     # TOTAL NACIONAL-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)
  "transp_oleo",             # TOTAL.NACIONAL.TRANSPORTE.POR.OLEODUCTOS..Miles.de.barriles.
  "produccion_derivados",    # TOTAL NACIONAL-PRODUCCIÓN DE DERIVADOS (Miles de barriles)
  "importacion_derivados",   # TOTAL NACIONAL-IMPORTACIÓN DE DERIVADOS (Miles de barriles)
  "consumo_int_derivados",   # TOTAL NACIONAL-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)
  "exportaciones_pet",       # TOTAL EXPORTACIONES DE PETRÓLEO (miles de barriles)
  "exp_emp_pub",             # EXPORTACIONES.DE.PETRÓLEO.DE.EMPRESAS.PÚBLICAS..miles.de.barriles.
  "exp_emp_priv",            # EXPORTACIONES.DE.PETRÓLEO.DE.COMPAÑÍAS.PRIVADAS..miles.de.barriles.
  "exp_pet_mem",             # EXPORTACIONES.DE.PETRÓLEO.DE.MEM..miles.de.barriles.
  "exportaciones_derivados", # TOTAL EXPORTACIONES DE DERIVADOS (miles de barriles)  
  "inv_exp_der",             # Exportaciones.de.Derivados..miles.de.barriles.
  "exp_petroecuador",        # Ingresos.de.EP.PETROECUADOR.por.Exportaciones..miles.de.USD.
  "saldo_ing_egre",          # DIFERENCIA.INGRESOS.Y.EGRESOS..miles.de.USD.
  "precio_WTI",              # Precio Promedio Mensual WTI
  "precio_Brent"            # Precio Promedio Mensual Brent
)
}

# Crear un nuevo data frame con las variables relevantes
{
data_1 <- sector_total_mensual %>%
  dplyr::select(
    periodo,                  # Periodo temporal para análisis temporal
    reservas_int,             # Reservas internacionales (indicador de liquidez nacional)
    depositos_vista,          # Depósitos a la vista (indicador de flujo financiero)
    cuasidinero_total,        # Cuasidinero total (activo financiero líquido)
    tasa_int_basica,          # Tasas de interés referenciales (condiciones de crédito)
    inflacion_mensual,        # Inflación mensual (estabilidad económica)
    inflacion_anual,          # Inflación anual
    produccion_pet_crudo,     # Producción nacional de petróleo crudo
    exportaciones_pet_crudo,  # Exportaciones de petróleo crudo
    consumo_int_derivados,    # Consumo interno de derivados (demanda interna energética)
    exportaciones_pet,        # Total exportaciones de petróleo
    precio_WTI,               # Precio promedio mensual del petróleo WTI
    precio_Brent              # Precio promedio mensual del Brent
  )
}
  
# Realizar las regresiones
modelo_1 <- lm(depositos_vista ~ exportaciones_pet, data = data_1)
modelo_2 <- lm(reservas_int ~ precio_WTI, data = data_1)
modelo_3 <- lm(inflacion_mensual ~ precio_WTI + precio_Brent, data = data_1)
modelo_4 <- lm(tasa_int_basica ~ produccion_pet_crudo, data = data_1)

# Exportar resultados con stargazer para LaTeX
{
stargazer(modelo_1, modelo_2, modelo_3, modelo_4, 
          title = "Resultados de Regresiones", 
          type = "latex",
          out = "resultados_regresiones.tex",
          label = "tab:resultados_regresiones",
          digits = 3,
          align = TRUE,
          column.labels = c("Depósitos Bancarios", "Reservas Int.", "Inflación", "Tasa Básica"),
          dep.var.labels.include = FALSE,
          covariate.labels = c(
            "Exportaciones de Petróleo",
            "Precio WTI",
            "Precio Brent",
            "Producción de Petróleo"
          ))
}