# Limpieza de datos -------------------------------------------------------
{
library(readxl)
library(tidyverse)
library(lubridate)
library(psych)
library(stargazer)
} # libreria

setwd("C:/Users/USER/Documents/YRF_PROJECT/")

data_inicial = read_excel("sector_petrolero/data/name_variable/SerieCifrasPetroleras_2_v.xlsx", skip=1)

{
data_1 = data_inicial %>%
  filter(`...1` == "TOTAL NACIONAL-Producción de Petróleo Crudo" | 
           #`...1` == "- Producción Promedio Diaria-Petróleo Crudo Nacional" | 
           #`...1` == "- Tasa de crecimiento anual (t / t-12) (1)-Petróleo Crudo Nacional" | 
           #`...1` == "- Tasa de crecimiento mensual (t / t-1) (1)-Petróleo Crudo Nacional" | 
           #`...1` == "EMPRESAS PÚBLICAS-Producción de Petróleo Crudo" | 
           #`...1` == "EP Petroecuador (2)" | 
           #`...1` == "Petroamazonas EP(3)(4)(5)(6)" | 
           #`...1` == "Operadora Río Napo (Sacha)(4)" | 
           #`...1` == "- Empresas Públicas Producción Promedio Diario" | 
           #`...1` == "- EP Petroecuador Producción Promedio Diario" | 
           #`...1` == "- Petroamazonas EP Producción Promedio Diario" | 
           #`...1` == "- Operad. Río Napo Producción Promedio Diario" | 
           #`...1` == "- Crudo recuperado Amazonía Viva (7)" | 
           #`...1` == "COMPAÑÍAS PRIVADAS*-Producción de Petróleo Crudo" | 
           #`...1` == "- Producción Promedio Diaria-Compañías Privadas" | 
           `...1` == "TOTAL NACIONAL-PRODUCCIÓN DE PETRÓLEO CRUDO FISCALIZADA (Miles de barriles)" | 
           #`...1` == "- Producción Promedio Diaria-PRODUCCIÓN DE PETRÓLEO CRUDO FISCALIZADA (Miles de barriles)" | 
           #`...1` == "EMPRESAS PÚBLICAS-PRODUCCIÓN DE PETRÓLEO CRUDO FISCALIZADA (Miles de barriles)" | 
           #`...1` == "- Producción Promedio Diaria-PRODUCCIÓN DE PETRÓLEO CRUDO FISCALIZADA (Miles de barriles)" | 
           #`...1` == "Petroproducción" | 
           #`...1` == "EP Petroecuador (8)" | 
           #`...1` == "EP Petroecuador (9)" | 
           #`...1` == "EP Petroecuador Bloque 1 (10)" | 
           #`...1` == "Bloque 27 (EX - CITY)" | 
           #`...1` == "Operad. Río Napo" | 
           #`...1` == "Petroamazonas EP Bloque 60" | 
           #`...1` == "Consumo de crudo refinería Shushufindi" | 
           #`...1` == "Consumo de crudo planta destiladora Lago Agrio" | 
           #`...1` == "COMPAÑÍAS PRIVADAS-PRODUCCIÓN DE PETRÓLEO CRUDO FISCALIZADA (Miles de barriles)" | 
           #`...1` == "- Producción Promedio Diaria-PRODUCCIÓN DE PETRÓLEO CRUDO FISCALIZADA (Miles de barriles)" | 
           `...1` == "TOTAL NACIONAL-EXPORTACIONES DE PETRÓLEO CRUDO (Miles de barriles)" | 
           #`...1` == "EMPRESAS PÚBLICAS-EXPORTACIONES DE PETRÓLEO CRUDO (Miles de barriles)" | 
           #`...1` == "Crudo Oriente (11)" | 
           #`...1` == "Crudo Napo (12)" | 
           #`...1` == "CÍAS. PRIVADAS**" | 
           #`...1` == "MINISTERIO DE ENERGÍA Y MINAS (MEM)" | 
           `...1` == "TOTAL NACIONAL-CRUDO RECIBIDO EN REFINERÍAS Y CONSUMO EN OLEODUCTO (Miles de barriles)" | 
           #`...1` == "- Promedio Diario Recibido" | 
           #`...1` == "Refinería Esmeraldas" | 
           #`...1` == "Refinería Libertad" | 
           #`...1` == "Refinería Shushufindi" | 
           #`...1` == "Otros (13)" | 
           `...1` == "TOTAL NACIONAL-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)" | 
           #`...1` == "- Consumo Promedio Diario-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)" | 
           #`...1` == "Refinería Esmeraldas-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)" | 
           #`...1` == "Refinería Libertad-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)" | 
           #`...1` == "Refinería Shushufindi -MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)" | 
           #`...1` == "Refinería Lago Agrio (14)-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)" | 
           `...1` == "TOTAL NACIONAL-TRANSPORTE POR OLEODUCTOS (Miles de barriles)" | 
           #`...1` == "- Transporte Promedio Diario-TRANSPORTE POR OLEODUCTOS (Miles de barriles)" | 
           #`...1` == "SOTE (11)-TRANSPORTE POR OLEODUCTOS (Miles de barriles)" | 
           #`...1` == "OCP (12)-TRANSPORTE POR OLEODUCTOS (Miles de barriles)" | 
           `...1` == "TOTAL NACIONAL-PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "- Producción Promedio Diaria-PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Gasolina Super-PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Gasolina Extra-PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Extra con Ethanol (ECOPAÍS)-PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Ecoplus (89 RON)-PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Diésel -PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Fuel Oil # 4-PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Fuel Oil # 6-PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Gas Licuado de Petróleo-PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Fuel Oil  # 6 exportación -PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Otros (15)-PRODUCCIÓN DE DERIVADOS (Miles de barriles)" | 
           `...1` == "TOTAL NACIONAL-IMPORTACIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Nafta de Alto Octano (16)-IMPORTACIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Diésel-IMPORTACIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Gas Licuado de Petróleo-IMPORTACIÓN DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Otros (17)-IMPORTACIÓN DE DERIVADOS (Miles de barriles)" | 
           `...1` == "TOTAL NACIONAL-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "- Consumo Promedio Diario-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Gasolina Super-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Gasolina Extra-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Extra con Ethanol (ECOPAÍS)-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Ecoplus (89 RON)-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Super Preminun-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Diésel-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Fuel Oil-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Gas Licuado de Petróleo-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           #`...1` == "Otros (18)-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)" | 
           `...1` == "TOTAL EXPORTACIONES DE PETRÓLEO (miles de barriles)" | 
           #`...1` == "Precio (USD por barril) (19)-EXPORTACIONES DE PETRÓLEO CRUDO" | 
           #`...1` == "Ingreso por exportaciones de petróleo (miles de USD)" | 
           `...1` == "EXPORTACIONES DE PETRÓLEO DE EMPRESAS PÚBLICAS (miles de barriles)" | 
           #`...1` == "Precio (USD por barril)-EXPORTACIONES DE PETRÓLEO CRUDO DE EMPRESAS PÚBLICAS (EP)" | 
           #`...1` == "Ingreso por exportaciones de petróleo de Empresas Públicas (miles de USD)" | 
           #`...1` == "- Exportación Total de las Empresas Públicas (miles de barriles)/ Días Mes (20)" | 
           #`...1` == "Exportaciones Crudo Oriente Ventas Directas (miles de barriles)" | 
           #`...1` == "Exportaciones Crudo Oriente por Regalías (miles de barriles)" | 
           #`...1` == "Exportaciones Crudo Napo Bloque 15 (miles de barriles)" | 
           #`...1` == "Exportaciones Crudo Napo Ventas Directas (miles de barriles)" | 
           #`...1` == "Exportaciones Crudo Napo por Regalías (miles de barriles)" | 
           #`...1` == "Precio Crudo Oriente (USD por barril)" | 
           #`...1` == "Precio Crudo Napo (USD por barril)" | 
           `...1` == "EXPORTACIONES DE PETRÓLEO DE COMPAÑÍAS PRIVADAS (miles de barriles)" | 
           #`...1` == "Precio (dólares por barril)-EXPORTACIONES DE PETRÓLEO DE COMPAÑÍAS PRIVADAS (miles de barriles)" | 
           #`...1` == "Ingreso por exportaciones de petróleo de Compañías Privadas (miles de dólares)" | 
           `...1` == "EXPORTACIONES DE PETRÓLEO DE MEM (miles de barriles)" | 
           #`...1` == "Precio (USD por barril)-EXPORT. DE PETRÓLEO CRUDO DEL MINIST. DE ENERGÍA Y MINAS (MEM)(21)" | 
           #`...1` == "Ingreso por exportaciones de petróleo de MEM (miles de USD)" | 
           `...1` == "TOTAL EXPORTACIONES DE DERIVADOS (miles de barriles)" | 
           #`...1` == "Ingreso por Exportaciones de Derivados (miles de USD)" | 
           #`...1` == "Exportaciones de Fuel oil # 6 (miles de barriles)" | 
           #`...1` == "Precio (USD por barril)-Exportaciones de Fuel oil # 6 (miles de barriles)" | 
           #`...1` == "Exportaciones de Fuel oil #4 (miles de barriles)" | 
           #`...1` == "Precio (dólares por barril)-Exportaciones de Fuel oil #4 (miles de barriles)" | 
           #`...1` == "Exportaciones de Gasóleo (VGO)(miles de barriles)" | 
           #`...1` == "Precio (dólares por barril)-Exportaciones de Gasóleo (VGO)(miles de barriles)" | 
           #`...1` == "Exportaciones de Nafta Bajo Octano (miles de barriles)" | 
           #`...1` == "Precio (USD por barril)-Exportaciones de Nafta Bajo Octano (miles de barriles)" | 
           `...1` == "Exportaciones de Derivados (miles de barriles)" | 
           #`...1` == "Precio (USD por barril)-EXPORTACIONES DE DERIVADOS DE LAS COMPAÑÍAS PRIVADAS" | 
           #`...1` == "Ingreso por Exportaciones de Derivados (miles de USD)" | 
           `...1` == "Ingresos de EP PETROECUADOR por Exportaciones (miles de USD)" | 
           #`...1` == "Ingreso por exportaciones de petróleo de Empresas Públicas (miles de USD)" | 
           #`...1` == "Ingreso por exportaciones de derivados de Empresas Públicas (miles de USD)" | 
           `...1` == "DIFERENCIA INGRESOS Y EGRESOS (miles de USD)" | 
           #`...1` == "Costos Totales Importaciones (miles de USD)" | 
           #`...1` == "Ingresos Totales Ventas Internas (miles de USD)" | 
           #`...1` == "Nafta Alto Octano" | 
           #`...1` == "Diferencia Ingreso y Costo (miles de USD)-Nafta Alto Octano" | 
           #`...1` == "Volumen Importado (miles de barriles)-Nafta Alto Octano" | 
           #`...1` == "Precio Importación (USD por barril)-Nafta Alto Octano" | 
           #`...1` == "Costo Importación (miles de USD)-Nafta Alto Octano" | 
           #`...1` == "Precio Venta Interna (USD por barril)-Nafta Alto Octano" | 
           #`...1` == "Ingreso Venta Interna (miles de USD)-Nafta Alto Octano" | 
           #`...1` == "Diésel" | 
           #`...1` == "Diferencia Ingreso y Costo (miles de USD)-Diésel" | 
           #`...1` == "Volumen Importado (miles de barriles)-Diésel" | 
           #`...1` == "Precio Importación (USD por barril)-Diésel" | 
           #`...1` == "Costo Importación (miles de USD)-Diésel" | 
           #`...1` == "Precio Venta Interna (USD por barril)-Diésel" | 
           #`...1` == "Ingreso Venta Interna (miles de USD)-Diésel" | 
           #`...1` == "Gas Licuado de Petróleo" | 
           #`...1` == "Diferencia Ingreso y Costo (miles de USD)-Gas Licuado de Petróleo" | 
           #`...1` == "Volumen Importado (miles de barriles)-Gas Licuado de Petróleo" | 
           #`...1` == "Precio Importación (USD por barril)-Gas Licuado de Petróleo" | 
           #`...1` == "Costo Importación (miles de USD)-Gas Licuado de Petróleo" | 
           #`...1` == "Precio Venta Interna (USD por barril)-Gas Licuado de Petróleo" | 
           #`...1` == "Ingreso Venta Interna (miles de USD)-Gas Licuado de Petróleo" | 
           `...1` == "Precio Promedio Mensual WTI" | 
           `...1` == "Precio Promedio Mensual Brent"
           )
} # Nombre de las variables

colnames(data_1) = c("Indicador", "2007_01", "2007_02", "2007_03", "2007_04", "2007_05", "2007_06", "2007_07", "2007_08", "2007_09", "2007_10", "2007_11", "2007_12", "2008_01", "2008_02", "2008_03", "2008_04", "2008_05", "2008_06", "2008_07", "2008_08", "2008_09", "2008_10", "2008_11", "2008_12", "2009_01", "2009_02", "2009_03", "2009_04", "2009_05", "2009_06", "2009_07", "2009_08", "2009_09", "2009_10", "2009_11", "2009_12", "2010_01", "2010_02", "2010_03", "2010_04", "2010_05", "2010_06", "2010_07", "2010_08", "2010_09", "2010_10", "2010_11", "2010_12", "2011_01", "2011_02", "2011_03", "2011_04", "2011_05", "2011_06", "2011_07", "2011_08", "2011_09", "2011_10", "2011_11", "2011_12", "2012_01", "2012_02", "2012_03", "2012_04", "2012_05", "2012_06", "2012_07", "2012_08", "2012_09", "2012_10", "2012_11", "2012_12", "2013_01", "2013_02", "2013_03", "2013_04", "2013_05", "2013_06", "2013_07", "2013_08", "2013_09", "2013_10", "2013_11", "2013_12", "2014_01", "2014_02", "2014_03", "2014_04", "2014_05", "2014_06", "2014_07", "2014_08", "2014_09", "2014_10", "2014_11", "2014_12", "2015_01", "2015_02", "2015_03", "2015_04", "2015_05", "2015_06", "2015_07", "2015_08", "2015_09", "2015_10", "2015_11", "2015_12", "2016_01", "2016_02", "2016_03", "2016_04", "2016_05", "2016_06", "2016_07", "2016_08", "2016_09", "2016_10", "2016_11", "2016_12", "2017_01", "2017_02", "2017_03", "2017_04", "2017_05", "2017_06", "2017_07", "2017_08", "2017_09", "2017_10", "2017_11", "2017_12", "2018_01", "2018_02", "2018_03", "2018_04", "2018_05", "2018_06", "2018_07", "2018_08", "2018_09", "2018_10", "2018_11", "2018_12", "2019_01", "2019_02", "2019_03", "2019_04", "2019_05", "2019_06", "2019_07", "2019_08", "2019_09", "2019_10", "2019_11", "2019_12", "2020_01", "2020_02", "2020_03", "2020_04", "2020_05", "2020_06", "2020_07", "2020_08", "2020_09", "2020_10", "2020_11", "2020_12", "2021_01", "2021_02", "2021_03", "2021_04", "2021_05", "2021_06", "2021_07", "2021_08", "2021_09", "2021_10", "2021_11", "2021_12", "2022_01", "2022_02", "2022_03", "2022_04", "2022_05", "2022_06", "2022_07", "2022_08", "2022_09", "2022_10", "2022_11", "2022_12", "2023_01", "2023_02", "2023_03", "2023_04", "2023_05", "2023_06", "2023_07", "2023_08", "2023_09", "2023_10", "2023_11", "2023_12", "2024_01", "2024_02", "2024_03", "2024_04", "2024_05", "2024_06", "2024_07","2024_08")

data_2 = select(data_1, "Indicador", "2007_01", "2007_02", "2007_03", "2007_04", "2007_05", "2007_06", "2007_07", "2007_08", "2007_09", "2007_10", "2007_11", "2007_12", "2008_01", "2008_02", "2008_03", "2008_04", "2008_05", "2008_06", "2008_07", "2008_08", "2008_09", "2008_10", "2008_11", "2008_12", "2009_01", "2009_02", "2009_03", "2009_04", "2009_05", "2009_06", "2009_07", "2009_08", "2009_09", "2009_10", "2009_11", "2009_12", "2010_01", "2010_02", "2010_03", "2010_04", "2010_05", "2010_06", "2010_07", "2010_08", "2010_09", "2010_10", "2010_11", "2010_12", "2011_01", "2011_02", "2011_03", "2011_04", "2011_05", "2011_06", "2011_07", "2011_08", "2011_09", "2011_10", "2011_11", "2011_12", "2012_01", "2012_02", "2012_03", "2012_04", "2012_05", "2012_06", "2012_07", "2012_08", "2012_09", "2012_10", "2012_11", "2012_12", "2013_01", "2013_02", "2013_03", "2013_04", "2013_05", "2013_06", "2013_07", "2013_08", "2013_09", "2013_10", "2013_11", "2013_12", "2014_01", "2014_02", "2014_03", "2014_04", "2014_05", "2014_06", "2014_07", "2014_08", "2014_09", "2014_10", "2014_11", "2014_12", "2015_01", "2015_02", "2015_03", "2015_04", "2015_05", "2015_06", "2015_07", "2015_08", "2015_09", "2015_10", "2015_11", "2015_12", "2016_01", "2016_02", "2016_03", "2016_04", "2016_05", "2016_06", "2016_07", "2016_08", "2016_09", "2016_10", "2016_11", "2016_12", "2017_01", "2017_02", "2017_03", "2017_04", "2017_05", "2017_06", "2017_07", "2017_08", "2017_09", "2017_10", "2017_11", "2017_12", "2018_01", "2018_02", "2018_03", "2018_04", "2018_05", "2018_06", "2018_07", "2018_08", "2018_09", "2018_10", "2018_11", "2018_12", "2019_01", "2019_02", "2019_03", "2019_04", "2019_05", "2019_06", "2019_07", "2019_08", "2019_09", "2019_10", "2019_11", "2019_12", "2020_01", "2020_02", "2020_03", "2020_04", "2020_05", "2020_06", "2020_07", "2020_08", "2020_09", "2020_10", "2020_11", "2020_12", "2021_01", "2021_02", "2021_03", "2021_04", "2021_05", "2021_06", "2021_07", "2021_08", "2021_09", "2021_10", "2021_11", "2021_12", "2022_01", "2022_02", "2022_03", "2022_04", "2022_05", "2022_06", "2022_07", "2022_08", "2022_09", "2022_10", "2022_11", "2022_12", "2023_01", "2023_02", "2023_03", "2023_04", "2023_05", "2023_06", "2023_07", "2023_08", "2023_09", "2023_10", "2023_11", "2023_12", "2024_01", "2024_02", "2024_03", "2024_04", "2024_05", "2024_06", "2024_07","2024_08")

data_2 %>% 
  count(Indicador, sort = TRUE)

{
data_3 = data_2 %>% 
  pivot_longer(
    cols = c("2007_01", "2007_02", "2007_03", "2007_04", "2007_05", "2007_06", 
             "2007_07", "2007_08", "2007_09", "2007_10", "2007_11", "2007_12", 
             "2008_01", "2008_02", "2008_03", "2008_04", "2008_05", "2008_06", 
             "2008_07", "2008_08", "2008_09", "2008_10", "2008_11", "2008_12", 
             "2009_01", "2009_02", "2009_03", "2009_04", "2009_05", "2009_06", 
             "2009_07", "2009_08", "2009_09", "2009_10", "2009_11", "2009_12", 
             "2010_01", "2010_02", "2010_03", "2010_04", "2010_05", "2010_06", 
             "2010_07", "2010_08", "2010_09", "2010_10", "2010_11", "2010_12", 
             "2011_01", "2011_02", "2011_03", "2011_04", "2011_05", "2011_06", 
             "2011_07", "2011_08", "2011_09", "2011_10", "2011_11", "2011_12", 
             "2012_01", "2012_02", "2012_03", "2012_04", "2012_05", "2012_06", 
             "2012_07", "2012_08", "2012_09", "2012_10", "2012_11", "2012_12", 
             "2013_01", "2013_02", "2013_03", "2013_04", "2013_05", "2013_06", 
             "2013_07", "2013_08", "2013_09", "2013_10", "2013_11", "2013_12", 
             "2014_01", "2014_02", "2014_03", "2014_04", "2014_05", "2014_06", 
             "2014_07", "2014_08", "2014_09", "2014_10", "2014_11", "2014_12", 
             "2015_01", "2015_02", "2015_03", "2015_04", "2015_05", "2015_06", 
             "2015_07", "2015_08", "2015_09", "2015_10", "2015_11", "2015_12", 
             "2016_01", "2016_02", "2016_03", "2016_04", "2016_05", "2016_06", 
             "2016_07", "2016_08", "2016_09", "2016_10", "2016_11", "2016_12", 
             "2017_01", "2017_02", "2017_03", "2017_04", "2017_05", "2017_06", 
             "2017_07", "2017_08", "2017_09", "2017_10", "2017_11", "2017_12", 
             "2018_01", "2018_02", "2018_03", "2018_04", "2018_05", "2018_06", 
             "2018_07", "2018_08", "2018_09", "2018_10", "2018_11", "2018_12", 
             "2019_01", "2019_02", "2019_03", "2019_04", "2019_05", "2019_06", 
             "2019_07", "2019_08", "2019_09", "2019_10", "2019_11", "2019_12", 
             "2020_01", "2020_02", "2020_03", "2020_04", "2020_05", "2020_06", 
             "2020_07", "2020_08", "2020_09", "2020_10", "2020_11", "2020_12", 
             "2021_01", "2021_02", "2021_03", "2021_04", "2021_05", "2021_06", 
             "2021_07", "2021_08", "2021_09", "2021_10", "2021_11", "2021_12", 
             "2022_01", "2022_02", "2022_03", "2022_04", "2022_05", "2022_06", 
             "2022_07", "2022_08", "2022_09", "2022_10", "2022_11", "2022_12", 
             "2023_01", "2023_02", "2023_03", "2023_04", "2023_05", "2023_06", 
             "2023_07", "2023_08", "2023_09", "2023_10", "2023_11", "2023_12", 
             "2024_01", "2024_02", "2024_03", "2024_04", "2024_05", "2024_06",
             "2024_07","2024_08"),
    names_to = "date", 
    values_to = "value"
  )
} # Formato de fechas

data_4 = data_3 %>%
  pivot_wider(id_cols = c("date"), names_from = "Indicador", values_from = c("value"))
# Convertir valores de caracteres a numericos y fechas en formato de fechas
# Cambiar formato de fechas

data_5 = data_4 %>%
  mutate(fecha = ymd(paste(date, "01", sep = "_"))) %>%
  select(fecha,everything()) %>%
  select(-date)

# Cambiar formato valores
data_6= data_5%>%
  mutate(across(where(is.character), as.numeric))

data_6$fecha <- as.Date(data_6$fecha, format = "%Y-%m-%d")

# Rango de fechas
data_7 <- subset(data_6, fecha >= as.Date("2007-01-01") & fecha <= as.Date("2024-08-01"))

# Casos NA
{
data_7 = data_6 %>% 
  replace_na(list(n = 0))

data_8 = data_7 %>% 
  mutate_all(~replace_na(., 0))
}
colSums(is.na(data_6))

  # Estadística descriptiva -------------------------------------------------------
# TOTAL NACIONAL-Producción de Petróleo Crudo 
stargazer(data_6$`TOTAL NACIONAL-Producción de Petróleo Crudo`, summary = T,out = "TN_prod_crudo.tex")

# TOTAL NACIONAL-EXPORTACIONES DE PETRÓLEO CRUDO (Miles de barriles) 
stargazer(data_6$`TOTAL NACIONAL-EXPORTACIONES DE PETRÓLEO CRUDO (Miles de barriles)`, summary = T,out = "TN_exp_crudo.tex")

# TOTAL NACIONAL-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles) 
stargazer(data_6$`TOTAL NACIONAL-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)`, summary = T,out = "TN_mp_ref.tex")

# TOTAL NACIONAL-PRODUCCIÓN DE DERIVADOS (Miles de barriles) 
stargazer(data_6$`TOTAL NACIONAL-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)`, summary = T,out = "TN_prod_derv.tex")

# TOTAL NACIONAL-IMPORTACIÓN DE DERIVADOS (Miles de barriles) imp_derv
stargazer(data_6$`TOTAL NACIONAL-IMPORTACIÓN DE DERIVADOS (Miles de barriles)`, summary = T,out = "TN_imp_derv.tex")

# TOTAL NACIONAL-CONSUMO INTERNO DE DERIVADOS (Miles de barriles) 
stargazer(data_6$`TOTAL NACIONAL-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)`, summary = T,out = "TN_cons_derv.tex")

# TOTAL EXPORTACIONES DE PETRÓLEO (miles de barriles) 
stargazer(data_6$`TOTAL EXPORTACIONES DE PETRÓLEO (miles de barriles)`, summary = T,out = "TN_exp_petro.tex")

# TOTAL EXPORTACIONES DE DERIVADOS (miles de barriles)  
stargazer(data_6$`TOTAL EXPORTACIONES DE DERIVADOS (miles de barriles)`, summary = T,out = "TN_exp_derv.tex")

# Precio Promedio Mensual WTI  
stargazer(data_6$`Precio Promedio Mensual WTI`, summary = T,out = "p_wti.tex")

# Precio Promedio Mensual Brent 
stargazer(data_6$`Precio Promedio Mensual Brent`, summary = T,out = "p_brent.tex")

# Gráfica -----------------------------------------------------------------
{
ggplot(data_6, aes(x = fecha)) +
  geom_line(aes(y = `TOTAL NACIONAL-Producción de Petróleo Crudo`, color = "Producción de Petróleo Crudo"), size = 1) +
  labs(
    title = "Evolución de la Producción de Petróleo Crudo en Ecuador (2007 - 2024)",
    x = "Fecha",
    y = "Producción de Petróleo Crudo (barriles)",
    color = "",
    ) +
  scale_color_manual(
    values = c("Producción de Petróleo Crudo" = "blue", "Datos Puntuales" = "red")
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    limits = c(as.Date("2007-01-01"), as.Date("2024-06-30"))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom",  # Coloca la leyenda debajo del gráfico
    legend.title = element_text(face = "bold")  # Resalta el título de la leyenda
  )
}

# TOTAL NACIONAL-Producción de Petróleo Crudo 
{
mean_1<-mean(data_6$`TOTAL NACIONAL-Producción de Petróleo Crudo`)/1000
plot_prod_petroleao <- ggplot(data_6, aes(x = fecha, y = `TOTAL NACIONAL-Producción de Petróleo Crudo` / 1000)) +
  geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
  geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
  theme_minimal() +
  labs(
    title = "TOTAL NACIONAL-Producción de Petróleo Crudo",
    x = "PERIODO",
    y = "Total Nacional (En miles de barriles)",
    caption = "Fuente: Banco Central del Ecuador"
  ) +
  theme_classic() +
  # Agregar líneas horizontales en valores específicos
  geom_hline(yintercept = c(15.4571), linetype = "dashed", color = "red")

pdf("plot_prod_petroleao.pdf", height = 5.5, width = 8)
plot_prod_petroleao
dev.off()
}  

# TOTAL NACIONAL-EXPORTACIONES DE PETRÓLEO CRUDO (Miles de barriles) 
{
mean_2<-mean(data_6$`TOTAL NACIONAL-EXPORTACIONES DE PETRÓLEO CRUDO (Miles de barriles)`)/1000
plot_exp_petroleao <- ggplot(data_6, aes(x = fecha, y = `TOTAL NACIONAL-EXPORTACIONES DE PETRÓLEO CRUDO (Miles de barriles)` / 1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "TOTAL NACIONAL-EXPORTACIONES DE PETRÓLEO CRUDO (Miles de barriles)",
      x = "PERIODO",
      y = "Total Nacional (En miles de barriles)",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(10.88419), linetype = "dashed", color = "red")
  
pdf("plot_exp_petroleao.pdf", height = 5.5, width = 8)
plot_exp_petroleao
dev.off()  
}  
  
# TOTAL NACIONAL-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles) 
{
mean_3<-mean(data_6$`TOTAL NACIONAL-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)`)/1000
plot_prima_refinada <- ggplot(data_6, aes(x = fecha, y = `TOTAL NACIONAL-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)` / 1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "TOTAL NACIONAL-MATERIA PRIMA PROCESADA EN REFINERÍAS (Miles de barriles)",
      x = "PERIODO",
      y = "Total Nacional (En miles de barriles)",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(4.400424), linetype = "dashed", color = "red")
  
pdf("plot_prima_refinada.pdf", height = 5.5, width = 8)
plot_prima_refinada
dev.off()  
}  
  
# TOTAL NACIONAL-PRODUCCIÓN DE DERIVADOS (Miles de barriles) 
{
mean_4<-mean(data_6$`TOTAL NACIONAL-PRODUCCIÓN DE DERIVADOS (Miles de barriles)`)/1000
plot_prod_deriv <- ggplot(data_6, aes(x = fecha, y = `TOTAL NACIONAL-PRODUCCIÓN DE DERIVADOS (Miles de barriles)` / 1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "TOTAL NACIONAL-PRODUCCIÓN DE DERIVADOS (Miles de barriles)",
      x = "PERIODO",
      y = "Total Nacional (En miles de barriles)",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(5.912452), linetype = "dashed", color = "red")
  
pdf("plot_prod_deriv.pdf", height = 5.5, width = 8)
plot_prod_deriv
dev.off()    
}

# TOTAL NACIONAL-IMPORTACIÓN DE DERIVADOS (Miles de barriles) 
{
mean_5<-mean(data_6$`TOTAL NACIONAL-IMPORTACIÓN DE DERIVADOS (Miles de barriles)`)/1000
plot_imp_deriv <- ggplot(data_6, aes(x = fecha, y = `TOTAL NACIONAL-IMPORTACIÓN DE DERIVADOS (Miles de barriles)` / 1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "TOTAL NACIONAL-IMPORTACIÓN DE DERIVADOS (Miles de barriles)",
      x = "PERIODO",
      y = "Total Nacional (En miles de barriles)",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(4.125026), linetype = "dashed", color = "red")
  
pdf("plot_imp_deriv.pdf", height = 5.5, width = 8)
plot_imp_deriv
dev.off()   
}  

# TOTAL NACIONAL-CONSUMO INTERNO DE DERIVADOS (Miles de barriles) 
{
mean_6<-mean(data_6$`TOTAL NACIONAL-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)`)/1000
plot_cons_inter <- ggplot(data_6, aes(x = fecha, y = `TOTAL NACIONAL-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)` / 1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "TOTAL NACIONAL-CONSUMO INTERNO DE DERIVADOS (Miles de barriles)",
      x = "PERIODO",
      y = "Total Nacional (En miles de barriles)",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(7.231809), linetype = "dashed", color = "red")
  
pdf("plot_cons_inter.pdf", height = 5.5, width = 8)
plot_cons_inter
dev.off()     
}

# TOTAL EXPORTACIONES DE PETRÓLEO (miles de barriles) 
{
mean_7<-mean(data_6$`TOTAL EXPORTACIONES DE PETRÓLEO (miles de barriles)`)/1000
plot_exp_petro <- ggplot(data_6, aes(x = fecha, y = `TOTAL EXPORTACIONES DE PETRÓLEO (miles de barriles)` / 1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "TOTAL EXPORTACIONES DE PETRÓLEO (miles de barriles) ",
      x = "PERIODO",
      y = "Total Nacional (En miles de barriles)",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(10.88419), linetype = "dashed", color = "red")
  
pdf("plot_exp_petro.pdf", height = 5.5, width = 8)
plot_exp_petro
dev.off()  
}  
  
# TOTAL EXPORTACIONES DE DERIVADOS (miles de barriles) 
{
mean_8<-mean(data_6$`TOTAL EXPORTACIONES DE DERIVADOS (miles de barriles)`)/1000
plot_exp_deriv <- ggplot(data_6, aes(x = fecha, y = `TOTAL EXPORTACIONES DE DERIVADOS (miles de barriles)` / 1000)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
    geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
    theme_minimal() +
    labs(
      title = "TOTAL EXPORTACIONES DE DERIVADOS (miles de barriles) ",
      x = "PERIODO",
      y = "Total Nacional (En miles de barriles)",
      caption = "Fuente: Banco Central del Ecuador"
    ) +
    theme_classic() +
    # Agregar líneas horizontales en valores específicos
    geom_hline(yintercept = c(0.9871987), linetype = "dashed", color = "red")
  
pdf("plot_exp_deriv.pdf", height = 5.5, width = 8)
plot_exp_deriv
dev.off()  
}

# Precio Promedio Mensual WTI 
{
mean_9<-mean(data_6$`Precio Promedio Mensual WTI`)
plot_prec_WTI <- ggplot(data_6, aes(x = fecha, y = `Precio Promedio Mensual WTI`)) +
  geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
  geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
  theme_minimal() +
  labs(
    title = "Precio Promedio Mensual WTI ",
    x = "PERIODO",
    y = "Precio Promedio Mensual WTI ",
    caption = "Fuente: Banco Central del Ecuador"
  ) +
  theme_classic() +
  # Agregar líneas horizontales en valores específicos
  geom_hline(yintercept = c(73.01302), linetype = "dashed", color = "red")

pdf("plot_prec_WTI.pdf", height = 5.5, width = 8)
plot_prec_WTI
dev.off()  
}

# Precio Promedio Mensual Brent 
{
mean_10<-mean(data_6$`Precio Promedio Mensual Brent`)
plot_prec_brent <- ggplot(data_6, aes(x = fecha, y = `Precio Promedio Mensual Brent`)) +
                            geom_area(fill = "lightblue", alpha = 0.5) +  # Sombrea la zona debajo de la curva con color y transparencia
                            geom_line(color = "#00008B") +  # Dibuja la línea sobre el área sombreada
                            theme_minimal() +
                            labs(
                              title = "Precio Promedio Mensual Brent ",
                              x = "PERIODO",
                              y = "Precio Promedio Mensual Brent",
                              caption = "Fuente: Banco Central del Ecuador"
                            ) +
                            theme_classic() +
                            # Agregar líneas horizontales en valores específicos
                            geom_hline(yintercept = c(78.00485), linetype = "dashed", color = "red")
                          
pdf("plot_prec_brent.pdf", height = 5.5, width = 8)
plot_prec_brent
dev.off()   
}