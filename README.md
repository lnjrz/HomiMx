
# homiMx <img src="https://img.shields.io/badge/INEGI-based-blue" align="right" height="30"/>

`homiMx` es un paquete en R para el análisis de **homicidios registrados
en México** a partir de los microdatos de defunciones del INEGI
(1990–2023).

Integra una base de datos estandarizada de defunciones por agresión
(homicidios), y proporciona funciones que permiten obtener tablas
desagregadas por:

- Año de ocurrencia o registro  
- Sexo  
- Entidad y municipio
- Edad
- Causa de la defunción, ocupación, educación, estado civil, lugar, mes,
  día y hora .

------------------------------------------------------------------------

## 🚀 Instalación

``` r
# Instala desde GitHub con:
# install.packages("devtools")
devtools::install_github("lnjrz/homiMx")
```

## 📦 Datos incluidos

**homidata**: Base unificada de homicidios 1990–2023

**cat_entidades**: Catálogo de claves y nombres de entidades

**cat_municipios**: Catálogo de claves y nombres de municipios

``` r
library(homiMx)

# Total de homicidios por año (ocurrencia)
table_total(homidata)

# Total por entidad, año de registro
table_total(homidata, anio_tipo = "registro", desagregacion = "entidad")

# Homicidios por tipo de causa y sexo
table_causa(homidata, sexo = "hombres")

# Consulte la viñeta de funcionamiento para ver más ejemplos
browseVignettes("homiMx")
```

## 📚 Fuente de datos

Todos los datos provienen de los microdatos de mortalidad del INEGI,
filtrando solo los registros clasificados como agresiones (homicidios),
con claves equivalentes en la CIE-9 y CIE-10. La actualización se hará
año con año para incorporar la información más reciente.

***En desarrollo***

Este paquete se encuentra en desarrollo activo. La versión 1.1 incluye
solo funciones tabulares. Las funciones de visualización estarán
disponibles en la versión 2.0.
