% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabla_dia.R
\name{tabla_dia}
\alias{tabla_dia}
\title{Tabla de homicidios por día de la semana (DIA_SEMANA) y año}
\usage{
tabla_dia(
  df,
  anio_tipo = NULL,
  desagregacion = NULL,
  geo_tipo = "ocurrencia",
  sexo = NULL,
  edad = NULL
)
}
\arguments{
\item{df}{Un data.frame que contenga las columnas \code{DIA_SEMANA}, \code{ANIO_OCUR}, \code{ANIO_REGIS}, \code{SEXO} y claves geográficas.}

\item{anio_tipo}{Tipo de año a usar: \code{"registro"} o \code{"ocurrencia"} (por defecto \code{"ocurrencia"}).}

\item{desagregacion}{Nivel geográfico adicional: \code{"entidad"}, \code{"municipio"} o \code{NULL}.}

\item{geo_tipo}{Fuente del dato geográfico: \code{"registro"} o \code{"ocurrencia"} (por defecto \code{"ocurrencia"}).}

\item{sexo}{Filtro opcional por sexo: \code{"hombres"}, \code{"mujeres"} o \code{NULL}.}

\item{edad}{Vector numérico opcional con una o más edades (entre 0 y 99) que se desean filtrar.
Por ejemplo: \code{edad = 25}, \code{edad = c(15:19)} o \code{edad = c(0:17, 24, 50)}.
Si se deja como \code{NULL} (por defecto), se incluyen todas las edades.}
}
\value{
Un data.frame con columnas \code{ANIO_XXX}, claves geográficas (si aplica) y columnas \code{Dia_1}, ..., \code{Dia_7}.
}
\description{
Devuelve una tabla con la distribución de homicidios por día de la semana,
con opción de desagregación por año, geografía y sexo.
}
