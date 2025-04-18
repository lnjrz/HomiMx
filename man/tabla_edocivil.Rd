% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabla_edocivil.R
\name{tabla_edocivil}
\alias{tabla_edocivil}
\title{Tabla de estado civil (CAT_CIVIL) por año}
\usage{
tabla_edocivil(
  df,
  anio_tipo = NULL,
  desagregacion = NULL,
  geo_tipo = "ocurrencia",
  sexo = NULL,
  edad = NULL
)
}
\arguments{
\item{df}{Un data.frame que contenga las columnas \code{CAT_CIVIL}, \code{ANIO_OCUR}, \code{ANIO_REGIS}, \code{SEXO} y claves geográficas.}

\item{anio_tipo}{\code{"registro"}, \code{"ocurrencia"} o \code{NULL} (usa \code{"ocurrencia"} por defecto).}

\item{desagregacion}{\code{NULL}, \code{"entidad"} o \code{"municipio"}.}

\item{geo_tipo}{\code{"registro"} o \code{"ocurrencia"} (por defecto \code{"ocurrencia"}).}

\item{sexo}{\code{NULL}, \code{"hombres"}, \code{"mujeres"}.}

\item{edad}{Vector numérico opcional con una o más edades (entre 0 y 99) que se desean filtrar.
Por ejemplo: \code{edad = 25}, \code{edad = c(15:19)} o \code{edad = c(0:17, 24, 50)}.
Si se deja como \code{NULL} (por defecto), se incluyen todas las edades.}
}
\value{
Un data.frame con columnas para cada estado civil:
\code{casado}, \code{divorciado}, \code{no_aplica}, \code{no_especificado}, \code{separado}, \code{soltero}, \code{union_libre}, \code{viudo}
}
\description{
Devuelve una tabla con la distribución de homicidios según estado civil (\code{CAT_CIVIL}),
con opción de desagregación por año, entidad o municipio y por sexo.
}
