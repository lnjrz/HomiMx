% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabla_lugar.R
\name{tabla_lugar}
\alias{tabla_lugar}
\title{Tabla de lugar de ocurrencia (CAT_LUG) por año}
\usage{
tabla_lugar(
  anio_tipo = NULL,
  desagregacion = NULL,
  geo_tipo = "ocurrencia",
  sexo = NULL,
  edad = NULL,
  periodo = NULL
)
}
\arguments{
\item{anio_tipo}{\code{"registro"}, \code{"ocurrencia"} o \code{NULL} (usa \code{"ocurrencia"} por defecto).}

\item{desagregacion}{\code{NULL}, \code{"entidad"} o \code{"municipio"}.}

\item{geo_tipo}{\code{"registro"} o \code{"ocurrencia"} (por defecto \code{"ocurrencia"}).}

\item{sexo}{\code{NULL}, \code{"hombres"}, \code{"mujeres"}.}

\item{edad}{Vector numérico opcional con una o más edades (entre 0 y 99) que se desean filtrar.
Por ejemplo: \code{edad = 25}, \code{edad = c(15:19)} o \code{edad = c(0:17, 24, 50)}.}

\item{periodo}{Vector numérico opcional con uno o más años (por ejemplo: \code{periodo = 2010}, \code{periodo = 2005:2010}, \code{periodo = c(2015, 2020)}).
Si se especifica, filtra los registros para que solo se incluyan los años indicados.}
}
\value{
Un data.frame con columnas: \code{institucional}, \code{laboral}, \code{recreativo_publico},
\code{hogar}, \code{via_publica}, \code{otro}, \code{no_especificado}
}
\description{
Devuelve una tabla con la distribución de homicidios según tipo de lugar (\code{CAT_LUG}),
con opción de desagregación por año, entidad o municipio, y con filtros opcionales por sexo y edad.
}
