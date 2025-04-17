#' Tabla de homicidios por día de la semana (DIA_SEMANA) y año
#'
#' Devuelve una tabla con la distribución de homicidios por día de la semana,
#' con opción de desagregación por año, geografía y sexo.
#'
#' @param df Un data.frame que contenga las columnas `DIA_SEMANA`, `ANIO_OCUR`, `ANIO_REGIS`, `SEXO` y claves geográficas.
#' @param anio_tipo Tipo de año a usar: `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param desagregacion Nivel geográfico adicional: `"entidad"`, `"municipio"` o `NULL`.
#' @param geo_tipo Fuente del dato geográfico: `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo Filtro opcional por sexo: `"hombres"`, `"mujeres"` o `NULL`.
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99) que se desean filtrar.
#' Por ejemplo: `edad = 25`, `edad = c(15:19)` o `edad = c(0:17, 24, 50)`.
#' Si se deja como `NULL` (por defecto), se incluyen todas las edades.
#'
#' @return Un data.frame con columnas `ANIO_XXX`, claves geográficas (si aplica) y columnas `Dia_1`, ..., `Dia_7`.
#' @export
tabla_dia <- function(df, anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL, edad=NULL) {

  # Filtrado por edad
  if (!is.null(edad)) {
    df <- df[df$EDAD %in% edad, ]
    if (nrow(df) == 0) {
      warning("No hay registros con las edades especificadas.")
      return(data.frame())
    }
  }

  anio_col <- if (is.null(anio_tipo) || anio_tipo == "ocurrencia") "ANIO_OCUR" else "ANIO_REGIS"

  geo_col <- NULL
  if (!is.null(desagregacion)) {
    if (desagregacion == "entidad") {
      geo_col <- if (geo_tipo == "registro") "ENT_REGIS" else "ENT_OCURR"
    } else if (desagregacion == "municipio") {
      geo_col <- if (geo_tipo == "registro") "MUN_REGIS" else "MUN_OCURR"
    }
  }

  if (!is.null(sexo)) {
    df <- dplyr::filter(df, (sexo == "hombres" & SEXO == 1) | (sexo == "mujeres" & SEXO == 2))
  }

  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "DIA_SEMANA")

  df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = DIA_SEMANA, values_from = n, values_fill = 0,
                       names_prefix = "Dia_") %>%
    dplyr::select(dplyr::all_of(c(anio_col, geo_col)), dplyr::starts_with("Dia_"))
}
