#' Tabla de nivel educativo (CAT_ESC) por año
#'
#' Devuelve una tabla con la distribución de homicidios según nivel educativo (`CAT_ESC`),
#' con opción de desagregación por año, entidad o municipio y por sexo.
#'
#' @param df Un data.frame que contenga las columnas `CAT_ESC`, `ANIO_OCUR`, `ANIO_REGIS`, `SEXO` y claves geográficas.
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (usa `"ocurrencia"` por defecto).
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo `NULL`, `"hombres"`, `"mujeres"`.
#'
#' @return Un data.frame con columnas para cada nivel educativo:
#' `sin_educacion`, `basica`, `media`, `superior`, `no_aplica`, `no_especificado`
#' @export
#' @importFrom dplyr filter group_by summarise n rename select across all_of
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
tabla_educacion <- function(df, anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL) {

  anio_col <- if (is.null(anio_tipo) || anio_tipo == "ocurrencia") {
    "ANIO_OCUR"
  } else if (anio_tipo == "registro") {
    "ANIO_REGIS"
  } else {
    warning("Valor de 'anio_tipo' no reconocido. Se usará 'ANIO_OCUR'.")
    "ANIO_OCUR"
  }

  geo_col <- NULL
  if (!is.null(desagregacion)) {
    if (desagregacion == "entidad") {
      geo_col <- if (geo_tipo == "registro") "ENT_REGIS" else "ENT_OCURR"
    } else if (desagregacion == "municipio") {
      geo_col <- if (geo_tipo == "registro") "MUN_REGIS" else "MUN_OCURR"
    }
  }

  if (!is.null(sexo)) {
    if (sexo == "hombres") {
      df <- dplyr::filter(df, SEXO == 1)
    } else if (sexo == "mujeres") {
      df <- dplyr::filter(df, SEXO == 2)
    }
  }

  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "CAT_ESC")

  df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = CAT_ESC, values_from = n, values_fill = 0) %>%
    dplyr::rename(
      sin_educacion   = `1`,
      basica          = `2`,
      media           = `3`,
      superior        = `4`,
      no_aplica       = `5`,
      no_especificado = `6`
    ) %>%
    dplyr::select(dplyr::all_of(c(anio_col, geo_col)),
                  sin_educacion, basica, media, superior, no_aplica, no_especificado)
}
