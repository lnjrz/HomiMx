#' Tabla de homicidios por mes (MES) y año
#'
#' Devuelve una tabla con el número de homicidios por mes (`MES`) y año,
#' con opción de desagregación por entidad o municipio y por sexo.
#'
#' @param df Un data.frame que contenga las columnas `MES`, `ANIO_OCUR`, `ANIO_REGIS`, `SEXO` y claves geográficas.
#' @param anio_tipo Tipo de año a usar: `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param desagregacion Nivel geográfico adicional: `"entidad"`, `"municipio"` o `NULL`.
#' @param geo_tipo Fuente del dato geográfico: `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo Filtro opcional por sexo: `"hombres"`, `"mujeres"` o `NULL`.
#'
#' @return Un data.frame con columnas `enero`, `febrero`, ..., `diciembre`.
#' @export
#' @importFrom dplyr filter group_by summarise n rename select across all_of
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
tabla_mes <- function(df, anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL) {

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
    if (sexo == "hombres") {
      df <- dplyr::filter(df, SEXO == 1)
    } else if (sexo == "mujeres") {
      df <- dplyr::filter(df, SEXO == 2)
    }
  }

  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "MES")

  df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = MES, values_from = n, values_fill = 0) %>%
    dplyr::rename(
      enero      = `1`,
      febrero    = `2`,
      marzo      = `3`,
      abril      = `4`,
      mayo       = `5`,
      junio      = `6`,
      julio      = `7`,
      agosto     = `8`,
      septiembre = `9`,
      octubre    = `10`,
      noviembre  = `11`,
      diciembre  = `12`
    ) %>%
    dplyr::select(dplyr::all_of(c(anio_col, geo_col)),
                  enero, febrero, marzo, abril, mayo, junio,
                  julio, agosto, septiembre, octubre, noviembre, diciembre)
}
