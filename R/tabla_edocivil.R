#' Tabla de estado civil (CAT_CIVIL) por año
#'
#' Devuelve una tabla con la distribución de homicidios según estado civil (`CAT_CIVIL`),
#' con opción de desagregación por año, entidad o municipio y por sexo.
#'
#' @param df Un data.frame que contenga las columnas `CAT_CIVIL`, `ANIO_OCUR`, `ANIO_REGIS`, `SEXO` y claves geográficas.
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (usa `"ocurrencia"` por defecto).
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo `NULL`, `"hombres"`, `"mujeres"`.
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99) que se desean filtrar.
#' Por ejemplo: `edad = 25`, `edad = c(15:19)` o `edad = c(0:17, 24, 50)`.
#' Si se deja como `NULL` (por defecto), se incluyen todas las edades.
#'
#' @return Un data.frame con columnas para cada estado civil:
#' `casado`, `divorciado`, `no_aplica`, `no_especificado`, `separado`, `soltero`, `union_libre`, `viudo`
#' @export
#' @importFrom dplyr filter group_by summarise n rename select across all_of
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
tabla_edocivil <- function(df, anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL, edad=NULL) {

  # Filtrado por edad
  if (!is.null(edad)) {
    df <- df[df$EDAD %in% edad, ]
    if (nrow(df) == 0) {
      warning("No hay registros con las edades especificadas.")
      return(data.frame())
    }
  }

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

  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "CAT_CIVIL")

  df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = CAT_CIVIL, values_from = n, values_fill = 0) %>%
    dplyr::rename(
      casado          = `1`,
      divorciado      = `2`,
      no_aplica       = `3`,
      no_especificado = `4`,
      separado        = `5`,
      soltero         = `6`,
      union_libre     = `7`,
      viudo           = `8`
    ) %>%
    dplyr::select(dplyr::all_of(c(anio_col, geo_col)),
                  casado, divorciado, no_aplica, no_especificado, separado,
                  soltero, union_libre, viudo)
}
