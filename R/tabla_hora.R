#' Tabla de homicidios por hora del día (HORAS) y año
#'
#' Devuelve una tabla con la distribución de homicidios según la hora del día (`HORAS`),
#' con opción de desagregación por año, entidad o municipio y por sexo.
#'
#' @param df Un data.frame con las columnas `HORAS`, `ANIO_OCUR`, `ANIO_REGIS`, `SEXO` y claves geográficas.
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (por defecto `"ocurrencia"`).
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo `NULL`, `"hombres"`, `"mujeres"`.
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99) que se desean filtrar.
#' Por ejemplo: `edad = 25`, `edad = c(15:19)` o `edad = c(0:17, 24, 50)`.
#' Si se deja como `NULL` (por defecto), se incluyen todas las edades.
#'
#' @return Un data.frame con columnas `hora_00` a `hora_23`, más `no_especificado`.
#' @export
#' @importFrom dplyr filter mutate group_by summarise n rename_with rename select across all_of
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
tabla_hora <- function(df, anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL, edad=NULL) {

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

  df <- df %>%
    dplyr::mutate(HORA_AJUSTADA = dplyr::case_when(
      HORAS == 24 ~ 0,
      HORAS == 99 ~ 99,
      TRUE ~ HORAS
    ))

  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "HORA_AJUSTADA")

  tabla <- df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = HORA_AJUSTADA, values_from = n, values_fill = 0) %>%
    dplyr::rename_with(.cols = as.character(0:23),
                       .fn = ~ paste0("hora_", stringr::str_pad(.x, 2, pad = "0"))) %>%
    dplyr::rename(no_especificado = `99`)

  # Orden explícito de columnas
  hora_cols <- paste0("hora_", stringr::str_pad(0:23, 2, pad = "0"))
  final_cols <- c(anio_col, geo_col, hora_cols, "no_especificado")

  tabla %>%
    dplyr::select(dplyr::all_of(final_cols[!is.null(final_cols)]))
}
