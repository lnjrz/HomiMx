#' Tabla de homicidios totales por año, sexo y nivel geográfico
#'
#' @param df Un data.frame con las columnas de año, sexo, geografía y claves territoriales
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (usa `"ocurrencia"` por defecto)
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`)
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99) que se desean filtrar.
#' Por ejemplo: `edad = 25`, `edad = c(15:19)` o `edad = c(0:17, 24, 50)`.
#' Si se deja como `NULL` (por defecto), se incluyen todas las edades.
#'
#' @return Un data.frame con totales por año, sexo, y opcionalmente por entidad o municipio
#' @export
#' @importFrom dplyr group_by summarise mutate case_when n select across all_of
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
tabla_total <- function(df, anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", edad = NULL) {

  # Aqui va el filtro de edad
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

  df <- df %>%
    dplyr::mutate(SEXO = dplyr::case_when(
      SEXO == 1 ~ "Hombres",
      SEXO == 2 ~ "Mujeres",
      SEXO == 9 ~ "NE",
      TRUE ~ "NE"
    ))

  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "SEXO")

  df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(homicidios = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = SEXO, values_from = homicidios, values_fill = 0) %>%
    dplyr::mutate(Total = Hombres + Mujeres + NE) %>%
    dplyr::select(dplyr::all_of(c(anio_col, geo_col)), Total, Hombres, Mujeres, NE)
}
