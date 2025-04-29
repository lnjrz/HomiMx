#' Tabla de homicidios totales por año, sexo y nivel geográfico
#'
#' Genera una tabla con el número total de homicidios por año,
#' con opción de desagregación por sexo, entidad o municipio,
#' y filtrado por grupo de edad.
#'
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (por defecto `"ocurrencia"`).
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99).
#' @param periodo Vector numérico opcional con uno o más años (por ejemplo: `periodo = 2010`, `periodo = 2005:2010`, `periodo = c(2015, 2020)`).
#' Si se especifica, filtra los registros para que solo se incluyan los años indicados.
#'
#' @return Un `data.frame` con totales por año, sexo (`Hombres`, `Mujeres`, `NE`, `Total`),
#' y, si se especifica, columnas geográficas como `NOM_ENT` o `NOMGEO`.
#' @export
#' @importFrom dplyr group_by summarise mutate filter left_join select rename rename_with n across all_of
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyselect any_of
tabla_total <- function(anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", edad = NULL, periodo = NULL) {
  df <- homidata

  # Filtrado por edad
  if (!is.null(edad)) {
    df <- df[df$EDAD %in% edad, ]
    if (nrow(df) == 0) {
      warning("No hay registros con las edades especificadas.")
      return(data.frame())
    }
  }

  # Año
  anio_col <- if (is.null(anio_tipo) || anio_tipo == "ocurrencia") "ANIO_OCUR" else "ANIO_REGIS"

  # Filtro por periodo
  if (!is.null(periodo)) {
    df <- df[df[[anio_col]] %in% periodo, ]
    if (nrow(df) == 0) {
      warning("No hay registros para el periodo especificado.")
      return(data.frame())
    }
  }

  # Geografía
  geo_col <- NULL
  if (!is.null(desagregacion)) {
    geo_col <- if (desagregacion == "entidad") {
      if (geo_tipo == "registro") "ENT_REGIS" else "ENT_OCURR"
    } else if (desagregacion == "municipio") {
      if (geo_tipo == "registro") "MUN_REGIS" else "MUN_OCURR"
    }
  }

  # Etiquetas de sexo
  df <- df %>%
    dplyr::mutate(SEXO = dplyr::case_when(
      SEXO == 1 ~ "Hombres",
      SEXO == 2 ~ "Mujeres",
      SEXO == 9 ~ "NE",
      TRUE ~ "NE"
    ))

  # Agrupación
  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "SEXO")

  tabla <- df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(homicidios = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = SEXO, values_from = homicidios, values_fill = 0)

  # Verificar columnas esperadas
  columnas_esperadas <- c("Hombres", "Mujeres", "NE")
  columnas_presentes <- intersect(columnas_esperadas, colnames(tabla))
  columnas_faltantes <- setdiff(columnas_esperadas, colnames(tabla))

  if (length(columnas_faltantes) > 0) {
    message("Las siguientes categorías de sexo no están presentes en los datos filtrados: ",
            paste(columnas_faltantes, collapse = ", "))
  }

  # Calcular total si existen columnas válidas
  tabla <- tabla %>%
    dplyr::mutate(Total = rowSums(dplyr::across(tidyselect::any_of(columnas_esperadas)), na.rm = TRUE))

  # Reordenar columnas
  columnas_salida <- c(anio_col, geo_col, "Total", columnas_esperadas)
  tabla <- tabla %>%
    dplyr::select(tidyselect::any_of(columnas_salida))

  # Unión con catálogos
  if (!is.null(desagregacion) && desagregacion == "entidad") {
    tabla <- tabla %>%
      dplyr::left_join(cat_entidades, by = setNames("CVE_ENT", geo_col)) %>%
      dplyr::relocate(NOM_ENT, .after = geo_col)

  } else if (!is.null(desagregacion) && desagregacion == "municipio") {
    tabla <- tabla %>%
      dplyr::left_join(cat_municipios, by = setNames("CVEGEO", geo_col)) %>%
      dplyr::mutate(CVE_ENT = substr(as.character(.data[[geo_col]]), 1, 2)) %>%
      dplyr::left_join(cat_entidades, by = "CVE_ENT") %>%
      dplyr::relocate(CVE_ENT, NOM_ENT, NOMGEO, .after = geo_col)
  }

  if (!is.null(desagregacion)) {
    if (is.null(getOption("HomiMx.geo_notice_shown"))) {
      message("Las claves geográficas corresponden al Marco Geoestadístico 2024 del INEGI")
      options(HomiMx.geo_notice_shown = TRUE)
    }
  }

  return(tabla)
}
