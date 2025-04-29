#' Tabla de nivel educativo (CAT_ESC) por año y nivel geográfico
#'
#' Genera una tabla con el número de homicidios según nivel educativo (`CAT_ESC`),
#' con opción de desagregación por año, entidad o municipio, y filtros por sexo y edad.
#'
#' La tabla incluye únicamente las categorías educativas presentes en los datos filtrados.
#' Si alguna categoría no está presente, se emite un mensaje agrupado indicando cuáles faltan.
#'
#' Categorías posibles:
#' - `sin_educacion` (1)
#' - `basica` (2)
#' - `media` (3)
#' - `superior` (4)
#' - `no_aplica` (5)
#' - `no_especificado` (6)
#'
#' @param anio_tipo Tipo de año a considerar: `"registro"`, `"ocurrencia"` o `NULL` (por defecto `"ocurrencia"`).
#' @param desagregacion Nivel geográfico adicional: `"entidad"`, `"municipio"` o `NULL`.
#' @param geo_tipo Fuente del dato geográfico: `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo Filtro opcional por sexo: `"hombres"`, `"mujeres"` o `NULL`.
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99).
#' Por ejemplo: `edad = 25`, `edad = c(15:19)` o `edad = c(0:17, 24, 50)`.
#' @param periodo Vector numérico opcional con uno o más años (por ejemplo: `periodo = 2010`, `periodo = 2005:2010`, `periodo = c(2015, 2020)`).
#' Si se especifica, filtra los registros para que solo se incluyan los años indicados.
#'
#' @return Un data.frame con columnas por año, claves geográficas (si aplica), y categorías educativas.
#' @export
#' @importFrom dplyr group_by summarise mutate case_when n select across all_of left_join rename filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang .data
tabla_educacion <- function(anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL, edad = NULL, periodo = NULL) {

  df <- homidata

  if (!is.null(edad)) {
    df <- df[df$EDAD %in% edad, ]
    if (nrow(df) == 0) {
      warning("No hay registros con las edades especificadas.")
      return(data.frame())
    }
  }

  anio_col <- if (is.null(anio_tipo) || anio_tipo == "ocurrencia") "ANIO_OCUR" else "ANIO_REGIS"

  # Filtro por periodo
  if (!is.null(periodo)) {
    df <- df[df[[anio_col]] %in% periodo, ]
    if (nrow(df) == 0) {
      warning("No hay registros para el periodo especificado.")
      return(data.frame())
    }
  }

  geo_col <- NULL
  if (!is.null(desagregacion)) {
    geo_col <- if (desagregacion == "entidad") {
      if (geo_tipo == "registro") "ENT_REGIS" else "ENT_OCURR"
    } else if (desagregacion == "municipio") {
      if (geo_tipo == "registro") "MUN_REGIS" else "MUN_OCURR"
    }
  }

  if (!is.null(sexo)) {
    df <- dplyr::filter(df, (sexo == "hombres" & SEXO == 1) | (sexo == "mujeres" & SEXO == 2))
    if (nrow(df) == 0) {
      warning("No hay registros para el sexo especificado.")
      return(data.frame())
    }
  }

  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "CAT_ESC")

  tabla <- df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = CAT_ESC, values_from = n, values_fill = 0)

  esc_labels <- c(
    `1` = "sin_educacion",
    `2` = "basica",
    `3` = "media",
    `4` = "superior",
    `5` = "no_aplica",
    `6` = "no_especificado"
  )

  cols_presentes <- intersect(names(esc_labels), colnames(tabla))
  cols_faltantes <- setdiff(names(esc_labels), colnames(tabla))

  if (length(cols_faltantes) > 0) {
    message("Las siguientes categorías educativas no están presentes en los datos filtrados: ",
            paste(esc_labels[cols_faltantes], collapse = ", "))
  }

  tabla <- tabla %>%
    dplyr::rename_with(~ esc_labels[.x], .cols = tidyselect::any_of(cols_presentes))

  orden_final <- c("sin_educacion", "basica", "media", "superior", "no_aplica", "no_especificado")
  vars_inicio <- c(anio_col, geo_col)
  tabla <- tabla %>%
    dplyr::select(dplyr::all_of(vars_inicio), tidyselect::any_of(orden_final))

  if (!is.null(desagregacion) && desagregacion == "entidad") {
    tabla <- tabla %>%
      dplyr::left_join(cat_entidades, by = setNames("CVE_ENT", geo_col)) %>%
      dplyr::select(dplyr::all_of(c(anio_col, geo_col)), NOM_ENT, tidyselect::any_of(orden_final))

  } else if (!is.null(desagregacion) && desagregacion == "municipio") {
    tabla <- tabla %>%
      dplyr::left_join(cat_municipios, by = setNames("CVEGEO", geo_col)) %>%
      dplyr::mutate(CVE_ENT = substr(as.character(.data[[geo_col]]), 1, 2)) %>%
      dplyr::left_join(cat_entidades, by = "CVE_ENT") %>%
      dplyr::select(
        dplyr::all_of(anio_col),
        CVE_ENT, NOM_ENT,
        dplyr::all_of(geo_col), NOMGEO,
        tidyselect::any_of(orden_final)
      )
  }

  if (!is.null(desagregacion)) {
    if (is.null(getOption("HomiMx.geo_notice_shown"))) {
      message("Las claves geográficas corresponden al Marco Geoestadístico 2024 del INEGI")
      options(HomiMx.geo_notice_shown = TRUE)
    }
  }

  return(tabla)
}
