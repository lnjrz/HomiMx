#' Tabla de lugar de ocurrencia (CAT_LUG) por año
#'
#' Devuelve una tabla con la distribución de homicidios según tipo de lugar (`CAT_LUG`),
#' con opción de desagregación por año, entidad o municipio, y con filtros opcionales por sexo y edad.
#'
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (usa `"ocurrencia"` por defecto).
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo `NULL`, `"hombres"`, `"mujeres"`.
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99) que se desean filtrar.
#' Por ejemplo: `edad = 25`, `edad = c(15:19)` o `edad = c(0:17, 24, 50)`.
#' @param periodo Vector numérico opcional con uno o más años (por ejemplo: `periodo = 2010`, `periodo = 2005:2010`, `periodo = c(2015, 2020)`).
#' Si se especifica, filtra los registros para que solo se incluyan los años indicados.
#'
#' @return Un data.frame con columnas: `institucional`, `laboral`, `recreativo_publico`,
#' `hogar`, `via_publica`, `otro`, `no_especificado`
#' @export
#' @importFrom dplyr group_by summarise mutate case_when n select across all_of left_join rename filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang .data
tabla_lugar <- function(anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL, edad = NULL, periodo = NULL) {

  df <- homidata

  # Filtro por edad
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
    if (desagregacion == "entidad") {
      geo_col <- if (geo_tipo == "registro") "ENT_REGIS" else "ENT_OCURR"
    } else if (desagregacion == "municipio") {
      geo_col <- if (geo_tipo == "registro") "MUN_REGIS" else "MUN_OCURR"
    }
  }

  # Filtro por sexo
  if (!is.null(sexo)) {
    df <- dplyr::filter(df, SEXO == ifelse(sexo == "hombres", 1, 2))
    if (nrow(df) == 0) {
      warning("No hay registros para el sexo especificado.")
      return(data.frame())
    }
  }

  # Agrupación y pivot
  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "CAT_LUG")
  tabla <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = CAT_LUG, values_from = n, values_fill = 0)

  # Mapeo de nombres
  lugar_labels <- c(
    `1` = "institucional",
    `2` = "laboral",
    `3` = "recreativo_publico",
    `4` = "hogar",
    `5` = "via_publica",
    `6` = "otro",
    `7` = "no_especificado"
  )

  presentes <- intersect(names(lugar_labels), colnames(tabla))
  faltantes <- setdiff(names(lugar_labels), colnames(tabla))

  if (length(faltantes) > 0) {
    message("Las siguientes categorías de lugar no están presentes en los datos filtrados: ",
            paste(lugar_labels[faltantes], collapse = ", "))
  }

  tabla <- tabla %>%
    dplyr::rename_with(~ lugar_labels[.x], .cols = tidyselect::any_of(presentes))

  # Orden base
  orden_final <- c("institucional", "laboral", "recreativo_publico", "hogar",
                   "via_publica", "otro", "no_especificado")

  vars_inicio <- c(anio_col, geo_col)
  tabla <- tabla %>%
    dplyr::select(dplyr::all_of(vars_inicio), tidyselect::any_of(orden_final))

  # Pegado de etiquetas geográficas
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
