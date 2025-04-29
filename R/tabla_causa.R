#' Tabla de causas (REC_CAU) por año y nivel geográfico
#'
#' Genera una tabla con el número de homicidios según categoría de causa
#' (variable `REC_CAU`), con opción de desagregación por año, entidad o municipio,
#' además de filtrado por sexo y grupo de edad.
#'
#' Las causas consideradas son:
#' - `arma_fuego` (1)
#' - `arma_cortante` (2)
#' - `asfixia` (3)
#' - `otros` (4)
#' - `no_especificado` (5)
#'
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (por defecto `"ocurrencia"`).
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo `NULL`, `"hombres"` o `"mujeres"`.
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99).
#' @param periodo Vector numérico opcional con uno o más años (por ejemplo: `periodo = 2010`, `periodo = 2005:2010`, `periodo = c(2015, 2020)`).
#' Si se especifica, filtra los registros para que solo se incluyan los años indicados.
#'
#' @return Un `data.frame` con columnas `arma_fuego`, `arma_cortante`, `asfixia`, `otros`, `no_especificado`,
#' y columnas geográficas si aplica.
#'
#' @export
#' @importFrom dplyr group_by summarise mutate filter left_join select rename rename_with n across all_of
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyselect any_of

tabla_causa <- function(anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL, edad = NULL, periodo = NULL) {

  df <- homidata

  # Filtro de edad
  if (!is.null(edad)) {
    df <- df[df$EDAD %in% edad, ]
    if (nrow(df) == 0) {
      warning("No hay registros con las edades especificadas.")
      return(data.frame())
    }
  }

  # Año y geografía
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
    geo_col <- switch(desagregacion,
                      "entidad"   = if (geo_tipo == "registro") "ENT_REGIS" else "ENT_OCURR",
                      "municipio" = if (geo_tipo == "registro") "MUN_REGIS" else "MUN_OCURR",
                      NULL
    )
  }

  # Filtro de sexo
  if (!is.null(sexo)) {
    df <- dplyr::filter(df, SEXO == ifelse(sexo == "hombres", 1, 2))
  }

  # Agrupación
  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "REC_CAU")

  tabla <- df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = REC_CAU, values_from = n, values_fill = 0)

  # Etiquetas de causa
  causa_labels <- c(
    `1` = "arma_fuego",
    `2` = "arma_cortante",
    `3` = "asfixia",
    `4` = "otros",
    `5` = "no_especificado"
  )

  # Verificar columnas presentes y faltantes
  cols_presentes <- intersect(names(causa_labels), colnames(tabla))
  cols_faltantes <- setdiff(names(causa_labels), colnames(tabla))

  if (length(cols_faltantes) > 0) {
    message("Las siguientes categorías de causa no están presentes en los datos filtrados: ",
            paste(causa_labels[cols_faltantes], collapse = ", "))
  }

  # Renombrar
  tabla <- tabla %>%
    dplyr::rename_with(~ causa_labels[.x], .cols = tidyselect::any_of(cols_presentes))

  # Orden final
  orden_final <- unname(causa_labels)

  # Select ordenado
  vars_inicio <- c(anio_col, geo_col)
  tabla <- tabla %>%
    dplyr::select(dplyr::all_of(vars_inicio), tidyselect::any_of(orden_final))

  # Pegado de nombres geográficos
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
