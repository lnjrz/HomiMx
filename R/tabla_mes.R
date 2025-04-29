#' Tabla de homicidios por mes (MES) y año
#'
#' Devuelve una tabla con el número de homicidios por mes (`MES`) y año,
#' con opción de desagregación por entidad o municipio y por sexo.
#'
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (por defecto `"ocurrencia"`).
#' @param desagregacion `"entidad"`, `"municipio"` o `NULL`.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo `"hombres"`, `"mujeres"` o `NULL`.
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99) que se desean filtrar.
#' @param periodo Vector numérico opcional con uno o más años (por ejemplo: `periodo = 2010`, `periodo = 2005:2010`, `periodo = c(2015, 2020)`).
#' Si se especifica, filtra los registros para que solo se incluyan los años indicados.
#'
#' @return Un data.frame con columnas: `enero`, `febrero`, ..., `diciembre`
#' @export
#' @importFrom dplyr group_by summarise mutate case_when n select across all_of left_join rename filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang .data
tabla_mes <- function(anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL, edad = NULL, periodo = NULL) {

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
    geo_col <- switch(desagregacion,
                      "entidad" = if (geo_tipo == "registro") "ENT_REGIS" else "ENT_OCURR",
                      "municipio" = if (geo_tipo == "registro") "MUN_REGIS" else "MUN_OCURR",
                      NULL)
  }

  # Filtro por sexo
  if (!is.null(sexo)) {
    df <- dplyr::filter(df, SEXO == ifelse(sexo == "hombres", 1, 2))
    if (nrow(df) == 0) {
      warning("No hay registros para el sexo especificado.")
      return(data.frame())
    }
  }

  # Agrupación
  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "MES")
  tabla <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = MES, values_from = n, values_fill = 0)

  # Mapeo de nombres de mes
  mes_labels <- c(
    `1` = "enero", `2` = "febrero", `3` = "marzo", `4` = "abril",
    `5` = "mayo", `6` = "junio", `7` = "julio", `8` = "agosto",
    `9` = "septiembre", `10` = "octubre", `11` = "noviembre", `12` = "diciembre"
  )

  presentes <- intersect(names(mes_labels), colnames(tabla))
  faltantes <- setdiff(names(mes_labels), colnames(tabla))

  if (length(faltantes) > 0) {
    message("No se registraron homicidios en los siguientes meses: ",
            paste(mes_labels[faltantes], collapse = ", "))
  }

  # Renombrar solo los presentes
  tabla <- tabla %>%
    dplyr::rename_with(~ mes_labels[.x], .cols = tidyselect::any_of(presentes))

  orden_final <- unname(mes_labels)  # enero a diciembre
  vars_inicio <- c(anio_col, geo_col)
  tabla <- tabla %>%
    dplyr::select(dplyr::all_of(vars_inicio), tidyselect::any_of(orden_final))

  # Pegar nombres geográficos
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
        dplyr::all_of(anio_col), CVE_ENT, NOM_ENT,
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
