#' Tabla de homicidios por día de la semana (DIA_SEMANA) y año
#'
#' Devuelve una tabla con la distribución de homicidios por día de la semana (`DIA_SEMANA`),
#' con opción de desagregación por año, entidad o municipio, filtrado por sexo y edad.
#'
#' Los días están codificados del 1 al 7, donde:
#' - `Dia_1`: Domingo
#' - `Dia_2`: Lunes
#' - `Dia_3`: Martes
#' - `Dia_4`: Miércoles
#' - `Dia_5`: Jueves
#' - `Dia_6`: Viernes
#' - `Dia_7`: Sábado
#'
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (por defecto `"ocurrencia"`).
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo `NULL`, `"hombres"`, `"mujeres"`.
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99). Por ejemplo:
#' `edad = 25`, `edad = c(15:19)` o `edad = c(0:17, 24, 50)`.
#' @param periodo Vector numérico opcional con uno o más años (por ejemplo: `periodo = 2010`, `periodo = 2005:2010`, `periodo = c(2015, 2020)`).
#' Si se especifica, filtra los registros para que solo se incluyan los años indicados.
#'
#' @return Un `data.frame` con columnas por año, claves geográficas (si aplica) y días de la semana (`Dia_1` a `Dia_7`).
#' @export
#' @importFrom dplyr group_by summarise mutate case_when n select across all_of left_join filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang .data
tabla_dia <- function(anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL, edad = NULL, periodo = NULL) {
  df <- homidata

  # Convertir a numérico si viene como texto
  if (is.character(df$DIA_SEMANA)) {
    dia_map <- c(
      "lunes" = 1, "martes" = 2, "miércoles" = 3, "miercoles" = 3,
      "jueves" = 4, "viernes" = 5, "sábado" = 6, "sabado" = 6, "domingo" = 7
    )
    df$DIA_SEMANA <- tolower(df$DIA_SEMANA)
    df$DIA_SEMANA <- unname(dia_map[df$DIA_SEMANA])
  }

  # Verifica valores válidos
  df <- df[df$DIA_SEMANA %in% 1:7, ]

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
                      "municipio" = if (geo_tipo == "registro") "MUN_REGIS" else "MUN_OCURR")
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
  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "DIA_SEMANA")

  tabla <- df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = DIA_SEMANA,
      values_from = n,
      values_fill = 0,
      names_prefix = "Dia_"
    )

  # Renombrado
  dia_labels <- setNames(
    c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"),
    paste0("Dia_", 1:7)
  )

  presentes <- intersect(names(dia_labels), colnames(tabla))
  faltantes <- setdiff(names(dia_labels), colnames(tabla))

  if (length(faltantes) > 0) {
    message("No se encontraron registros para los siguientes días: ",
            paste(dia_labels[faltantes], collapse = ", "))
  }

  tabla <- tabla %>%
    dplyr::rename_with(~ dia_labels[.x], .cols = tidyselect::any_of(presentes))

  orden_dias <- unname(dia_labels)
  tabla <- tabla %>%
    dplyr::select(dplyr::all_of(c(anio_col, geo_col)), tidyselect::any_of(orden_dias))

  # Geografía
  if (!is.null(desagregacion)) {
    if (desagregacion == "entidad") {
      tabla <- tabla %>%
        dplyr::left_join(cat_entidades, by = setNames("CVE_ENT", geo_col)) %>%
        dplyr::select(dplyr::all_of(c(anio_col, geo_col)), NOM_ENT, tidyselect::any_of(orden_dias))
    } else if (desagregacion == "municipio") {
      tabla <- tabla %>%
        dplyr::left_join(cat_municipios, by = setNames("CVEGEO", geo_col)) %>%
        dplyr::mutate(CVE_ENT = substr(as.character(.data[[geo_col]]), 1, 2)) %>%
        dplyr::left_join(cat_entidades, by = "CVE_ENT") %>%
        dplyr::select(
          dplyr::all_of(anio_col),
          CVE_ENT, NOM_ENT,
          dplyr::all_of(geo_col), NOMGEO,
          tidyselect::any_of(orden_dias)
        )
    }

    if (is.null(getOption("HomiMx.geo_notice_shown"))) {
      message("Las claves geográficas corresponden al Marco Geoestadístico 2024 del INEGI")
      options(HomiMx.geo_notice_shown = TRUE)
    }
  }

  return(tabla)
}
