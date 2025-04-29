#' Tabla de estado civil (CAT_CIVIL) por año
#'
#' Devuelve una tabla con la distribución de homicidios según estado civil (`CAT_CIVIL`),
#' con opción de desagregación por año, entidad o municipio, y filtros por sexo y edad.
#'
#' Categorías: `casado`, `divorciado`, `no_aplica`, `no_especificado`, `separado`,
#' `soltero`, `union_libre`, `viudo`.
#'
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (usa `"ocurrencia"` por defecto).
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo `NULL`, `"hombres"`, `"mujeres"`.
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99). Por ejemplo:
#' `edad = 25`, `edad = c(15:19)` o `edad = c(0:17, 24, 50)`.
#' @param periodo Vector numérico opcional con uno o más años (por ejemplo: `periodo = 2010`, `periodo = 2005:2010`, `periodo = c(2015, 2020)`).
#' Si se especifica, filtra los registros para que solo se incluyan los años indicados.
#'
#' @return Un `data.frame` con columnas por año, claves geográficas (si aplica), y columnas por estado civil.
#' @export
#' @importFrom dplyr group_by summarise mutate case_when n select across all_of left_join rename filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang .data
tabla_edocivil <- function(anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL, edad = NULL, periodo = NULL) {

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
  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "CAT_CIVIL")
  tabla <- df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = CAT_CIVIL, values_from = n, values_fill = 0)

  # Etiquetas
  civil_labels <- c(
    `1` = "casado", `2` = "divorciado", `3` = "no_aplica", `4` = "no_especificado",
    `5` = "separado", `6` = "soltero", `7` = "union_libre", `8` = "viudo"
  )

  # Detectar presentes y ausentes
  presentes <- intersect(names(civil_labels), colnames(tabla))
  faltantes <- setdiff(names(civil_labels), colnames(tabla))

  if (length(faltantes) > 0) {
    message("Las siguientes categorías de estado civil no están presentes en los datos filtrados: ",
            paste(civil_labels[faltantes], collapse = ", "))
  }

  # Renombrar solo presentes
  tabla <- tabla %>%
    dplyr::rename_with(~ civil_labels[.x], .cols = tidyselect::any_of(presentes))

  # Orden base y reordenar
  orden_final <- c("casado", "divorciado", "no_aplica", "no_especificado",
                   "separado", "soltero", "union_libre", "viudo")
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
