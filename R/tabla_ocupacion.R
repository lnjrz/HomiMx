#' Tabla de homicidios por ocupación (CAT_OCU), año y nivel geográfico
#'
#' Genera una tabla con el número de homicidios según categoría ocupacional
#' (variable `CAT_OCU`), con opción de desagregación por año, entidad o municipio,
#' además de filtrado por sexo y grupo de edad.
#'
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL` (por defecto `"ocurrencia"`).
#' Especifica si el año a considerar es el de registro o el de ocurrencia.
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`.
#' Define el nivel geográfico para desagregar la información.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' Indica si la desagregación geográfica corresponde al lugar de registro o ocurrencia.
#' @param sexo `NULL`, `"hombres"` o `"mujeres"`.
#' Si se especifica, filtra los homicidios por sexo.
#' @param edad Vector numérico opcional con una o más edades (entre 0 y 99).
#' Por ejemplo: `edad = 25`, `edad = c(15:19)` o `edad = c(0:17, 24, 50)`.
#' Si se deja como `NULL` (por defecto), se incluyen todas las edades.
#' @param periodo Vector numérico opcional con uno o más años (por ejemplo: `periodo = 2010`, `periodo = 2005:2010`, `periodo = c(2015, 2020)`).
#' Si se especifica, filtra los registros para que solo se incluyan los años indicados.
#'
#' @return Un `data.frame` con columnas para cada categoría ocupacional:
#' `agricultor`, `artesanos`, `empleados`, `no_trabajaba`,
#' `comerciantes`, `operadores`, `directores`, `profesionales`,
#' `seguridad`, `no_especificado`, `no_aplica`.
#' Puede incluir columnas geográficas como `NOM_ENT` o `NOMGEO` si se especifica `desagregacion`.
#'
#' @export
#' @importFrom dplyr group_by summarise mutate case_when n select across all_of left_join rename rename_with
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang .data

tabla_ocupacion <- function(anio_tipo = NULL, desagregacion = NULL, geo_tipo = "ocurrencia", sexo = NULL, edad = NULL, periodo = NULL) {

  df <- homidata

  # Filtrado por edad
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
    if (desagregacion == "entidad") {
      geo_col <- if (geo_tipo == "registro") "ENT_REGIS" else "ENT_OCURR"
    } else if (desagregacion == "municipio") {
      geo_col <- if (geo_tipo == "registro") "MUN_REGIS" else "MUN_OCURR"
    }
  }

  # Sexo
  if (!is.null(sexo)) {
    df <- dplyr::filter(df, SEXO == ifelse(sexo == "hombres", 1, 2))
  }

  # Agrupación
  group_vars <- c(anio_col, if (!is.null(geo_col)) geo_col, "CAT_OCU")

  tabla <- df %>%
    dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = CAT_OCU, values_from = n, values_fill = 0)

  # Mapeo de nombres
  ocup_labels <- c(
    `1` = "agricultor",
    `2` = "artesanos",
    `3` = "empleados",
    `4` = "no_trabajaba",
    `5` = "comerciantes",
    `6` = "operadores",
    `7` = "directores",
    `8` = "profesionales",
    `9` = "no_especificado",
    `10` = "no_aplica",
    `11` = "seguridad"
  )

  # Identificar columnas presentes y faltantes
  cols_presentes <- intersect(names(ocup_labels), colnames(tabla))
  cols_faltantes <- setdiff(names(ocup_labels), colnames(tabla))

  if (length(cols_faltantes) > 0) {
    message("Las siguientes categorías de ocupación no están presentes en los datos filtrados: ",
            paste(ocup_labels[cols_faltantes], collapse = ", "))
  }

  # Renombrar solo las que están
  tabla <- tabla %>%
    dplyr::rename_with(~ ocup_labels[.x], .cols = tidyselect::any_of(cols_presentes))

  # Orden base
  orden_final <- c(
    "agricultor", "artesanos", "empleados", "no_trabajaba", "comerciantes",
    "operadores", "directores", "profesionales", "seguridad",
    "no_especificado", "no_aplica"
  )

  # Reordenar columnas según desagregación
  vars_inicio <- c(anio_col, geo_col)
  tabla <- tabla %>%
    dplyr::select(dplyr::all_of(vars_inicio), tidyselect::any_of(orden_final))

  # Pegar nombres
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
