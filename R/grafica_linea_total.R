#' Gráfica de línea de homicidios por año
#'
#' Genera una gráfica de línea del total de homicidios por año, con opción de desagregar por entidad o municipio.
#'
#' @param df Un data.frame como el generado por `table_total()`, típicamente `homidata`.
#' @param anio_tipo `"registro"`, `"ocurrencia"` o `NULL`.
#' @param desagregacion `NULL`, `"entidad"` o `"municipio"`.
#' @param geo_tipo `"registro"` o `"ocurrencia"` (por defecto `"ocurrencia"`).
#' @param sexo `NULL`, `"hombres"`, `"mujeres"`.
#' @param entidad_filtro Clave numérica de entidad si se desea filtrar municipios dentro de una entidad.
#' @param top_n Número máximo de líneas a mostrar (por total acumulado). Default: 10.
#' @param proporcion Si `TRUE`, muestra proporciones en lugar de valores absolutos.
#'
#' @return Un objeto ggplot2
#' @export
#' @importFrom ggplot2 ggplot aes geom_line labs scale_y_continuous theme_minimal theme element_text
#' @importFrom dplyr group_by summarise mutate arrange desc slice_head ungroup filter select rename pull
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
grafica_linea_total <- function(df,
                                anio_tipo = NULL,
                                desagregacion = NULL,
                                geo_tipo = "ocurrencia",
                                sexo = NULL,
                                entidad_filtro = NULL,
                                top_n = 10,
                                proporcion = FALSE) {

  datos <- tabla_total(df,
                       anio_tipo = anio_tipo,
                       desagregacion = desagregacion,
                       geo_tipo = geo_tipo)

  # Filtrar años válidos (1990–2024)
  anio_col <- names(datos)[1]
  datos[[anio_col]] <- as.numeric(datos[[anio_col]])
  datos <- datos[dplyr::between(datos[[anio_col]], 1990, 2024), ]

  # Elegir variable de interés (Total o SEXO)
  if (!is.null(sexo)) {
    datos <- dplyr::select(datos, dplyr::all_of(c(names(datos)[1:ifelse(is.null(desagregacion), 1, 2)], sexo)))
    names(datos)[ncol(datos)] <- "valor"
  } else {
    datos <- dplyr::select(datos, dplyr::all_of(c(names(datos)[1:ifelse(is.null(desagregacion), 1, 2)], "Total"))) %>%
      dplyr::rename(valor = Total)
  }

  # Filtrar municipios dentro de entidad si se especifica
  if (desagregacion == "municipio" && !is.null(entidad_filtro)) {
    clave <- if (geo_tipo == "registro") "ENT_REGIS" else "ENT_OCURR"
    datos <- datos[dplyr::pull(datos, clave) == entidad_filtro, ]
  }

  # Seleccionar top_n jurisdicciones con mayor acumulado
  if (!is.null(desagregacion)) {
    top_ids <- datos %>%
      dplyr::group_by_at(2) %>%
      dplyr::summarise(total_acum = sum(valor), .groups = "drop") %>%
      dplyr::arrange(desc(total_acum)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::pull(1)

    datos <- datos[dplyr::pull(datos, 2) %in% top_ids, ]
  }

  # Calcular proporciones si se solicita
  if (proporcion) {
    datos <- datos %>%
      dplyr::group_by_at(1) %>%
      dplyr::mutate(valor = valor / sum(valor)) %>%
      dplyr::ungroup()
  }

  nombres <- names(datos)

  # Crear gráfico
  p <- if (is.null(desagregacion)) {
    ggplot2::ggplot(datos, ggplot2::aes_string(x = nombres[1], y = "valor")) +
      ggplot2::geom_line(linewidth = 1.1, color = "steelblue")
  } else {
    ggplot2::ggplot(datos, ggplot2::aes_string(x = nombres[1], y = "valor", color = nombres[2], group = nombres[2])) +
      ggplot2::geom_line(linewidth = 1.1)
  }

  # Añadir etiquetas y estilo
  p +
    ggplot2::labs(
      x = "Año",
      y = ifelse(proporcion, "Proporción", "Número de homicidios"),
      color = ifelse(desagregacion == "municipio", "Municipio",
                     ifelse(desagregacion == "entidad", "Entidad", NULL)),
      title = "Evolución de homicidios por año"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom"
    )
}
