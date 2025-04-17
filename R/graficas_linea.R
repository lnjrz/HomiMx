#' Gráfico de línea de homicidios a nivel nacional
#'
#' Genera una gráfica de la evolución de homicidios por año, con opción de desagregar por sexo.
#'
#' @param df Un data.frame con las columnas `ANIO_OCUR`, `ANIO_REGIS` y `SEXO`
#' @param tipo Tipo de año a usar: `"registro"` o `"ocurrencia"` (por defecto)
#' @param por_sexo Lógico. Si `TRUE`, muestra líneas por sexo. Si `FALSE`, solo la serie total.
#'
#' @return Un gráfico de línea (ggplot2)
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal theme element_blank element_text scale_x_continuous
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
grafica_linea_nacional <- function(df, tipo = NULL, por_sexo = FALSE) {
  resumen <- resumen_nacional(df, tipo = tipo) %>%
    dplyr::filter(anio >= 1990 & anio < 9999)  # 👈 Filtro solo aquí

  if (por_sexo) {
    resumen_long <- resumen %>%
      tidyr::pivot_longer(cols = c("Hombres", "Mujeres", "NE"),
                          names_to = "Sexo", values_to = "Homicidios")

    ggplot2::ggplot(resumen_long, ggplot2::aes(x = as.numeric(anio), y = Homicidios, color = Sexo)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(breaks = sort(unique(as.numeric(resumen_long$anio)))) +
      ggplot2::labs(
        title = "Evolución de homicidios por sexo",
        x = "Año", y = "Número de homicidios", color = "Sexo"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  } else {
    ggplot2::ggplot(resumen, ggplot2::aes(x = as.numeric(anio), y = Total)) +
      ggplot2::geom_line(color = "firebrick", linewidth = 1) +
      ggplot2::geom_point(color = "black", size = 2) +
      ggplot2::scale_x_continuous(breaks = sort(unique(as.numeric(resumen$anio)))) +
      ggplot2::labs(
        title = "Evolución de homicidios en México",
        x = "Año", y = "Número de homicidios"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  }
}
