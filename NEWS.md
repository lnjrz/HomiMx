# homiMx 1.1

## 🎉 Versión inicial (estable)

- Publicación de la primera versión del paquete **homiMx**.
- Integra una base de datos depurada con defunciones por **agresión (homicidios)**, generada a partir de los microdatos del INEGI entre **1990 y 2023**.
- Homologación de claves y variables provenientes de diferentes esquemas de codificación (CIE-9 y CIE-10).

## 📦 Funciones incluidas

- `tabla_total()`: Total de homicidios por año, con opción de desagregación geográfica y por sexo.
- `tabla_causa()`: Tablas por tipo de causa (arma de fuego, cortante, asfixia, otros, no especificado).
- `tabla_ocupacion()`: Homicidios por categoría ocupacional.
- `tabla_educacion()`: Nivel educativo de las víctimas.
- `tabla_edocivil()`: Estado civil al momento de fallecer.
- `tabla_lugar()`: Lugar de ocurrencia de la agresión.
- `tabla_mes()`, `tabla_dia()`, `tabla_hora()`: Distribución temporal básica.

## ✅ Mejoras de v1.1

- parámetro de edad incluido

## 📂 Datos incluidos

- `homidata`: Base de homicidios con claves estandarizadas y variables unificadas.
- `cat_entidades`: Catálogo oficial de entidades federativas.
- `cat_municipios`: Catálogo de municipios para referencia cruzada.

## 📚 Documentación

- Viñeta introductoria: `uso_tablas`
- Archivo `README.md` con instrucciones de uso y ejemplos básicos.
- Descripción clara en el archivo `DESCRIPTION`.

---

# 🔭 Planeación para la versión 2.0.0 (en desarrollo)

- Funciones de visualización (`grafica_linea_total()`, `grafica_barras_causa()`, etc.)
- Compatibilidad con `ggplot2`, `highcharter`, `DT`, y `leaflet`
- Sitio web de documentación con `pkgdown`
- Tablas interactivas y estilo en `gt`, `DT`
- Funciones adicionales para análisis por entidad
- Integración con apps `Shiny`

---

