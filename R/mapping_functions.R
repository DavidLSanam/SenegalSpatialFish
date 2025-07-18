#' Crée une carte ggplot stylisée pour une variable régionale
#'
#' @param spatial_data Données spatiales (avec géométrie + variable à représenter)
#' @param var Nom de la variable à représenter (ex: "QuantiteConsommeeKG")
#' @param title Titre de la carte
#' @param palette Palette de couleurs viridis (ex: "viridis", "plasma", "magma", "cividis")
#' @param legend_title Titre de la légende
#'
#' @return Un objet ggplot2
#' @export
create_professional_map <- function(spatial_data, var,
                                    title = "",
                                    palette = "viridis",
                                    legend_title = "") {

  # Vérifie si la variable existe
  if (!var %in% names(spatial_data)) {
    stop(paste("La variable", var, "n'existe pas dans les données."))
  }

  ggplot2::ggplot(spatial_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = !!sym(var)), color = "white", size = 0.2) +
    ggplot2::scale_fill_viridis_c(
      option = palette,
      direction = 1,
      na.value = "grey80",
      labels = scales::comma
    ) +
    ggplot2::labs(
      title = title,
      fill = legend_title
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    ) +
    ggplot2::coord_sf(datum = NA)
}


#' Génère directement une carte spatiale stylisée à partir d'une base de consommation brute
#'
#' @param base_data La base de consommation brute (avec région labellisée)
#' @param shapefile_path Chemin vers le shapefile (optionnel si déjà intégré dans le package)
#' @param variable Nom de la variable à cartographier (ex: "QuantiteConsommeeKG")
#' @param palette Palette de couleurs viridis (ex: "plasma", "magma", etc.)
#' @param title Titre de la carte
#' @param legend_title Titre de la légende
#'
#' @return Un objet ggplot2 (la carte)
#' @export
generate_spatial_map <- function(base_data,
                                 shapefile_path = NULL,
                                 variable = "QuantiteConsommeeKG",
                                 palette = "viridis",
                                 title = "",
                                 legend_title = "") {

  require_packages(c("sf", "dplyr", "ggplot2", "stringr", "haven", "viridis", "rlang"))

  # 1. Nettoyage des noms de région
  base_data_clean <- base_data %>%
    dplyr::mutate(region = haven::as_factor(region)) %>%
    dplyr::mutate(region = as.character(region)) %>%
    dplyr::mutate(region = stringr::str_replace_all(region, "-", " ")) %>%
    dplyr::mutate(region = stringr::str_trim(toupper(region))) %>%
    dplyr::filter(!is.na(region))

  # 2. Agrégation régionale
  regional_stats <- base_data_clean %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      QuantiteConsommeeKG = sum(QuantiteConsommeeKG, na.rm = TRUE),
      ValeurConsommee = sum(ValeurConsommee, na.rm = TRUE),
      CaloriesParTete = mean(CaloriesParTete, na.rm = TRUE),
      .groups = "drop"
    )

  # 3. Chargement des formes géographiques
  region_sf <- load_regional_shapes(path = shapefile_path)

  # 4. Intégration
  spatial_data <- integrate_data_with_shapes(regional_stats, region_sf)

  # 5. Affichage de la carte
  return(create_professional_map(
    spatial_data,
    var = variable,
    title = title,
    legend_title = legend_title,
    palette = palette
  ))
}


#' Génère directement une carte interactive Leaflet à partir d'une base de consommation brute
#'
#' @param base_data La base de consommation brute (avec région labellisée)
#' @param shapefile_path Chemin vers le shapefile (optionnel si déjà intégré dans le package)
#' @param variable Nom de la variable à cartographier (ex: "QuantiteConsommeeKG")
#' @param popup_vars Variables à afficher dans les popups (ex: c("ValeurConsommee", "CaloriesParTete"))
#' @param palette Palette de couleurs (ex: "YlOrRd", "Viridis", "Blues", etc.)
#'
#' @return Un objet leaflet (carte interactive)
#' @export
generate_interactive_map <- function(base_data,
                                     shapefile_path = NULL,
                                     variable = "QuantiteConsommeeKG",
                                     popup_vars = c("ValeurConsommee", "CaloriesParTete"),
                                     palette = "YlOrRd") {

  require_packages(c("sf", "dplyr", "stringr", "haven", "leaflet", "viridis", "purrr"))

  # 1. Nettoyage des noms de région
  base_data_clean <- base_data %>%
    dplyr::mutate(region = haven::as_factor(region)) %>%
    dplyr::mutate(region = as.character(region)) %>%
    dplyr::mutate(region = stringr::str_replace_all(region, "-", " ")) %>%
    dplyr::mutate(region = stringr::str_trim(toupper(region))) %>%
    dplyr::filter(!is.na(region))

  # 2. Agrégation régionale
  regional_stats <- base_data_clean %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      QuantiteConsommeeKG = sum(QuantiteConsommeeKG, na.rm = TRUE),
      ValeurConsommee = sum(ValeurConsommee, na.rm = TRUE),
      CaloriesParTete = mean(CaloriesParTete, na.rm = TRUE),
      .groups = "drop"
    )

  # 3. Chargement des formes géographiques
  region_sf <- load_regional_shapes(path = shapefile_path)

  # 4. Intégration
  spatial_data <- integrate_data_with_shapes(regional_stats, region_sf)

  # 5. Création de la carte interactive
  return(create_interactive_map(
    spatial_data,
    var = variable,
    popup_vars = popup_vars,
    palette = palette
  ))
}

