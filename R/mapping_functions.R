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



#' Crée une carte interactive Leaflet pour visualiser une variable régionale
#'
#' @param spatial_data Données spatialisées (objet `sf` avec géométrie)
#' @param var Nom de la variable numérique à cartographier
#' @param popup_vars Variables à afficher dans le popup (par défaut : nom de la région + var)
#' @param palette Palette de couleurs (ex: "YlOrRd", "Viridis", "Blues", etc.)
#'
#' @return Un objet leaflet
#' @export
create_interactive_map <- function(spatial_data,
                                   var,
                                   popup_vars = NULL,
                                   palette = "YlOrRd") {

  # Vérifie les packages requis
  pkgs <- c("leaflet", "sf", "viridis", "purrr")
  lapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Le package '%s' est requis mais n'est pas installé.", pkg))
    }
  })

  # Reprojection en WGS84 (obligatoire pour leaflet)
  spatial_data <- sf::st_transform(spatial_data, 4326)

  # Palette de couleurs
  pal <- leaflet::colorNumeric(
    palette = palette,
    domain = spatial_data[[var]],
    na.color = "transparent"
  )

  # Préparation du contenu des popups
  if (is.null(popup_vars)) {
    popup_content <- paste0(
      "<strong>Région:</strong> ", spatial_data$region, "<br/>",
      "<strong>", var, ":</strong> ", round(spatial_data[[var]], 2)
    )
  } else {
    popup_content <- purrr::map_chr(1:nrow(spatial_data), function(i) {
      html <- paste0("<strong>", spatial_data$region[i], "</strong><br/>")
      for (v in popup_vars) {
        val <- spatial_data[[v]][i]
        val <- ifelse(is.numeric(val), round(val, 2), val)
        html <- paste0(html, "<strong>", v, ":</strong> ", val, "<br/>")
      }
      html
    })
  }

  # Création de la carte
  leaflet::leaflet(spatial_data) %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    leaflet::addPolygons(
      fillColor = ~pal(spatial_data[[var]]),
      fillOpacity = 0.8,
      weight = 1,
      color = "white",
      opacity = 1,
      popup = popup_content,
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    leaflet::addLegend(
      pal = pal,
      values = spatial_data[[var]],
      position = "bottomright",
      title = var
    )
}
