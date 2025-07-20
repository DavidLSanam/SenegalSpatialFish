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
    ggplot2::labs(title = title, fill = legend_title) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    ) +
    ggplot2::coord_sf(datum = NA)
}


#' Génère directement une carte spatiale stylisée
#'
#' @param base_data Base de consommation brute
#' @param shapefile_path Chemin vers le shapefile
#' @param variable Variable à cartographier
#' @param palette Palette de couleurs viridis
#' @param title Titre de la carte
#' @param legend_title Titre de la légende
#'
#' @return Un ggplot2
#' @export
generate_spatial_map <- function(base_data,
                                 shapefile_path = NULL,
                                 variable = "QuantiteConsommeeKG",
                                 palette = "viridis",
                                 title = "",
                                 legend_title = "") {

  require_packages(c("sf", "dplyr", "ggplot2", "stringr", "haven", "viridis", "rlang"))

  base_data_clean <- base_data %>%
    dplyr::mutate(region = haven::as_factor(region)) %>%
    dplyr::mutate(region = stringr::str_replace_all(as.character(region), "-", " ")) %>%
    dplyr::mutate(region = stringr::str_trim(toupper(region))) %>%
    dplyr::filter(!is.na(region))

  regional_stats <- base_data_clean %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      QuantiteConsommeeKG = sum(QuantiteConsommeeKG, na.rm = TRUE),
      ValeurConsommee = sum(ValeurConsommee, na.rm = TRUE),
      CaloriesParTete = mean(CaloriesParTete, na.rm = TRUE),
      .groups = "drop"
    )

  region_sf <- load_regional_shapes(path = shapefile_path)
  spatial_data <- integrate_data_with_shapes(regional_stats, region_sf)

  create_professional_map(
    spatial_data,
    var = variable,
    title = title,
    legend_title = legend_title,
    palette = palette
  )
}


#' Crée une carte interactive Leaflet avancée
#'
#' @param spatial_data Données spatiales (sf)
#' @param var Variable numérique à cartographier
#' @param popup_vars Colonnes à afficher dans le popup
#' @param popup_labels Libellés des colonnes (même longueur que popup_vars)
#' @param palette Palette de couleurs
#' @param legend_title Titre de la légende
#' @param show_layer_control Ajouter un contrôle de couches
#'
#' @return Un objet leaflet
#' @export
create_interactive_map <- function(spatial_data,
                                   var,
                                   popup_vars = NULL,
                                   popup_labels = NULL,
                                   palette = "YlOrRd",
                                   legend_title = NULL,
                                   show_layer_control = TRUE) {

  pkgs <- c("leaflet", "sf", "viridis", "purrr", "dplyr")
  lapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Le package '%s' est requis mais n'est pas installé.", pkg))
    }
  })

  spatial_data <- sf::st_transform(spatial_data, 4326)

  pal <- leaflet::colorNumeric(
    palette = palette,
    domain = spatial_data[[var]],
    na.color = "transparent"
  )

  if (is.null(popup_vars)) popup_vars <- c("region", var)
  popup_labels_vec <- if (!is.null(popup_labels) && length(popup_labels) == length(popup_vars)) {
    popup_labels
  } else popup_vars

  popup_content <- purrr::map_chr(1:nrow(spatial_data), function(i) {
    html <- "<div style='font-size: 12px;'>"
    for (j in seq_along(popup_vars)) {
      val <- spatial_data[[popup_vars[j]]][i]
      val <- ifelse(is.numeric(val), round(val, 2), val)
      html <- paste0(html, "<strong>", popup_labels_vec[j], ":</strong> ", val, "<br/>")
    }
    paste0(html, "</div>")
  })

  map <- leaflet::leaflet(spatial_data) %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Fond clair") %>%
    leaflet::addProviderTiles("CartoDB.DarkMatter", group = "Fond sombre") %>%
    leaflet::addPolygons(
      fillColor = ~pal(spatial_data[[var]]),
      fillOpacity = 0.8,
      weight = 1,
      color = "white",
      popup = popup_content,
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#333",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    leaflet::addLegend(
      pal = pal,
      values = spatial_data[[var]],
      position = "bottomright",
      title = legend_title %||% var
    )

  if (show_layer_control) {
    map <- map %>% leaflet::addLayersControl(
      baseGroups = c("Fond clair", "Fond sombre"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  }
  return(map)
}


#' Génère une carte interactive à partir de données de consommation
#'
#' @param base_data Données brutes avec colonnes `region`
#' @param shapefile_path Chemin vers un shapefile personnalisé
#' @param variable Variable à représenter
#' @param popup_vars Colonnes pour popup
#' @param popup_labels Libellés pour popup
#' @param palette Palette de couleurs
#' @param legend_title Titre de la légende
#'
#' @return Un objet leaflet
#' @export
generate_interactive_map <- function(base_data,
                                     shapefile_path = NULL,
                                     variable = "QuantiteConsommeeKG",
                                     popup_vars = c("region", "ValeurConsommee", "CaloriesParTete"),
                                     popup_labels = c("Région", "Valeur (F CFA)", "Calories Moyennes"),
                                     palette = "YlOrRd",
                                     legend_title = "Valeur") {

  require_packages(c("sf", "dplyr", "stringr", "haven", "leaflet", "viridis", "purrr"))

  base_data_clean <- base_data %>%
    dplyr::mutate(region = haven::as_factor(region)) %>%
    dplyr::mutate(region = stringr::str_replace_all(as.character(region), "-", " ")) %>%
    dplyr::mutate(region = stringr::str_trim(toupper(region))) %>%
    dplyr::filter(!is.na(region))

  regional_stats <- base_data_clean %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      QuantiteConsommeeKG = sum(QuantiteConsommeeKG, na.rm = TRUE),
      ValeurConsommee = sum(ValeurConsommee, na.rm = TRUE),
      CaloriesParTete = mean(CaloriesParTete, na.rm = TRUE),
      .groups = "drop"
    )

  region_sf <- load_regional_shapes(path = shapefile_path)
  spatial_data <- integrate_data_with_shapes(regional_stats, region_sf)

  create_interactive_map(
    spatial_data,
    var = variable,
    popup_vars = popup_vars,
    popup_labels = popup_labels,
    palette = palette,
    legend_title = legend_title
  )
}
