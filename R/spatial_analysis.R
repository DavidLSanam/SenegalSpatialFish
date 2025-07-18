#' Intègre les données de consommation avec les shapes régionaux
#'
#' @param consumption_data Données de consommation (data.frame avec une colonne "region")
#' @param region_sf Objet sf des régions (de load_regional_shapes)
#' @return Un sf dataframe enrichi
#' @export
integrate_data_with_shapes <- function(consumption_data, region_sf) {
  # Vérification des noms de colonnes
  if (!"region" %in% names(consumption_data)) {
    stop("La base de consommation doit contenir une colonne 'region'")
  }

  # Harmoniser les noms de région (majuscules et suppression d'espaces)
  consumption_data <- consumption_data %>%
    dplyr::mutate(region = stringr::str_trim(toupper(region)))

  region_sf <- region_sf %>%
    dplyr::mutate(region = stringr::str_trim(toupper(region)))

  # Jointure simple (non-spatiale)
  merged_data <- dplyr::left_join(region_sf, consumption_data, by = "region")

  return(merged_data)
}
