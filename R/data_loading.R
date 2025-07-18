#' Charger les limites régionales du Sénégal
#'
#' @param path Chemin vers le shapefile. Si NULL, utilise celui inclus dans le package.
#' @return Un objet sf avec les régions
#' @export
load_regional_shapes <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "Limite_Région.shp", package = "SenegalSpatialFish")
  }

  if (!file.exists(path)) {
    stop("Shapefile non trouvé.")
  }

  regions_sf <- sf::st_read(path, quiet = TRUE) %>%
    sf::st_transform(4326) %>%
    sf::st_make_valid() %>%
    dplyr::mutate(region = as.character(NOMREG))%>%
    dplyr::select(region, geometry)

  return(regions_sf)
}
