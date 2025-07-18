#' Charge une liste de packages requis
#'
#' @param packages Vecteur de noms de packages à charger
#' @return Charge les packages s’ils sont installés, ou affiche un message d’erreur clair
require_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Le package", pkg, "est requis. Installez-le avec install.packages('", pkg, "')"))
    }
  }
}
