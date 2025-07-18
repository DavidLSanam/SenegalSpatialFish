% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping_functions.R
\name{generate_interactive_map}
\alias{generate_interactive_map}
\title{Génère directement une carte interactive Leaflet à partir d'une base de consommation brute}
\usage{
generate_interactive_map(
  base_data,
  shapefile_path = NULL,
  variable = "QuantiteConsommeeKG",
  popup_vars = c("ValeurConsommee", "CaloriesParTete"),
  palette = "YlOrRd"
)
}
\arguments{
\item{base_data}{La base de consommation brute (avec région labellisée)}

\item{shapefile_path}{Chemin vers le shapefile (optionnel si déjà intégré dans le package)}

\item{variable}{Nom de la variable à cartographier (ex: "QuantiteConsommeeKG")}

\item{popup_vars}{Variables à afficher dans les popups (ex: c("ValeurConsommee", "CaloriesParTete"))}

\item{palette}{Palette de couleurs (ex: "YlOrRd", "Viridis", "Blues", etc.)}
}
\value{
Un objet leaflet (carte interactive)
}
\description{
Génère directement une carte interactive Leaflet à partir d'une base de consommation brute
}
