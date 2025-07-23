---
title: "README"
output: html_document
---

# SenegalSpatialFish <img src="man/figures/logo.png" align="right" width="120"/>

**`SenegalSpatialFish`** est un package R conçu pour **visualiser des données de consommation ou d'indicateurs socio-économiques au Sénégal**, en combinant des données tabulaires avec des limites géographiques des régions (incluses dans le package).

## ✨ Points forts

- **Cartes statiques (ggplot2)** prêtes pour les rapports.
- **Cartes interactives (Leaflet)** avec popups personnalisables, contrôle des couches et palettes de couleurs.
- **Shapefile intégré des régions** du Sénégal (plus besoin de fichiers externes).
- **Fonctions simples et intuitives** pour intégrer vos données tabulaires avec les données spatiales.

---

## 🚀 Installation

### Depuis GitHub

Installez le package avec :

```r
# Installer depuis GitHub
remotes::install_github("DavidLSanam/SenegalSpatialFish")
```

## 📦 Données intégrées

Le package inclut le shapefile des régions du Sénégal (extdata/Limite_Region.shp), ce qui évite les téléchargements externes.


## Dépendances
*SenegalSpatialFish* repose sur les packages R suivants :
sf, dplyr, ggplot2, leaflet, viridis, purrr, rlang, stringr, haven.


## ✨ Fonctions principales

*1. Cartes statiques*

Créer une carte statique professionnelle à partir de vos données régionales :

library(SenegalSpatialFish)
df <- read_dta("BASE_X1_Appuree.dta")

generate_spatial_map(
  df,
  variable = "QuantiteConsommeeKG",
  title = "Consommation par région (kg)",
  legend_title = "Kg consommés",
  palette = "plasma"
)


*2. Cartes interactives*

Créer une carte interactive Leaflet avec popups personnalisés :

generate_interactive_map(
  df,
  variable = "QuantiteConsommeeKG",
  popup_vars = c("region", "ValeurConsommee", "CaloriesParTete"),
  popup_labels = c("Région", "Valeur (F CFA)", "Calories Moyennes"),
  palette = "YlOrRd",
  legend_title = "Kg consommés"
)


## 🛠 Fonctions utilitaires

load_regional_shapes() : Charge le shapefile des régions.

integrate_data_with_shapes() : Fusionne les données tabulaires avec la géométrie.

create_professional_map() : Génère une carte statique avec ggplot2.

create_interactive_map() : Génère une carte interactive avancée avec leaflet.


## 🖼 Exemple de workflow complet

library(SenegalSpatialFish)

# Charger la base brute
setwd("chemin vers BASE_X1_Appuree.dta")
df <- read_dta("BASE_X1_Appuree.dta") // Ou une base de même type depuis votre PC

# Générer une carte interactive
map <- generate_interactive_map(
  df,
  variable = "QuantiteConsommeeKG",
  popup_vars = c("region", "ValeurConsommee", "CaloriesParTete"),
  popup_labels = c("Région", "Dépense de consommation (FCFA)", "Apport calorique moyen par tête"),
  palette = "plasma",
  legend_title = "Quantité consommée (kg)"
)

map

#### Vous avez bien-sûr la possibilité de modifier le "popup_labels" selon votre convenance.



## 📄 Licence
Ce package est sous licence MIT.
Vous êtes libre de l'utiliser, de le modifier et de le redistribuer, en citant les auteurs.


## ✍️ Auteurs
David Landry SANAM
Contact : [landrysanam1@gmail.com] ou via GitHub @DavidLSanam.

Leslye Patricia NKWA
Contact : [] ou via GitHub

Herman Parfait YAMAHA
Contact : [] ou via GitHub

Michel TEVOEDJRE
Contact : [] ou via GitHub
