% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gaules_evolution.R
\name{pi68HEG}
\alias{pi68HEG}
\title{Fonction qui calcul la probabilité d'absence de Gaules de hêtre à grande feuille classes de 6 ou 8 cm de diamètre.
Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
Les prévisions sont basées sur un modèle de type Zero inflated}
\usage{
pi68HEG(RecGaules, Ratio, Rec, RandomPlacGaules, Para.68_HEG)
}
\arguments{
\item{RecGaules}{Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha par Placette/GrEspece}

\item{Ratio}{Dataframe qui contient la variable lnNb_Gaules_Ess_Ha Placette/GrEspece}

\item{Rec}{Dataframe avec la colonne Placette}

\item{RandomPlacGaules}{Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules}

\item{Para.68_HEG}{Paramètres de l'équation de prévision du nombre de gaules de hêtre à grande feuille de 6 et 8 cm de diamètre}
}
\value{
Retourne un data d'une ligne par placette avec la colonne pi_68HEG et xb_pi_68HEG
}
\description{
Fonction qui calcul la probabilité d'absence de Gaules de hêtre à grande feuille classes de 6 ou 8 cm de diamètre.
Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
Les prévisions sont basées sur un modèle de type Zero inflated
}
