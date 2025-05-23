% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gaules_evolution.R
\name{pi68SAB}
\alias{pi68SAB}
\title{Fonction qui calcul la probabilité d'absence de Gaules de sapin baumier classes de 6 ou 8 cm de diamètre.
Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
Les prévisions sont basées sur un modèle de type Zero inflated}
\usage{
pi68SAB(RecGaules, Ratio, Rec, RandomPlacGaules, Para.68_SAB)
}
\arguments{
\item{RecGaules}{Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha par Placette/GrEspece}

\item{Ratio}{Dataframe qui contient les variables Nb_Gaules_Ha et lnNb_Gaules_Ess_Ha par Placette/GrEspece}

\item{Rec}{Dataframe qui contient les variables dens_tot0 par placette}

\item{RandomPlacGaules}{Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules}

\item{Para.68_SAB}{Paramètres de l'équation de prévision du nombre de gaules de sapin baumier de 6 et 8 cm de diamètre}
}
\value{
Retourne un data d'une ligne par placette avec la colonne pi_68SAB et xb_pi_68SAB
}
\description{
Fonction qui calcul la probabilité d'absence de Gaules de sapin baumier classes de 6 ou 8 cm de diamètre.
Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
Les prévisions sont basées sur un modèle de type Zero inflated
}
