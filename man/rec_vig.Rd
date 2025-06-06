% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Recrutement_Vigueur.R
\name{rec_vig}
\alias{rec_vig}
\title{Fonction qui prevoit si une recrue est vigoureuse ou non}
\usage{
rec_vig(RecSelect, Para.rec_vig, RandomRecVig, seed_value = NULL)
}
\arguments{
\item{RecSelect}{Un dataframe qui contient une liste d'arbres avec les colonnes Placette, GrEspece, latitude .}

\item{Para.rec_vig}{Paramètres de l'équation de la probabilité que les recrues
soit vigoureuses.}

\item{RandomRecVig}{Effets aléatoire du modèle de vigueur des recrues}

\item{seed_value}{Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.}
}
\value{
Retourne RecSelect avec les colonnes prob_vig_rec et vigu1.
}
\description{
Fonction qui prevoit si une recrue est vigoureuse ou non
}
