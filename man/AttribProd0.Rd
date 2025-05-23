% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AttributionProd0.R
\name{AttribProd0}
\alias{AttribProd0}
\title{Fonction qui extrait le produit à partir du code de vigueur au début de la simulation.
Si pas de code de vigueur, utilise les équations de conversion MSCR à produit}
\usage{
AttribProd0(Data, Para.ConvMSCRProd1024, Para.ConvMSCRProd24)
}
\arguments{
\item{Data}{Un dataframe contenant une ligne par arbre avec au minimum les colonnes Placette, ArbreID, Vigueur, GrEspece, MSCR, DHPcm}

\item{Para.ConvMSCRProd1024}{Un dataframe  contenant les paramettres des équations
de conversion du classement MSCR en produits pour les arbres de moins de 23.1 cm.}

\item{Para.ConvMSCRProd24}{Un dataframe  contenant les paramettres des équations
de conversion du classement MSCR en produits pour les arbres de plus de 23 cm.}
}
\value{
Retourne Data avec la colonne prod0 ("sciage" ou "pate" ou "resineux").
}
\description{
Fonction qui extrait le produit à partir du code de vigueur au début de la simulation.
Si pas de code de vigueur, utilise les équations de conversion MSCR à produit
}
