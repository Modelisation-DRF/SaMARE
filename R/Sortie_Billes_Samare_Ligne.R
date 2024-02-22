#' Fonction qui structure un dataframe de sortie pour lequels on rapporte en format ligne
#' pour les essences feuillues le volume prévu pour chacune des classe de produits
#' issues de la classification Petro.
#'
#' @param SimulHtVolBillon Un dataframe de sortie de la fonction de billonnage.
#' @return  Retourne un dataframe contenant pour chaque arbre, placette, annee,
#'         iteration et classe de produits une ligne présentant le volume en dm3
#'         de chacune des classes de produit Petro des arbres de groupe d'essence
#'         feuillus. Les arbres d'essence résineuses sont présentés avec des
#'         valeures nulles de produits et de volume de produits.
#' @examples
#'
SortieBillesSamareLigne <- function(SimulHtVol){
  select=dplyr::select
  BillesSamareLigne <- SimulHtVol %>%  mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE)  %>%
    select(c(Placette,Annee,Iter,PlacetteID, Residuel,ArbreID,NoArbre,Espece,GrEspece,Etat
             , Nombre,DHPcm,hauteur_pred,Stm2ha,vol_dm3))



  return (BillesSamareLigne)

}
