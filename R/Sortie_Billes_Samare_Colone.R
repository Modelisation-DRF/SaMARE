#' Fonction qui structure un dataframe de sortie pour lequels on rapporte en format colone
#' pour les essences feuillues le volume prévu pour chacune des classe de produits
#' issues de la classification Petro.

#' @param SimulHtVolBillon Un dataframe de sortie de la fonction de billonnage.
#' @return Retourne un dataframe contenant une ligne par arbre, placette, annee,
#'         iteration, incluant en colone le volume en dm3 de chacune des classes
#'         de produit Petro des arbres de groupe d'essence feuillus.
#' @examples

SortieBillesSamareColone <- function(SimulHtVolBillon){
  select=dplyr::select
  BillesSamareColone <- SimulHtVolBillon %>%  mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE )  %>%
    relocate(Annee, PlacetteID,Residuel,ArbreID,NoArbre) %>%
    pivot_wider(names_from = Produit, values_from = VolBillonM3) %>%
    select(c(Placette,Annee,Iter,PlacetteID, Residuel,ArbreID,NoArbre,Espece,GrEspece,Etat
       , Nombre,DHPcm,hauteur_pred,Stm2ha,vol_dm3, F1,F2,F3,F4,P))




  return (BillesSamareColone)

}

