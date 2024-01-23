#' Fonction qui structure un dataframe de sortie pour lequels on rapporte en format colone
#' pour les essences feuillues le volume prévu pour chacune des classe de produits
#' issues de la classification Petro.

#' @param SimulHtVolBillon Un dataframe de sortie de la fonction de billonnage.
#' @return Retourne un dataframe contenant une ligne par arbre, placette, annee,
#'         iteration, incluant en colone le volume en dm3 de chacune des classes
#'         de produit Petro des arbres de groupe d'essence feuillus.
#' @examples

SortieBillesSamareColone <- function(SimulHtVolBillon){

  BillesSamareColone <- SimulHtVolBillon %>%  mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE,vigu0 =NULL, prod0=NULL, Nb_Gaules_Ha = NULL,
                                                      Nb_Gaules_ERS = NULL,
                                                      Nb_Gaules_BOJ=NULL,Nb_Gaules_SAB =NULL, Nb_Gaules_68_ERS=NULL,
                                                      Nb_Gaules_68_HEG=NULL,Nb_Gaules_68_BOJ=NULL,Nb_Gaules_68_SAB=NULL,
                                                      Nb_Gaules_68_Ha =NULL, Nb_Gaules_HEG=NULL, Sup_PE =NULL,
                                                      reg_eco=NULL, Type_Eco=NULL, Altitude =NULL, Ptot=NULL, Tmoy=NULL,
                                                      veg_pot=NULL, milieu=NULL, nb_tige=NULL  )  %>%
    relocate(Annee, PlacetteID,Residuel,ArbreID,NoArbre) %>%
    pivot_wider(names_from = Produit, values_from = VolBillonM3) %>%
    select_if(~ !all(is.na(.))) %>%
    select(c(Placette,Annee,Iter,PlacetteID, Residuel,ArbreID,NoArbre,Espece,GrEspece,Etat
             , Nombre,DHPcm,hauteur_pred,Stm2ha,vol_dm3, F1,F2,F3,F4,P,DER))




  return (BillesSamareColone)

}

