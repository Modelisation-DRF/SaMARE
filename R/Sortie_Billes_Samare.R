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
SortieBillesSamare<- function(SimulHtVolBillon){

  BillesSamareLigne <- SimulHtVolBillon %>%
                        mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE)  %>%
                        rename(origTreeID=NoArbre, Hautm=hauteur_pred,ST_m2ha=Stm2ha, Vol_dm3=vol_dm3) %>%
                        relocate(Placette,Annee,Iter, Residuel,ArbreID,origTreeID,Espece,GrEspece,Etat,
                                 Nombre,DHPcm,Hautm,ST_m2ha,Vol_dm3)



  return (BillesSamareLigne)

}
