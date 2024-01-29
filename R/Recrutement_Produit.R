#'Fonction qui prevoit le prédicteur linéaire de l'équation de prévision
#'de la probabilité qu'une recrue de groupe d'essence feuillue ait un potentiel
#'de production de sciage.
#'
#' @param RecSelect Un dataframe qui contient la prévision du nombre de recrues
#'                  par groupe d'espèce.
#' @param type_pe_Plac Variable indicatrice de la taille de la placette soit
#'                      400 m2, soit entre 2500 et 5000 m2 inclusivement ou
#'                      soit une autre dimension.
#' @param rid1  Variable de groupement de variables écologiques.
#' @param Iterj Itération en cours.
#' @param Para.rec_prod  Paramètres de l'équation de la probabilité de présence
#'                      de potentiel sciage des recrues de groupe d'essence feuillus.
#' @return Retourne le prédicteur linéaire de l'équation de prévision de la
#'         probabilité de présence de potentiel sciage pour les recrues de groupes
#'         d'essences feuillues.
#' @examples
#'

rec_prod<-function(RecSelect,type_pe_Plac,rid1,Iterj,Para.rec_prod){
  select=dplyr::select
  n<-nrow(RecSelect)

  #Liste des effets

  listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
              rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))
  listeTypePe<-c(rep("type0",n),rep("type1",n),rep("type2",n))
  listeRid1<-c(rep("2o",n),rep("3a",n),rep("3b",n),rep("3c",n),rep("3d",n),
               rep("4e",n),rep("4o",n),rep("DU",n),rep("SV",n))

  # Construction matrice X
  Xrec_prod<-matrix(0,ncol=23,nrow=n)
  Xrec_prod[,1]<-1
  Xrec_prod[,2:11]<-(RecSelect$GrEspece==listeEss)*1
  Xrec_prod[,12:14]<-(type_pe_Plac==listeTypePe)*1
  Xrec_prod[,15:23]<-(rid1==listeRid1)*1


  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_prod<-Para.rec_prod %>%
    filter(Iter==Iterj)

  # Construction matrice beta
  BetaMat<-matrix(ParaRec_prod$ParameterEstimate,ncol=1)

  # Calcul accroissement
  Xbeta <-Xrec_prod %*% BetaMat

  return(Xbeta)

}
