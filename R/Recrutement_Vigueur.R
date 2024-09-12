#'Fonction qui prevoit le prédicteur linéaire de l'équation de prévision
#'de la probabilité qu'une recrue soit vigoureuse.
#'
#' @param RecSelect  Un dataframe qui contient la prévision du nombre de recrues
#'                  par groupe d'espèce.
#' @param latitude Latitude de la placette.
#' @param Iterj  Itération en cours.
#' @param Para.rec_vig  Paramètres de l'équation de la probabilité que les recrues
#'                     soit vigoureuses.
#' @return  Retourne le prédicteur linéaire de l'équation de prévision de la
#'         probabilité qu'une recrue soit vigoureuse.
#' @export
rec_vig<-function(RecSelect,latitude,Iterj,Para.rec_vig){
  select=dplyr::select
  n<-nrow(RecSelect)

  #Liste des effets

  listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
              rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))

  # Construction matrice X
  Xrec_vig<-matrix(0,ncol=12,nrow=n)
  Xrec_vig[,1]<-1
  Xrec_vig[,2:11]<-(RecSelect$GrEspece==listeEss)*1
  Xrec_vig[,12]<-latitude

  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_vig<-Para.rec_vig %>%
    filter(Iter==Iterj)

  # Construction matrice beta
  BetaMat<-matrix(ParaRec_vig$ParameterEstimate,ncol=1)

  # Calcul accroissement
  Xbeta <-Xrec_vig %*% BetaMat

  return(Xbeta)

}
