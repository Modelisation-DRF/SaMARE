


#' @param RecSelect
#' @param latitude
#' @param Iterj
#' @param Para.rec_vig
#' @return
#' @examples
#'



rec_vig<-function(RecSelect,latitude,Iterj,Para.rec_vig){

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
