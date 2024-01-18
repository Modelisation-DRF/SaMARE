
#'
#'
#' @param RecGaules Recrutement Gaules
#' @param t
#' @param Rec    Nombre de  Recrues
#' @param Iterj  l'iteration souhaité
#' @param RandomPlacGaules  placette de RandomGaules choisie
#' @param st_tot0
#' @param latitude
#' @param longitude
#' @param Ratio
#' @param Para.ratio_gaules
#' @return
#' @examples


ratio_pi_Gaules<-function(Ratio,Rec,RecGaules,t,st_tot0,latitude,longitude,
                          Iterj,RandomPlacGaules,Para.ratio_gaules){

  n<-nrow(Ratio)

  #Liste des effets


  listeGrEss1<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),
                 rep("FEN",n),rep("FIN",n),rep("SAB",n))


  # Construction matrice X
  XRatio_pi<-matrix(0,ncol=11,nrow=n)
  XRatio_pi[,1]<-RecGaules$lnNb_Gaules_Ess_Ha
  XRatio_pi[,2:8]<-(Ratio$GrEspece==listeGrEss1)*Rec$lnNb_Ess_Ha
  XRatio_pi[,9]<-t
  XRatio_pi[,10]<-longitude
  XRatio_pi[,11]<-latitude

  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRatio_pi<-Para.ratio_gaules %>%
              filter(Iter==Iterj & response=="pi")
  # Construction matrice beta
  BetaMat<-matrix(ParaRatio_pi$ParameterEstimate,ncol=1)

  # Calcul
  logit <-XRatio_pi %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==12 & RandomPlacGaules$response=="pi")]

  pred<-1/(1+exp(-(logit+Random)))

  return(pred)

}
