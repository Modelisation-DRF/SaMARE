
#'
#'
#' @param RecGaules Recrutement Gaules
#' @param t
#' @param Rec    Nombre de  Recrues
#' @param Iterj  l'iteration souhaité
#' @param RandomPlacGaules  placette de RandomGaules choisie
#' @param st_tot0
#' @param Para.rec_gaules
#' @return
#' @examples


rec_pi_Gaules<-function(Rec,RecGaules,t,st_tot0,Iterj,RandomPlacGaules,Para.rec_gaules){

  n<-nrow(Rec)

  #Liste des effets

  listeGrEss1<-c(rep("AUT",n),rep("EPX",n),rep("ERR",n),rep("FEN",n),
                 rep("FIN",n),rep("HEG",n),rep("RES",n))

  listeGrEss2<-c(rep("ERS",n),rep("HEG",n),rep("BOJ",n),rep("SAB",n))

  # Construction matrice X
  Xrec_pi<-matrix(0,ncol=14,nrow=n)
  Xrec_pi[,1]<-st_tot0
  Xrec_pi[,2]<-t
  Xrec_pi[,3:9]<-(Rec$GrEspece==listeGrEss1)*Rec$lnNb_Ess_Ha
  Xrec_pi[,10]<-(RecGaules$GrEspece=="BOJ")*RecGaules$lnNb_Gaules_Ess_Ha
  Xrec_pi[,11:14]<-(RecGaules$GrEspece==listeGrEss2)*RecGaules$lnNb_Gaules_68_Ess_Ha

  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_pi<-Para.rec_gaules %>%
              filter(Iter==Iterj & response=="pi")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_pi$ParameterEstimate,ncol=1)

  # Calcul
  logit <-Xrec_pi %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==10 & RandomPlacGaules$response=="pi")]

  pred<-1/(1+exp(-(logit+Random)))

  return(pred)

}
