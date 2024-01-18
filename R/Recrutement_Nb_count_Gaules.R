
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


rec_count_Gaules<-function(Rec,RecGaules,t,st_tot0,Iterj,RandomPlacGaules,Para.rec_gaules){

  n<-nrow(Rec)

  #Liste des effets

  listeGrEss1<-c(rep("AUT",n),rep("FEN",n))
  listeGrEss2<-c(rep("AUT",n),rep("SAB",n))
  listeGrEss3<-c(rep("EPX",n),rep("ERR",n),rep("FEN",n),rep("FIN",n))
  listeGrEss4<-c(rep("ERS",n),rep("HEG",n),rep("BOJ",n),rep("SAB",n))

  # Construction matrice X
  Xrec_count<-matrix(0,ncol=14,nrow=n)
  Xrec_count[,1]<-1
  Xrec_count[,2]<-st_tot0
  Xrec_count[,3:4]<-(Rec$GrEspece==listeGrEss1)*Rec$St_Ess_Ha
  Xrec_count[,5:6]<-(Rec$GrEspece==listeGrEss2)*Rec$lnNb_Ess_Ha
  Xrec_count[,7:10]<-(RecGaules$GrEspece==listeGrEss3)*RecGaules$lnNb_Gaules_Ess_Ha
  Xrec_count[,11:14]<-(RecGaules$GrEspece==listeGrEss4)*RecGaules$lnNb_Gaules_68_Ess_Ha

  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_count<-Para.rec_gaules  %>%
              filter(Iter==Iterj & response=="count")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_count$ParameterEstimate,ncol=1)

  # Calcul
  logit <-Xrec_count %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==10 & RandomPlacGaules$response=="count")]

  pred<-exp(logit+Random)


  return(pred)

}
