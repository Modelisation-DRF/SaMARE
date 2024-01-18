

#'
#'
#' @param RecGaules Recrutement Gaules
#' @param t
#' @param Rec    Nombre de  Recrues
#' @param trt  Variable du peuplement residuel avec condition que si St >26 = TEM
#' @param t0_aj_     Temps depuis coupe
#' @param prec
#' @param Iterj  l'iteration souhaité
#' @param RandomPlacGaules  placette de RandomGaules choisie
#' @param st_tot0
#' @param latitude
#' @param longitude
#' @param Ratio
#' @param Para.ratio_gaules
#' @return
#' @examples


ratio_count_Gaules<-function(Ratio,Rec,RecGaules,t,st_tot0,latitude,longitude,
                          prec,trt,t0_aj_,Iterj,RandomPlacGaules,Para.ratio_gaules){

  n<-nrow(Ratio)

  #Liste des effets


  listeGrEss1<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
                 rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))


  # Construction matrice X
  XRatio_count<-matrix(0,ncol=18,nrow=n)
  XRatio_count[,1]<-RecGaules$Ratio
  XRatio_count[,2]<-RecGaules$lnNb_Gaules_Ess_Ha
  XRatio_count[,3]<-st_tot0
  XRatio_count[,4]<-log(sum(Rec$NbHa))
  XRatio_count[1,5]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="AUT")]
  XRatio_count[2,6]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="BOJ")]
  XRatio_count[3,7]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="EPX")]
  XRatio_count[4,8]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="ERR")]
  XRatio_count[5,9]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="ERS")]
  XRatio_count[6,10]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="FEN")]
  XRatio_count[7,11]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="FIN")]
  XRatio_count[8,12]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="HEG")]
  XRatio_count[9,13]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="RES")]
  XRatio_count[10,14]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="SAB")]
  XRatio_count[,15]<-prec
  XRatio_count[,16]<-ifelse(trt=="CP",t0_aj_,0)
  XRatio_count[,17]<-t
  XRatio_count[,18]<-longitude


  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRatio_count<-Para.ratio_gaules %>%
              filter(Iter==Iterj & response=="count")
  # Construction matrice beta
  BetaMat<-matrix(ParaRatio_count$ParameterEstimate,ncol=1)

  # Calcul
  logit <-XRatio_count %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==12 & RandomPlacGaules$response=="count")]

  pred<-1/(1+exp(-(logit+Random)))

  return(pred)

}
