
#' Parametres nombre de gaules de 6 et 8 cm BOJ
#'
#' @param RecGaules Recrutement Gaules
#' @param Ratio   Ratio Gaules
#' @param Rec    Nombre de  Recrues
#' @param trt  Variable du peuplement residuel avec condition que si St >26 = TEM
#' @param t0_aj_     Temps depuis coupe
#' @param altitude l'altitude une Variables de classification écologiques
#' @param Iterj  l'iteration souhaité
#' @param RandomPlacGaules  placette de RandomGaules choisie
#' @param Para.68_BOJ nombre de gaules de 6 et 8 cm BOJ
#' @return
#' @examples


pi68BOJ<-function(RecGaules,Ratio,Rec,trt,t0_aj_,altitude,Iterj,RandomPlacGaules,Para.68_BOJ){


  # Construction matrice X
  X68BOJ_pi<-matrix(0,ncol=8,nrow=1)
  X68BOJ_pi[,1]<-1
  X68BOJ_pi[,2]<-log(Ratio$Nb_Gaules_Ha[which(Ratio$GrEspece=="BOJ")])
  X68BOJ_pi[,3]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="BOJ")]
  X68BOJ_pi[,4]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="BOJ")]
  X68BOJ_pi[,5]<-Rec$St_Ess_Ha[which(Rec$GrEspece=="BOJ")]
  X68BOJ_pi[,6]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="BOJ")]
  X68BOJ_pi[,7]<-ifelse(trt=="CP",t0_aj_,0)
  X68BOJ_pi[,8]<-altitude


  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  Para68BOJ_pi<-Para.68_BOJ %>%
              filter(Iter==Iterj & response=="pi")
  # Construction matrice beta
  BetaMat<-matrix(Para68BOJ_pi$ParameterEstimate,ncol=1)

  # Calcul
  logit <-X68BOJ_pi %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==15 & RandomPlacGaules$response=="pi")]

  pred<-1/(1+exp(-(logit+Random)))

  return(pred)

}

count68BOJ<-function(RecGaules,Ratio,t,trt,t0_aj_,latitude,Iterj,RandomPlacGaules,Para.68_BOJ){


  # Construction matrice X
  X68BOJ_count<-matrix(0,ncol=7,nrow=1)
  X68BOJ_count[,1]<-1
  X68BOJ_count[,2]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="BOJ")]
  X68BOJ_count[,3]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="BOJ")]
  X68BOJ_count[,4]<-t
  X68BOJ_count[,5]<-ifelse(trt=="TEM",1,0)
  X68BOJ_count[,6]<-ifelse(trt=="CP",t0_aj_,0)
  X68BOJ_count[,7]<-latitude



  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  Para68BOJ_count<-Para.68_BOJ %>%
                   filter(Iter==Iterj & response=="count")
  # Construction matrice beta
  BetaMat<-matrix(Para68BOJ_count$ParameterEstimate,ncol=1)

  # Calcul
  logit <-X68BOJ_count %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==15 & RandomPlacGaules$response=="count")]

  pred<-exp(logit+Random)-1

  return(pred)

}
