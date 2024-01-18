

#' Parametres nombre de gaules de 6 et 8 cm SAB
#'
#' @param RecGaules Recrutement Gaules
#' @param Ratio   Ratio Gaules
#' @param dens_tot0 densité total
#' @param Iterj  l'iteration souhaité
#' @param RandomPlacGaules  placette de RandomGaules choisie
#' @param Para.68_BOJ nombre de gaules de 6 et 8 cm SAB
#' @return
#' @examples

pi68SAB<-function(RecGaules,Ratio,dens_tot0,Iterj,RandomPlacGaules,Para.68_SAB){


  # Construction matrice X
  X68SAB_pi<-matrix(0,ncol=4,nrow=1)
  X68SAB_pi[,1]<-log(Ratio$Nb_Gaules_Ha[which(Ratio$GrEspece=="SAB")])
  X68SAB_pi[,2]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="SAB")]
  X68SAB_pi[,3]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="SAB")]
  X68SAB_pi[,4]<-log(dens_tot0)


  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  Para68SAB_pi<-Para.68_SAB %>%
              filter(Iter==Iterj & response=="pi")
  # Construction matrice beta
  BetaMat<-matrix(Para68SAB_pi$ParameterEstimate,ncol=1)

  # Calcul
  logit <-X68SAB_pi %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==16 & RandomPlacGaules$response=="pi")]

  pred<-1/(1+exp(-(logit+Random)))

  return(pred)

}

count68SAB<-function(RecGaules,Ratio,Rec,trt,t0_aj_,dens_tot0,Iterj,RandomPlacGaules,Para.68_SAB){


  # Construction matrice X
  X68SAB_count<-matrix(0,ncol=6,nrow=1)
  X68SAB_count[,1]<-1
  X68SAB_count[,2]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="SAB")]
  X68SAB_count[,3]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="SAB")]
  X68SAB_count[,4]<-log(dens_tot0)
  X68SAB_count[,5]<-Rec$St_Ess_Ha[which(Rec$GrEspece=="SAB")]
  X68SAB_count[,6]<-ifelse(trt=="CP",t0_aj_,0)




  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  Para68SAB_count<-Para.68_SAB %>%
                   filter(Iter==Iterj & response=="count")
  # Construction matrice beta
  BetaMat<-matrix(Para68SAB_count$ParameterEstimate,ncol=1)

  # Calcul
  logit <-X68SAB_count %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==16 & RandomPlacGaules$response=="count")]

  pred<-exp(logit+Random)-1

  return(pred)

}
