
#' Parametres nombre de gaules de 6 et 8 cm ERS
#'
#' @param RecGaules Recrutement Gaules
#' @param Ratio   Ratio Gaules
#' @param Iterj  l'iteration souhaité
#' @param RandomPlacGaules  placette de RandomGaules choisie
#' @param Para.68_ERS nombre de gaules de 6 et 8 cm ERS
#' @return
#' @examples




pi68ERS<-function(RecGaules,Ratio,Iterj,RandomPlacGaules,Para.68_ERS){


  # Construction matrice X
  X68ERS_pi<-matrix(0,ncol=4,nrow=1)
  X68ERS_pi[,1]<-1
  X68ERS_pi[,2]<-log(Ratio$Nb_Gaules_Ha[which(Ratio$GrEspece=="ERS")])
  X68ERS_pi[,3]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="ERS")]
  X68ERS_pi[,4]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="ERS")]


  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  Para68ERS_pi<-Para.68_ERS %>%
              filter(Iter==Iterj & response=="pi")
  # Construction matrice beta
  BetaMat<-matrix(Para68ERS_pi$ParameterEstimate,ncol=1)

  # Calcul
  logit <-X68ERS_pi %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==13 & RandomPlacGaules$response=="pi")]

  pred<-1/(1+exp(-(logit+Random)))

  return(pred)

}

count68ERS<-function(RecGaules,Ratio,dens_tot0,grwd,Iterj,RandomPlacGaules,Para.68_ERS){


  # Construction matrice X
  X68ERS_count<-matrix(0,ncol=5,nrow=1)
  X68ERS_count[,1]<-log(Ratio$Nb_Gaules_Ha[which(Ratio$GrEspece=="ERS")])
  X68ERS_count[,2]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="ERS")]
  X68ERS_count[,3]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="ERS")]
  X68ERS_count[,4]<-log(dens_tot0)
  X68ERS_count[,5]<-grwd




  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  Para68ERS_count<-Para.68_ERS %>%
                   filter(Iter==Iterj & response=="count")
  # Construction matrice beta
  BetaMat<-matrix(Para68ERS_count$ParameterEstimate,ncol=1)

  # Calcul
  logit <-X68ERS_count %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==13 & RandomPlacGaules$response=="count")]

  pred<-exp(logit+Random)-1

  return(pred)

}
