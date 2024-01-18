
#' Parametres nombre de gaules de 6 et 8 cm HEG
#'
#' @param RecGaules Recrutement Gaules
#' @param Ratio   Ratio Gaules
#' @param Rec    Nombre de  Recrues
#' @param Iterj  l'iteration souhaité
#' @param RandomPlacGaules  placette de RandomGaules choisie
#' @param Para.68_HEG nombre de gaules de 6 et 8 cm HEG
#' @return
#' @examples

pi68HEG<-function(RecGaules,Ratio,Rec,Iterj,RandomPlacGaules,Para.68_HEG){


  # Construction matrice X
  X68HEG_pi<-matrix(0,ncol=3,nrow=1)
  X68HEG_pi[,1]<-1
  X68HEG_pi[,2]<-RecGaules$lnNb_Gaules_Ess_Ha[which(RecGaules$GrEspece=="HEG")]
  X68HEG_pi[,3]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="HEG")]



  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  Para68HEG_pi<-Para.68_HEG %>%
              filter(Iter==Iterj & response=="pi")
  # Construction matrice beta
  BetaMat<-matrix(Para68HEG_pi$ParameterEstimate,ncol=1)

  # Calcul
  logit <-X68HEG_pi %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==14 & RandomPlacGaules$response=="pi")]

  pred<-1/(1+exp(-(logit+Random)))

  return(pred)

}

count68HEG<-function(RecGaules,Ratio,Rec,Iterj,RandomPlacGaules,Para.68_HEG){


  # Construction matrice X
  X68HEG_count<-matrix(0,ncol=3,nrow=1)
  X68HEG_count[,1]<-1
  X68HEG_count[,2]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="HEG")]
  X68HEG_count[,3]<-RecGaules$lnNb_Gaules_24_Ess_Ha[which(RecGaules$GrEspece=="HEG")]





  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  Para68HEG_count<-Para.68_HEG %>%
                   filter(Iter==Iterj & response=="count")
  # Construction matrice beta
  BetaMat<-matrix(Para68HEG_count$ParameterEstimate,ncol=1)

  # Calcul
  logit <-X68HEG_count %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==14 & RandomPlacGaules$response=="count")]

  pred<-exp(logit+Random)

  return(pred)

}
