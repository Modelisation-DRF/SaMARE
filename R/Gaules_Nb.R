

#'
#'
#' @param RecGaules Recrutement Gaules
#' @param t
#' @param Rec    Nombre de  Recrues
#' @param trt  Variable du peuplement residuel avec condition que si St >26 = TEM
#' @param t0_aj_     Temps depuis coupe
#' @param altitude l'altitude une Variables de classification écologiques
#' @param Iterj  l'iteration souhaité
#' @param RandomPlacGaules  placette de RandomGaules choisie
#' @param st_tot0
#' @param latitude
#' @param longitude
#' @param temp
#' @param pente
#' @param Para.nb_gaules
#' @return
#' @examples


nb_Gaules<-function(Rec,RecGaules,t,st_tot0,altitude,latitude,trt,t0_aj_,
                    longitude,temp,pente,Iterj,RandomPlacGaules,Para.nb_gaules){


  # Construction matrice X
  XNb_Gaules<-matrix(0,ncol=10,nrow=1)
  XNb_Gaules[,1]<-1
  XNb_Gaules[,2]<-log(sum(RecGaules$Nb_Gaules_Ess_Ha)+1)
  XNb_Gaules[,3]<-log(sum(Rec$NbHa))
  XNb_Gaules[,4]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="HEG")]
  XNb_Gaules[,5]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="ERS")]
  XNb_Gaules[,6]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="BOJ")]
  XNb_Gaules[,7]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="SAB")]
  XNb_Gaules[,8]<-(trt=="TEM")*1
  XNb_Gaules[,9]<-ifelse(trt=="CP",t0_aj_,0)
  XNb_Gaules[,10]<-t


  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaNb_Gaules<-Para.nb_gaules  %>%
              filter(Iter==Iterj)
  # Construction matrice beta
  BetaMat<-matrix(ParaNb_Gaules$ParameterEstimate,ncol=1)

  # Calcul
  logit <-XNb_Gaules %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==11 )]

  pred<-exp(logit+Random)


  return(pred)

}
