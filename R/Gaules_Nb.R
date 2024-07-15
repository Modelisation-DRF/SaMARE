#' Fonction qui prévoit le nombre total de gaules à la fin du pas de simulation.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
#'
#' @param Rec  Un dataframe qui contient la prévision du nombre de recrues par
#'             groupes d'espèces.
#' @param RecGaules Dataframe qui contient les information sur la distribution
#'                 des gaules dans la placette.
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param st_tot0 Surface terrière marchande (DHP >9.0cm) de la placette
#'                 au début du pas de simulation.
#' @param altitude Altitude de la placette.
#' @param latitude Latitude de la placette en degrés décimaux.
#' @param trt  Variable distinguant les peuplements traités des témoins, si St >26 = TEM
#' @param t0_aj_  Temps écoulé depuis la dernière coupe partielle
#' @param longitude Longitude de la placette en degrés décimaux.
#' @param temp Température annuelle moyenne de la placette.
#' @param pente Inclinaison de la pente  en pourcent.
#' @param Iterj  Itération en cours.
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à
#'                            l'échelle de la placette du module de
#'                            recrutement basé sur les gaules et du module
#'                           d'évolution des gaules.
#' @param Para.nb_gaules Paramètres de l'équation de la prévision du nombre total de gaules.
#' @return Retourne une prévision du nombre total de gaules à la fin du pas de simulation.
#' @examples
#'  #  prévoit le nombre total de gaules à la fin du pas de simulation
#' resultat <- nb_Gaules(Rec,RecGaules,t,st_tot0,altitude,latitude,trt,t0_aj_,longitude,temp,pente,Iterj,RandomPlacGaules,Para.nb_gaules)
#' print(resultat)
#'
#' @export


nb_Gaules<-function(Rec,RecGaules,t,st_tot0,altitude,latitude,trt,t0_aj_,
                    longitude,temp,pente,Iterj,RandomPlacGaules,Para.nb_gaules){

  select=dplyr::select

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
