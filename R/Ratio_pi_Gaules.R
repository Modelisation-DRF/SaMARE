#'Fonction qui qui prévoit la proportion du nombre de gaules par espèce pour la période
#'de simulation suivante. Cette fonction est la première partie du modèle zero-inflated
#'de Rijal et al. 2023 et prévoit une probabilité d'absence de gaules par
#'groupe d'espèce.
#'
#'
#' @param RecGaules Dataframe qui contient les informations sur la distribution
#'                 des gaules dans la placette.
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param Rec   Un dataframe qui contient la prévision du nombre de recrues
#'               par groupes d'espèces.
#' @param Iterj  Itération en cours.
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à
#'                          l'échelle de la placette du module de
#'                          recrutement basé sur les gaules et du module d'évolution des gaules.
#' @param st_tot0 Surface terrière marchande de la placette au début du pas
#'                de simulation.
#' @param latitude Latitude de la placette en degrés décimal.
#' @param longitude Longitude de la placette en degrés décimal.
#' @param Ratio Dataframe qui possède une ligne par groupe d'espèces dans lequel
#'             les prévisions de ratios seront rapportées.
#' @param Para.ratio_gaules Paramètres de l'équation de la prévision du ratio du
#'                         nombre de gaules par espèce.
#' @return  Retourne une probabilité d'absence de gaules par groupe d'espèces.
#' @examples


ratio_pi_Gaules<-function(Ratio,Rec,RecGaules,t,st_tot0,latitude,longitude,
                          Iterj,RandomPlacGaules,Para.ratio_gaules){
  select=dplyr::select

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
