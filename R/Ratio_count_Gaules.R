#'Fonction qui qui prévoit la proportion du nombre de gaules par espèce pour la période
#'de simulation suivante. Cette fonction est la deuxième partie du modèle zero-inflated
#'de Rijal et al. 2023 et prévoit un ratio par groupe d'espèce lorsqu'il y a
#'présence de gaules.
#'
#' @param RecGaules Dataframe qui contient les informations sur la distribution
#'                 des gaules dans la placette.
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param Rec    Un dataframe qui contient la prévision du nombre de recrues
#'               par groupes d'espèces.
#' @param trt  Variable distinguant les peuplements traités des témoins,
#'              si St >26 = TEM.
#' @param t0_aj_  Temps écoulé depuis la dernière coupe partielle.
#' @param prec  Précipitations annuelles moyennes de la placette.
#' @param Iterj  Itération en cours.
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à
#'                          l'échelle de la placette du module de
#'                          recrutement basé sur les gaules et du module d'évolution des gaules.
#' @param st_tot0  Surface terrière marchande de la placette au début du pas
#'                de simulation.
#' @param latitude  Latitude de la placette en degrés décimal.
#' @param longitude  Longitude de la placette en degrés décimal.
#' @param Ratio Dataframe qui possède une ligne par groupe d'espèce dans lequel
#'             les prévisions de ratios seront rapportées.
#' @param Para.ratio_gaules Paramètres de l'équation de la prévision du ratio du
#'                         nombre de gaules par espèce.
#' @return  Retourne une prévision du ratio du nombre de gaules pour
#'          chacun des groupes d'espèces lorsqu'il y a
#'          présence de gaules de l'espèce.
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
