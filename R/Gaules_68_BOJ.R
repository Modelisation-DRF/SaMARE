
#' Fonction qui calcul la probabilité d'absence de Gaules de bouleau jaune classes
#' de 6 ou 8 cm de diamètre. Cette fonction utilise une variante des paramètre
#' publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
#' Les prévisions sont basées sur un modèle de type Zero inflated.
#'
#' @param RecGaules Dataframe qui contient les informations sur la distribution
#'                 des gaules dans la placette.
#' @param Ratio   Un dataframe qui contient la répartition du nombre de gaules
#'               prédit entre les groupes d'espèces.
#' @param Rec    Un dataframe qui contient la prévision du nombre de recrues par
#'               groupes d'espèces.
#' @param trt    Variable distinguant les peuplements traités des témoins, si
#'                St >26 = TEM.
#' @param t0_aj_  Temps écoulé depuis la dernière coupe partielle.
#' @param altitude Altitude de la placette.
#' @param Iterj  Itération en cours.
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à l'échelle
#'                           de la placette du module de recrutement basé sur
#'                           les gaules et du module d'évolution des gaules.
#' @param Para.68_BOJ Paramètres de l'équation de prévision du nombre de gaules
#'                   de bouleau jaune de 6 et 8 cm de diamètre.
#' @return Retourne une probabilité d'absence de gaules de 6 et 8 cm de DHP de
#'         bouleau jaune.
#' @examples


pi68BOJ<-function(RecGaules,Ratio,Rec,trt,t0_aj_,altitude,Iterj,RandomPlacGaules,Para.68_BOJ){

  select=dplyr::select

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

#' Fonction qui calcul le nombre de Gaules de bouleau jaune classes de 6 ou 8 cm de diamètre lorsquelle sont présente.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient les information sur la distribution des gaules dans la placette
#' @param Ratio   Un dataframe qui contient la répartition du nombre de gaule prédit entre les groupes d'espèces
#' @param Rec    Un dataframe qui contient la prévision du nombre de recrues par groupes d'espèces
#' @param trt    Variable distinguant les peuplements traités des témoins, si St >26 = TEM
#' @param t0_aj_     Temps écoulé depuis la dernière coupe partielle
#' @param altitude Altitude de la placette
#' @param Iterj  Itération en cours
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à l'échelle de la placette du module de
#'                          recrutement basé sur les gaules et du module d'évolution des gaules
#' @param Para.68_BOJ Paramètre de l'équation de prévision du nombre de gaules de bouleau jaune de 6 et 8 cm de diamètre
#' @return Retourne une prévision du nombre de gaules de 6 et 8 cm de DHP de bouleau jaune lorsquelles sont présentes
#' @examples

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
