#' Fonction qui calcul la probabilité d'absence de Gaules de hêtre à grande feuille classes de 6 ou 8 cm de diamètre.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
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
#' @param Para.68_BOJ Paramètres de l'équation de prévision du nombre de gaules de bouleau jaune de 6 et 8 cm de diamètre
#' @return Retourne une probabilité d'absence de gaules de 6 et 8 cm de DHP de bouleau jaune
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

#' Fonction qui calcul le nombre de Gaules de hêtre à grandes feuilles classes de 6 ou 8 cm de diamètre lorsquelle sont présente.
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
#' @param Para.68_BOJ Paramètres de l'équation de prévision du nombre de gaules de bouleau jaune de 6 et 8 cm de diamètre
#' @return Retourne une prévision du nombre de gaules de 6 et 8 cm de DHP de bouleau jaune lorsquelles sont présentes
#' @examples


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
