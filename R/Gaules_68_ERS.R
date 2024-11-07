#' Fonction qui calcul la probabilité d'absence de Gaules d'érable à sucre classes de 6 ou 8 cm de diamètre.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient les information sur la distribution des gaules dans la placette
#' @param Ratio   Un dataframe qui contient la répartition du nombre de gaule prédit entre les groupes d'espèces
#' @param Iterj  Itération en cours
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à l'échelle de la placette du module de
#'                          recrutement basé sur les gaules et du module d'évolution des gaules
#' @param Para.68_ERS Paramètres de l'équation de prévision du nombre de gaules d'érable à sucre de 6 et 8 cm de diamètre
#' @return Retourne une probabilité d'absence de gaules de 6 et 8 cm de DHP d'érable à sucre
#' @export

pi68ERS<-function(RecGaules,Ratio,Iterj,RandomPlacGaules,Para.68_ERS){

  select=dplyr::select

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

#' Fonction qui calcul le nombre de Gaules d'érable à sucre classes de 6 ou 8 cm de diamètre lorsquelle sont présente.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient les information sur la distribution des gaules dans la placette
#' @param Ratio   Un dataframe qui contient la répartition du nombre de gaule prédit entre les groupes d'espèces
#' @param dens_tot0 Densité totale
#' @param grwd nombre de jours dans la saison de croissance
#' @param Iterj  Itération en cours
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à l'échelle de la placette du module de
#'                          recrutement basé sur les gaules et du module d'évolution des gaules
#' @param Para.68_ERS Paramètres de l'équation de prévision du nombre de gaules d'érable à sucre de 6 et 8 cm de diamètre
#' @return Retourne une prévision du nombre de gaules de 6 et 8 cm de DHP d'érable à sucre lorsquelles sont présentes
#' @export


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
