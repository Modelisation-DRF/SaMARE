#' Fonction qui calcul la probabilité d'absence de Gaules de sapin baumier classes de 6 ou 8 cm de diamètre.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient les information sur la distribution des gaules dans la placette
#' @param Ratio   Un dataframe qui contient la répartition du nombre de gaule prédit entre les groupes d'espèces
#' @param dens_tot0    Densité totale
#' @param Iterj  Itération en cours
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à l'échelle de la placette du module de
#'                          recrutement basé sur les gaules et du module d'évolution des gaules
#' @param Para.68_SAB Paramètres de l'équation de prévision du nombre de gaules de sapin baumier de 6 et 8 cm de diamètre
#' @return Retourne une probabilité d'absence de gaules de 6 et 8 cm de DHP de sapin baumier
#' @export

pi68SAB<-function(RecGaules,Ratio,dens_tot0,Iterj,RandomPlacGaules,Para.68_SAB){

  select=dplyr::select

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

#' Fonction qui calcul le nombre de Gaules de sapin baumier classes de 6 ou 8 cm de diamètre lorsquelle sont présente.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient les information sur la distribution des gaules dans la placette
#' @param Ratio   Un dataframe qui contient la répartition du nombre de gaule prédit entre les groupes d'espèces
#' @param Rec    Un dataframe qui contient la prévision du nombre de recrues par groupes d'espèces
#' @param trt    Variable distinguant les peuplements traités des témoins, si St >26 = TEM
#' @param t0_aj_     Temps écoulé depuis la dernière coupe partielle
#' @param dens_tot0 Densité totale
#' @param Iterj  Itération en cours
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à l'échelle de la placette du module de
#'                          recrutement basé sur les gaules et du module d'évolution des gaules
#' @param Para.68_SAB Paramètres de l'équation de prévision du nombre de gaules de sapin baumier de 6 et 8 cm de diamètre
#' @return Retourne une prévision du nombre de gaules de 6 et 8 cm de DHP de sapin baumier lorsqu'elles sont présentes.
#' @export

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
