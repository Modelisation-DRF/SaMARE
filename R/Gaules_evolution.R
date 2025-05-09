#### Modèle nombre de gaules total ####

#' Fonction qui prévoit le nombre total de gaules à la fin du pas de simulation.
#'
#' @details
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
#'
#' @param Rec Dataframe qui contient les variables St_Ess_Ha, lnNb_Ess_Ha, trt, t0_aj_, dens_tot0 par Placette/GrEspece
#' @param RecGaules Dataframe qui contient la variable Nb_Gaules_Ess_Ha par Placette/GrEspece
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Para.nb_gaules Paramètres de l'équation de la prévision du nombre total de gaules.
#' @return Retourne un data d'une ligne par placette avec la colonne Nb_Gaules_Ha et xb_nb_gaules.
#' @export


nb_Gaules <- function(Rec, RecGaules, t, RandomPlacGaules, Para.nb_gaules){

  select=dplyr::select
  n = length(unique(Rec$Placette))

  # Construction matrice X
  # XNb_Gaules<-matrix(0,ncol=10,nrow=1)
  # XNb_Gaules[,1]<-1
  # XNb_Gaules[,2]<-log(sum(RecGaules$Nb_Gaules_Ess_Ha)+1)
  # XNb_Gaules[,3]<-log(sum(Rec$NbHa))  # nombre total d'arbres marchands = dens_tot0
  # XNb_Gaules[,4]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="HEG")]
  # XNb_Gaules[,5]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="ERS")]
  # XNb_Gaules[,6]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="BOJ")]
  # XNb_Gaules[,7]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="SAB")]
  # XNb_Gaules[,8]<-(Rec$trt=="TEM")*1
  # XNb_Gaules[,9]<-ifelse(Rec$trt=="CP",Rec$t0_aj_,0)
  # XNb_Gaules[,10]<-t

  # Préparer les info placettes
  lnNb_Ess_Ha_HEG <- Rec %>% filter(GrEspece=="HEG") %>% ungroup() %>% select(lnNb_Ess_Ha)
  lnNb_Ess_Ha_ERS <- Rec %>% filter(GrEspece=="ERS") %>% ungroup() %>% select(lnNb_Ess_Ha)
  lnNb_Ess_Ha_BOJ <- Rec %>% filter(GrEspece=="BOJ") %>% ungroup() %>% select(lnNb_Ess_Ha)
  lnNb_Ess_Ha_SAB <- Rec %>% filter(GrEspece=="SAB") %>% ungroup() %>% select(lnNb_Ess_Ha)
  info_plot <- Rec %>% group_by(Placette) %>% slice(1)
  nb_gaules <- RecGaules %>% group_by(Placette) %>% summarise(nb_gaules = sum(Nb_Gaules_Ess_Ha))

  XNb_Gaules<-matrix(0,ncol=10,nrow=n)
  XNb_Gaules[,1]<-1
  XNb_Gaules[,2]<-log(nb_gaules$nb_gaules+1) # nombre total de gaules
  XNb_Gaules[,3]<-log(info_plot$dens_tot0)  # nombre total d'arbres marchands = dens_tot0
  XNb_Gaules[,4]<-lnNb_Ess_Ha_HEG$lnNb_Ess_Ha
  XNb_Gaules[,5]<-lnNb_Ess_Ha_ERS$lnNb_Ess_Ha
  XNb_Gaules[,6]<-lnNb_Ess_Ha_BOJ$lnNb_Ess_Ha
  XNb_Gaules[,7]<-lnNb_Ess_Ha_SAB$lnNb_Ess_Ha
  XNb_Gaules[,8]<-(info_plot$trt=="TEM")*1
  XNb_Gaules[,9]<-ifelse(info_plot$trt=="CP",info_plot$t0_aj_,0)
  XNb_Gaules[,10]<-t



  # Construction matrice beta
  BetaMat<-matrix(Para.nb_gaules$ParameterEstimate,ncol=1)

  # Calcul du xb
  gaules <- info_plot %>% select(Placette)
  logit <- XNb_Gaules %*% BetaMat
  gaules$xb_nb_gaules <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==11) %>% select(Placette, RandomPlac)
  setDT(gaules)
  setDT(Random)
  gaules <- Random[, .(Placette, RandomPlac)][gaules, on = .(Placette)]


  gaules$Nb_Gaules_Ha <- round(exp(gaules$xb_nb_gaules + gaules$Random))
  gaules[, c("RandomPlac") := NULL]

  return(gaules)

}


#### Modèle Probabilité de gaules par essence ####

#'Fonction qui qui prévoit la probabilité d'absence de gaules par espèce pour la période
#'de simulation suivante. Cette fonction est la première partie du modèle zero-inflated
#'de Rijal et al. 2023
#'
#'
#' @param RecGaules Dataframe qui contient les variables lnNb_Gaules_Ess_Ha et Ratio par Placette/GrEspece
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param Rec Dataframe qui contient les variables lnNb_Ess_Ha, longitude, latitude par Placette/GrEspece
#' @param RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#'                          recrutement basé sur les gaules et du module d'évolution des gaules.
#' @param Ratio Dataframe qui possède une ligne par groupe d'espèces dans lequel les prévisions de ratios seront rapportées.
#' @param Para.ratio_gaules Paramètres de l'équation de la prévision du ratio du nombre de gaules par espèce.
#' @return  Retourne Ratio avec la colonne pred_pi_gaules et xb_pi_gaules.
#' @export
#'
#'
ratio_pi_Gaules<-function(Ratio,Rec,RecGaules,t,RandomPlacGaules,Para.ratio_gaules){
  select=dplyr::select

  n <- nrow(Ratio)

  #Liste des effets
  listeGrEss1<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n), rep("FEN",n),rep("FIN",n),rep("SAB",n))


  # Construction matrice X
  XRatio_pi<-matrix(0,ncol=11,nrow=n)
  XRatio_pi[,1]<-RecGaules$lnNb_Gaules_Ess_Ha
  XRatio_pi[,2:8]<-(Ratio$GrEspece==listeGrEss1)*Rec$lnNb_Ess_Ha
  XRatio_pi[,9]<-t
  XRatio_pi[,10]<-Rec$longitude
  XRatio_pi[,11]<-Rec$latitude

  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRatio_pi<-Para.ratio_gaules %>% filter(response=="pi")

  # Construction matrice beta
  BetaMat<-matrix(ParaRatio_pi$ParameterEstimate,ncol=1)

  # Calcul xb
  logit <-XRatio_pi %*% BetaMat
  Ratio$xb_pi_gaules <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==12 & response=="pi") %>% select(Placette, RandomPlac)
  setDT(Ratio)
  setDT(Random)
  Ratio <- Random[, .(Placette, RandomPlac)][Ratio, on = .(Placette)]



  Ratio$pred_pi_gaules<-1/(1+exp(-(Ratio$xb_pi_gaules + Ratio$RandomPlac)))
  Ratio[, c("RandomPlac") := NULL]


  return(Ratio)

}

#### Modèle de nombre gaules par essence quand l'essence est présente ####

#'Fonction qui prévoit le nombre de gaules par espèce quand l'espece est présente pour la période
#'de simulation suivante. Cette fonction est la deuxième partie du modèle zero-inflated
#'de Rijal et al. 2023.
#'
#' @param RecGaules Dataframe qui contient les variables lnNb_Gaules_Ess_Ha et Ratio par Placette/GrEspece
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param Rec Dataframe qui contient les variables st_tot0, dens_tot0, lnNb_Ess_Ha, prec, trt, t0_aj_, longitude Placette/GrEspece
#' @param RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Ratio Dataframe qui possède une ligne par groupe d'espèce dans lequel les prévisions de ratios seront rapportées.
#' @param Para.ratio_gaules Paramètres de l'équation de la prévision du ratio du nombre de gaules par espèce.
#' @return  Retourne Ratio avec la colonne pred_count_gaules et xb_count_gaules.
#' @export
ratio_count_Gaules<-function(Ratio,Rec,RecGaules,t,RandomPlacGaules,Para.ratio_gaules){
  select=dplyr::select

  n<-nrow(Ratio)

  #Liste des effets


  listeGrEss1<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
                 rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))


  # Construction matrice X
  XRatio_count<-matrix(0,ncol=18,nrow=n)
  XRatio_count[,1]<-RecGaules$Ratio
  XRatio_count[,2]<-RecGaules$lnNb_Gaules_Ess_Ha
  XRatio_count[,3]<-Rec$st_tot0
  XRatio_count[,4]<-log(Rec$dens_tot0)
  XRatio_count[,5:14]<-(Ratio$GrEspece==listeGrEss1)*Rec$lnNb_Ess_Ha
  XRatio_count[,15]<-Rec$prec
  XRatio_count[,16]<-ifelse(Rec$trt=="CP",Rec$t0_aj_,0)
  XRatio_count[,17]<-t
  XRatio_count[,18]<-Rec$longitude


  # selectionner les parametres
  ParaRatio_count<-Para.ratio_gaules %>% filter(response=="count")

  # Construction matrice beta
  BetaMat<-matrix(ParaRatio_count$ParameterEstimate,ncol=1)

  # Calcul xb
  logit <- XRatio_count %*% BetaMat
  Ratio$xb_count_gaules <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==12 & response=="count") %>% select(Placette, RandomPlac)
  setDT(Ratio)
  setDT(Random)
  Ratio <- Random[, .(Placette, RandomPlac)][Ratio, on = .(Placette)]

  Ratio$pred_count_gaules <- 1/(1+exp(-(Ratio$xb_count_gaules + Ratio$RandomPlac)))
  Ratio[, c("RandomPlac") := NULL]

  return(Ratio)

}

#### Calcul du nombre gaules par essence ####

#'Fonction qui prévoit la proportion du nombre de gaules par espèce pour la période
#'de simulation suivante à partir des modèles de Rijal et al. 2023
#'
#' @param Ratio Dataframe qui possède une ligne par groupe d'espèce dans lequel les prévisions de ratios seront rapportées.
#' @param Rec Dataframe qui contient les variables lnNb_Ess_Ha, longitude, latitude, st_tot0, dens_tot0, lnNb_Ess_Ha, prec, trt, t0_aj_ par Placette/GrEspece
#' @param RecGaules Dataframe qui contient la variable lnNb_Gaules_Ess_Ha par Placette/GrEspece
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à l'échelle de la placette du module des gaules.
#' @param Para.nb_gaules Paramètres de l'équation du nombre de gaules total
#' @param Para.ratio_gaules Paramètres de l'équation de la prévision du ratio du nombre de gaules par espèce.
#' @return Retourne Ratio avec les colonnes Nb_Gaules_Ess_Ha et lnNb_Gaules_Ess_Ha
#' @export
ratio_Gaules<-function(Ratio, Rec, RecGaules, t, RandomPlacGaules, Para.nb_gaules, Para.ratio_gaules){

  # Nombre total de gaules
  predNbGaules <- nb_Gaules(Rec, RecGaules, t, RandomPlacGaules, Para.nb_gaules) # calcul la variable Nb_Gaules_Ha

  # Probabilité d'Avoir au moins une gaules de l'essence (première partie du modèle zero-inflated count)
  Ratio <- ratio_pi_Gaules(Ratio, Rec, RecGaules, t, RandomPlacGaules, Para.ratio_gaules) # calcul la variable pred_pi_gaules

  # Nombre de gaules par essences (2e partie du modèle zero-inflated count)
  Ratio <- ratio_count_Gaules(Ratio, Rec, RecGaules, t, RandomPlacGaules, Para.ratio_gaules) # calcul la variable pred_count_gaules

  # Mise en commun des 2 parties du modèle et Calcul des ratios de gaules
  Ratio <- Ratio %>%
    group_by(Placette) %>%
    mutate(TotRatio = (1-pred_pi_gaules)*pred_count_gaules, # nombre de gaules de chaque essence (pas ajusté)
           FinalRatio = TotRatio/sum(TotRatio)) %>%         # ratio de gaules de chaque essence
    left_join(predNbGaules, by='Placette') %>%
    mutate(Nb_Gaules_Ess_Ha = round(FinalRatio*Nb_Gaules_Ha),  # nombre de gaules de chaque essence (ajusté pour le total), Ce round n'est pas dans la version de Junior
           lnNb_Gaules_Ess_Ha = log(Nb_Gaules_Ess_Ha+1)) %>%
    select(-contains('xb_'), -contains('pred_'))


  return(Ratio)

}


#### Modèles du nombre gaules 68 BOJ ####


#' Fonction qui calcul la probabilité d'absence de Gaules de bouleau jaune classes
#' de 6 ou 8 cm de diamètre. Cette fonction utilise une variante des paramètre
#' publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
#' Les prévisions sont basées sur un modèle de type Zero inflated.
#'
#' @param RecGaules Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha par Placette/GrEspece
#' @param Ratio Dataframe qui contient les variables Nb_Gaules_Ha et lnNb_Gaules_Ess_Ha par Placette/GrEspece
#' @param Rec Dataframe qui contient les variables St_Ess_Ha, lnNb_Ess_Ha, trt, t0_aj_, altitude par Placette/GrEspece
#' @param RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Para.68_BOJ Paramètres de l'équation de prévision du nombre de gaules de bouleau jaune de 6 et 8 cm de diamètre.
#' @return Retourne un data d'une ligne par placette avec la colonne pi_68BOJ et xb_pi_68BOJ.
#' @export
#'

pi68BOJ<-function(RecGaules,Ratio,Rec,RandomPlacGaules,Para.68_BOJ){

  select=dplyr::select
  n = length(unique(Rec$Placette))
  # info placette
  Rec_plot <- Rec %>% group_by(Placette) %>% slice(1)

  # Construction matrice X
  X68BOJ_pi<-matrix(0,ncol=8,nrow=n)
  X68BOJ_pi[,1]<-1
  X68BOJ_pi[,2]<-log(Ratio$Nb_Gaules_Ha[which(Ratio$GrEspece=="BOJ")])
  X68BOJ_pi[,3]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="BOJ")]
  X68BOJ_pi[,4]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="BOJ")]
  X68BOJ_pi[,5]<-Rec$St_Ess_Ha[which(Rec$GrEspece=="BOJ")]
  X68BOJ_pi[,6]<-Rec$lnNb_Ess_Ha[which(Rec$GrEspece=="BOJ")]
  X68BOJ_pi[,7]<-ifelse(Rec_plot$trt=="CP",Rec_plot$t0_aj_,0)
  X68BOJ_pi[,8]<-Rec_plot$altitude


  # selectionner les parametres
  Para68BOJ_pi<-Para.68_BOJ %>% filter(response=="pi")

  # Construction matrice beta
  BetaMat<-matrix(Para68BOJ_pi$ParameterEstimate,ncol=1)

  # Calcul xb
  logit <- X68BOJ_pi %*% BetaMat
  Rec_plot$xb_pi_68BOJ <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==15 & response=="pi") %>% select(Placette, RandomPlac)
  setDT(Rec_plot)
  setDT(Random)
  Rec_plot <- Random[, .(Placette, RandomPlac)][Rec_plot, on = .(Placette)]

  Rec_plot$pi_68BOJ <- 1/(1+exp(-(Rec_plot$xb_pi_68BOJ + Rec_plot$RandomPlac)))
  Rec_plot <- Rec_plot %>% select(Placette, xb_pi_68BOJ, pi_68BOJ)

  return(Rec_plot)

}

#' Fonction qui calcul le nombre de Gaules de bouleau jaune classes de 6 ou 8 cm de diamètre lorsquelle sont présente.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha par Placette/GrEspece
#' @param Ratio Dataframe qui contient la variable lnNb_Gaules_Ess_Ha par Placette/GrEspece
#' @param Rec Dataframe qui contient les variables à l'échelle de la placette trt, t0_aj_, latitude par Placette
#' @param t Longueur d'un pas de simulation
#' @param RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Para.68_BOJ Paramètre de l'équation de prévision du nombre de gaules de bouleau jaune de 6 et 8 cm de diamètre
#' @return Retourne un data d'une ligne par placette avec la colonne count_68BOJ et xb_count_68BOJ
#' @export
#'

count68BOJ<-function(RecGaules, Ratio, Rec, t, RandomPlacGaules, Para.68_BOJ){

  n = length(unique(Rec$Placette))
  # info placette
  Rec_plot <- Rec %>% group_by(Placette) %>% slice(1)

  # Construction matrice X
  X68BOJ_count<-matrix(0,ncol=7,nrow=n)
  X68BOJ_count[,1]<-1
  X68BOJ_count[,2]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="BOJ")]
  X68BOJ_count[,3]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="BOJ")]
  X68BOJ_count[,4]<-t
  X68BOJ_count[,5]<-ifelse(Rec_plot$trt=="TEM",1,0)
  X68BOJ_count[,6]<-ifelse(Rec_plot$trt=="CP",Rec_plot$t0_aj_,0)
  X68BOJ_count[,7]<-Rec_plot$latitude


  # selectionner les parametres
  Para68BOJ_count<-Para.68_BOJ %>% filter(response=="count")

  # Construction matrice beta
  BetaMat<-matrix(Para68BOJ_count$ParameterEstimate,ncol=1)

  # Calcul xb
  logit <- X68BOJ_count %*% BetaMat
  Rec_plot$xb_count_68BOJ <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==15 & response=="count") %>% select(Placette, RandomPlac)
  setDT(Rec_plot)
  setDT(Random)
  Rec_plot <- Random[, .(Placette, RandomPlac)][Rec_plot, on = .(Placette)]


  Rec_plot$count_68BOJ <- exp(Rec_plot$xb_count_68BOJ + Rec_plot$RandomPlac)-1
  Rec_plot<-Rec_plot %>% select(Placette, xb_count_68BOJ, count_68BOJ)

  return(Rec_plot)

}

#### Modèles du nombre gaules 68 ERS ####

#' Fonction qui calcul la probabilité d'absence de Gaules d'érable à sucre classes de 6 ou 8 cm de diamètre.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha par Placette/GrEspece
#' @param Ratio Dataframe qui contient les variables Nb_Gaules_Ha et lnNb_Gaules_Ess_Ha par Placette/GrEspece
#' @param RandomPlacGaules  Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Rec Dataframe avec la colonne Placette
#' @param Para.68_ERS Paramètres de l'équation de prévision du nombre de gaules d'érable à sucre de 6 et 8 cm de diamètre
#' @return Retourne un data d'une ligne par placette avec la colonne pi_68ERS et xb_pi_68ERS
#' @export

pi68ERS<-function(RecGaules, Ratio, Rec, RandomPlacGaules, Para.68_ERS){

  select=dplyr::select
  n = length(unique(Ratio$Placette))
  Rec_plot <- Rec %>% group_by(Placette) %>% slice(1)

  # Construction matrice X
  X68ERS_pi<-matrix(0,ncol=4,nrow=n)
  X68ERS_pi[,1]<-1
  X68ERS_pi[,2]<-log(Ratio$Nb_Gaules_Ha[which(Ratio$GrEspece=="ERS")])
  X68ERS_pi[,3]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="ERS")]
  X68ERS_pi[,4]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="ERS")]


  # selectionner les parametres
  Para68ERS_pi<-Para.68_ERS %>% filter(response=="pi")

  # Construction matrice beta
  BetaMat<-matrix(Para68ERS_pi$ParameterEstimate,ncol=1)

  # Calcul xb
  logit <- X68ERS_pi %*% BetaMat
  Rec_plot$xb_pi_68ERS <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==13 & response=="pi") %>% select(Placette, RandomPlac)
  setDT(Rec_plot)
  setDT(Random)
  Rec_plot <- Random[, .(Placette, RandomPlac)][Rec_plot, on = .(Placette)]

  Rec_plot$pi_68ERS <- 1/(1+exp(-(Rec_plot$xb_pi_68ERS + Rec_plot$RandomPlac)))
  Rec_plot <- Rec_plot %>% select(Placette, xb_pi_68ERS, pi_68ERS)

  return(Rec_plot)

}

#' Fonction qui calcul le nombre de Gaules d'érable à sucre classes de 6 ou 8 cm de diamètre lorsquelle sont présente.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha par Placette/GrEspece
#' @param Ratio Dataframe qui contient les variables Nb_Gaules_Ha et lnNb_Gaules_Ess_Ha par Placette/GrEspece
#' @param RandomPlacGaules  Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Rec Dataframe qui contient les variables à l'échelle de la placette dens_tot0, grwd par placette
#' @param Para.68_ERS Paramètres de l'équation de prévision du nombre de gaules d'érable à sucre de 6 et 8 cm de diamètre
#' @return Retourne un data d'une ligne par placette avec la colonne count_68ERS et xb_count_68ERS
#' @export
count68ERS<-function(RecGaules, Ratio, Rec, RandomPlacGaules,Para.68_ERS){

  n = length(unique(Rec$Placette))
  # info placette
  Rec_plot <- Rec %>% group_by(Placette) %>% slice(1)

  # Construction matrice X
  X68ERS_count<-matrix(0,ncol=5,nrow=n)
  X68ERS_count[,1]<-log(Ratio$Nb_Gaules_Ha[which(Ratio$GrEspece=="ERS")])
  X68ERS_count[,2]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="ERS")]
  X68ERS_count[,3]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="ERS")]
  X68ERS_count[,4]<-log(Rec_plot$dens_tot0) # il faut garder seulement une ligne par placette
  X68ERS_count[,5]<-Rec_plot$grwd           # il faut garder seulement une ligne par placette


  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  Para68ERS_count<-Para.68_ERS %>% filter(response=="count")

  # Construction matrice beta
  BetaMat<-matrix(Para68ERS_count$ParameterEstimate,ncol=1)

  # Calcul xb
  logit <- X68ERS_count %*% BetaMat
  Rec_plot$xb_count_68ERS <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==13 & response=="count") %>% select(Placette, RandomPlac)
  setDT(Rec_plot)
  setDT(Random)
  Rec_plot <- Random[, .(Placette, RandomPlac)][Rec_plot, on = .(Placette)]


  Rec_plot$count_68ERS <- exp(Rec_plot$xb_count_68ERS + Rec_plot$RandomPlac)-1
  Rec_plot <- Rec_plot %>% select(Placette, xb_count_68ERS, count_68ERS)

  return(Rec_plot)

}

#### Modèles du nombre gaules 68 HEG ####

#' Fonction qui calcul la probabilité d'absence de Gaules de hêtre à grande feuille classes de 6 ou 8 cm de diamètre.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha par Placette/GrEspece
#' @param Ratio Dataframe qui contient la variable lnNb_Gaules_Ess_Ha Placette/GrEspece
#' @param RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Rec Dataframe avec la colonne Placette
#' @param Para.68_HEG Paramètres de l'équation de prévision du nombre de gaules de hêtre à grande feuille de 6 et 8 cm de diamètre
#' @return Retourne un data d'une ligne par placette avec la colonne pi_68HEG et xb_pi_68HEG
#' @export
#'
pi68HEG<-function(RecGaules,Ratio,Rec,RandomPlacGaules,Para.68_HEG){

  select=dplyr::select
  n = length(unique(Ratio$Placette))
  Rec_plot <- Rec %>% group_by(Placette) %>% slice(1)

  # Construction matrice X
  X68HEG_pi<-matrix(0,ncol=3,nrow=n)
  X68HEG_pi[,1]<-1
  X68HEG_pi[,2]<-Ratio$lnNb_Gaules_Ess_Ha[which(RecGaules$GrEspece=="HEG")]
  X68HEG_pi[,3]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="HEG")]


  # selectionner les parametres
  Para68HEG_pi<-Para.68_HEG %>% filter(response=="pi")

  # Construction matrice beta
  BetaMat<-matrix(Para68HEG_pi$ParameterEstimate,ncol=1)

  # Calcul xb
  logit <- X68HEG_pi %*% BetaMat
  Rec_plot$xb_pi_68HEG <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==14 & response=="pi") %>% select(Placette, RandomPlac)
  setDT(Rec_plot)
  setDT(Random)
  Rec_plot <- Random[, .(Placette, RandomPlac)][Rec_plot, on = .(Placette)]


  Rec_plot$pi_68HEG <- 1/(1+exp(-(Rec_plot$xb_pi_68HEG + Rec_plot$RandomPlac)))
  Rec_plot <- Rec_plot %>% select(Placette, xb_pi_68HEG, pi_68HEG)

  return(Rec_plot)

}

#' Fonction qui calcul le nombre de Gaules de hêtre à grandes feuilles classes de 6 ou 8 cm de diamètre lorsquelle sont présente.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient les variables lnNb_Gaules_68_Ess_Ha et lnNb_Gaules_24_Ess_Ha par Placette/GrEspece
#' @param Ratio Dataframe qui contient la variable Placette
#' @param Rec Dataframe avec la colonne Placette
#' @param RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Para.68_HEG Paramètres de l'équation de prévision du nombre de gaules de hêtre à grande feuille de 6 et 8 cm de diamètre
#' @return Retourne un data d'une ligne par placette avec la colonne count_68HEG et xb_count_68HEG
#' @export
count68HEG<-function(RecGaules,Ratio,Rec,RandomPlacGaules,Para.68_HEG){

  n = length(unique(Ratio$Placette))
  Rec_plot <- Rec %>% group_by(Placette) %>% slice(1)

  # Construction matrice X
  X68HEG_count<-matrix(0,ncol=3,nrow=n)
  X68HEG_count[,1]<-1
  X68HEG_count[,2]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="HEG")]
  X68HEG_count[,3]<-RecGaules$lnNb_Gaules_24_Ess_Ha[which(RecGaules$GrEspece=="HEG")]


  # selectionner les parametres
  Para68HEG_count<-Para.68_HEG %>% filter(response=="count")

  # Construction matrice beta
  BetaMat<-matrix(Para68HEG_count$ParameterEstimate,ncol=1)

  # Calcul xb
  logit <-X68HEG_count %*% BetaMat

  Rec_plot$xb_count_68HEG <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==14 & response=="count") %>% select(Placette, RandomPlac)
  setDT(Rec_plot)
  setDT(Random)
  Rec_plot <- Random[, .(Placette, RandomPlac)][Rec_plot, on = .(Placette)]


  Rec_plot$count_68HEG <- exp(Rec_plot$xb_count_68HEG + Rec_plot$RandomPlac)-1
  Rec_plot <- Rec_plot %>% select(Placette, xb_count_68HEG, count_68HEG)

  return(Rec_plot)

}

#### Modèles du nombre gaules 68 SAB ####

#' Fonction qui calcul la probabilité d'absence de Gaules de sapin baumier classes de 6 ou 8 cm de diamètre.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha par Placette/GrEspece
#' @param Ratio Dataframe qui contient les variables Nb_Gaules_Ha et lnNb_Gaules_Ess_Ha par Placette/GrEspece
#' @param RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Rec Dataframe qui contient les variables dens_tot0 par placette
#' @param Para.68_SAB Paramètres de l'équation de prévision du nombre de gaules de sapin baumier de 6 et 8 cm de diamètre
#' @return Retourne un data d'une ligne par placette avec la colonne pi_68SAB et xb_pi_68SAB
#' @export
pi68SAB<-function(RecGaules,Ratio,Rec,RandomPlacGaules,Para.68_SAB){

  select=dplyr::select
  n = length(unique(Rec$Placette))
  # info placette
  Rec_plot <- Rec %>% group_by(Placette) %>% slice(1)

  # Construction matrice X
  X68SAB_pi<-matrix(0,ncol=4,nrow=n)
  X68SAB_pi[,1]<-log(Ratio$Nb_Gaules_Ha[which(Ratio$GrEspece=="SAB")])
  X68SAB_pi[,2]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="SAB")]
  X68SAB_pi[,3]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="SAB")]
  X68SAB_pi[,4]<-log(Rec_plot$dens_tot0)


  # selectionner les parametres
  Para68SAB_pi<-Para.68_SAB %>% filter(response=="pi")

  # Construction matrice beta
  BetaMat<-matrix(Para68SAB_pi$ParameterEstimate,ncol=1)

  # Calcul xb
  logit <- X68SAB_pi %*% BetaMat
  Rec_plot$xb_pi_68SAB <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==16 & response=="pi") %>% select(Placette, RandomPlac)
  setDT(Rec_plot)
  setDT(Random)
  Rec_plot <- Random[, .(Placette, RandomPlac)][Rec_plot, on = .(Placette)]

  Rec_plot$pi_68SAB <- 1/(1+exp(-(Rec_plot$xb_pi_68SAB + Rec_plot$RandomPlac)))
  Rec_plot <- Rec_plot %>% select(Placette, xb_pi_68SAB, pi_68SAB)

  return(Rec_plot)

}

#' Fonction qui calcul le nombre de Gaules de sapin baumier classes de 6 ou 8 cm de diamètre lorsquelle sont présente.
#' Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
#' Les prévisions sont basées sur un modèle de type Zero inflated
#'
#' @param RecGaules Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha par Placette/GrEspece
#' @param Ratio Dataframe qui contient les variables Nb_Gaules_Ha et lnNb_Gaules_Ess_Ha par Placette/GrEspece
#' @param Rec Dataframe qui contient les variables St_Ess_Ha, trt, t0_aj_ par placette/GrEspece
#' @param RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Para.68_SAB Paramètres de l'équation de prévision du nombre de gaules de sapin baumier de 6 et 8 cm de diamètre
#' @return Retourne un data d'une ligne par placette avec la colonne count_68SAB et xb_count_68SAB
#' @export
count68SAB<-function(RecGaules,Ratio,Rec,RandomPlacGaules,Para.68_SAB){

  n = length(unique(Rec$Placette))
  # info placette
  Rec_plot <- Rec %>% group_by(Placette) %>% slice(1)

  # Construction matrice X
  X68SAB_count<-matrix(0,ncol=6,nrow=n)
  X68SAB_count[,1]<-1
  X68SAB_count[,2]<-Ratio$lnNb_Gaules_Ess_Ha[which(Ratio$GrEspece=="SAB")]
  X68SAB_count[,3]<-RecGaules$lnNb_Gaules_68_Ess_Ha[which(RecGaules$GrEspece=="SAB")]
  X68SAB_count[,4]<-log(Rec_plot$dens_tot0)
  X68SAB_count[,5]<-Rec$St_Ess_Ha[which(Rec$GrEspece=="SAB")]
  X68SAB_count[,6]<-ifelse(Rec_plot$trt=="CP", Rec_plot$t0_aj_, 0)


  # selectionner les parametres
  Para68SAB_count<-Para.68_SAB %>% filter(response=="count")

  # Construction matrice beta
  BetaMat<-matrix(Para68SAB_count$ParameterEstimate,ncol=1)

  # Calcul xb
  logit <-X68SAB_count %*% BetaMat
  Rec_plot$xb_count_68SAB <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==16 & response=="count") %>% select(Placette, RandomPlac)
  setDT(Rec_plot)
  setDT(Random)
  Rec_plot <- Random[, .(Placette, RandomPlac)][Rec_plot, on = .(Placette)]


  Rec_plot$count_68SAB <- exp(Rec_plot$xb_count_68SAB + Rec_plot$RandomPlac)-1
  Rec_plot <- Rec_plot %>% select(Placette, xb_count_68SAB, count_68SAB)

  return(Rec_plot)

}

#### Calcul du nombre gaules 68 par essence ####

#'Fonction qui prévoit la proportion du nombre de gaules par espèce pour la période
#'de simulation suivante à partir des modèles de Rijal et al. 2023
#'
#' @param Ratio Dataframe qui contient les variables Nb_Gaules_Ha et lnNb_Gaules_Ess_Ha par Placette/GrEspece
#' @param Rec Dataframe qui contient les variables St_Ess_Ha, lnNb_Ess_Ha, trt, t0_aj_, altitude, latitude, dens_tot0, grwd par Placette/GrEspece
#' @param RecGaules Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha et lnNb_Gaules_24_Ess_Ha par Placette/GrEspece
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
#' @param Para.68_ERS Paramètre du modèle du nombre de gaules 68 de ERS
#' @param Para.68_HEG Paramètre du modèle du nombre de gaules 68 de HEG
#' @param Para.68_BOJ Paramètre du modèle du nombre de gaules 68 de BOJ
#' @param Para.68_SAB Paramètre du modèle du nombre de gaules 68 de SAB
#' @return Retourne un dataframe avec le nombre de gaules 24 et 68 par placette/espèce, colonnes
#'         Nb_Gaules_Ha, Nb_Gaules_Ess_Ha, Nb_Gaules_24_Ess_Ha, Nb_Gaules_68_Ess_Ha,
#'         lnNb_Gaules_Ess_Ha, lnNb_Gaules_24_Ess_Ha, lnNb_Gaules_68_Ess_Ha
#' @export
gaules_68 <- function(Ratio, Rec, RecGaules, t, RandomPlacGaules, Para.68_ERS, Para.68_HEG, Para.68_BOJ, Para.68_SAB){

  # Nombre de gaules 68 de ERS
  Ratio_pi68ERS <- pi68ERS(RecGaules, Ratio, Rec, RandomPlacGaules, Para.68_ERS)
  Ratio_count68ERS <- count68ERS(RecGaules, Ratio, Rec, RandomPlacGaules, Para.68_ERS)

  # Nombre de gaules 68 de HEG
  Ratio_pi68HEG <- pi68HEG(RecGaules, Ratio, Rec, RandomPlacGaules, Para.68_HEG)
  Ratio_count68HEG <- count68HEG(RecGaules, Ratio, Rec, RandomPlacGaules, Para.68_HEG)

  # Nombre de gaules 68 de BOJ
  Ratio_pi68BOJ <- pi68BOJ(RecGaules, Ratio, Rec, RandomPlacGaules, Para.68_BOJ)
  Ratio_count68BOJ <- count68BOJ(RecGaules, Ratio, Rec, t, RandomPlacGaules, Para.68_BOJ)

  # Nombre de gaules 68 de SAB
  Ratio_pi68SAB <- pi68SAB(RecGaules, Ratio, Rec, RandomPlacGaules, Para.68_SAB)
  Ratio_count68SAB <- count68SAB(RecGaules, Ratio, Rec, RandomPlacGaules, Para.68_SAB)


  result <- left_join(Ratio_pi68ERS, Ratio_count68ERS, by='Placette') %>%
    left_join(Ratio_pi68HEG, by='Placette') %>%
    left_join(Ratio_count68HEG, by='Placette') %>%
    left_join(Ratio_pi68BOJ, by='Placette') %>%
    left_join(Ratio_count68BOJ, by='Placette') %>%
    left_join(Ratio_pi68SAB, by='Placette') %>%
    left_join(Ratio_count68SAB, by='Placette') %>%
    mutate(Pred68ERS = round((1-pi_68ERS) * count_68ERS),
           Pred68HEG = round(((1-pi_68HEG) * count_68HEG)*0.71),  #Correction basé sur le biais moyen observé
           Pred68BOJ = round((1-pi_68BOJ) * count_68BOJ),
           Pred68SAB = round((1-pi_68SAB) * count_68SAB)) %>%
    select(-contains('xb_'), -contains('count_'), -contains('pi_'))

  result <- left_join(Ratio, result, by='Placette')

  # Mise à jour Gaules
  Nb68 <- result %>%
    mutate(
      # Calcul du nombre de gaules24 si le nombre total de gaules predit est plus grand que le nombre de gaules68 prédit, sinon gaules24=0
      Nb_Gaules_24_Ess_Ha = ifelse(GrEspece=="ERS", ifelse(Nb_Gaules_Ess_Ha > Pred68ERS, Nb_Gaules_Ess_Ha-Pred68ERS, 0),
                                   ifelse(GrEspece=="HEG", ifelse(Nb_Gaules_Ess_Ha > Pred68HEG, Nb_Gaules_Ess_Ha-Pred68HEG, 0),
                                          ifelse(GrEspece=="BOJ", ifelse(Nb_Gaules_Ess_Ha > Pred68BOJ, Nb_Gaules_Ess_Ha-Pred68BOJ, 0),
                                                 ifelse(GrEspece=="SAB", ifelse(Nb_Gaules_Ess_Ha > Pred68SAB, Nb_Gaules_Ess_Ha-Pred68SAB,0),
                                                        0)))),

      lnNb_Gaules_24_Ess_Ha = log(Nb_Gaules_24_Ess_Ha+1),

      # si gaules68 est plus grand que gaules total, on corrige gaules68
      # ceci n'est pas dans la version de Junior, ça ne donnera donc pas la même chose
      Pred68ERS = ifelse(Pred68ERS > Nb_Gaules_Ess_Ha, Nb_Gaules_Ess_Ha, Pred68ERS),
      Pred68HEG = ifelse(Pred68HEG > Nb_Gaules_Ess_Ha, Nb_Gaules_Ess_Ha, Pred68HEG),
      Pred68BOJ = ifelse(Pred68BOJ > Nb_Gaules_Ess_Ha, Nb_Gaules_Ess_Ha, Pred68BOJ),
      Pred68SAB = ifelse(Pred68SAB > Nb_Gaules_Ess_Ha, Nb_Gaules_Ess_Ha, Pred68SAB),

      Nb_Gaules_68_Ess_Ha = ifelse(GrEspece=="ERS", Pred68ERS,
                                   ifelse(GrEspece=="HEG", Pred68HEG,
                                          ifelse(GrEspece=="BOJ", Pred68BOJ,
                                                 ifelse(GrEspece=="SAB", Pred68SAB,
                                                        0)))),

      lnNb_Gaules_68_Ess_Ha = log(Nb_Gaules_68_Ess_Ha+1)) %>%

    select(Placette, GrEspece, Nb_Gaules_Ha, Nb_Gaules_Ess_Ha, Nb_Gaules_24_Ess_Ha, Nb_Gaules_68_Ess_Ha,
           lnNb_Gaules_Ess_Ha, lnNb_Gaules_24_Ess_Ha, lnNb_Gaules_68_Ess_Ha)


  return(Nb68)

}

