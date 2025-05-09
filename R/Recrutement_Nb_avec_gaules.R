#### Modèle de probabilité de recrue basé sur les gaules  ####

#' Fonction qui prévoie la probabilité d'absence de recrues par groupe d'espèce.
#' Cette fonction corespond à la première portion de la fonction zero-inflated
#' de prévision du nombre de recrues basé sur les gaules de Rijal et al. 2023.
#'
#'
#' @param RecGaules Dataframe qui contient les variables lnNb_Gaules_Ess_Ha et lnNb_Gaules_68_Ess_Ha par Placette/GrEspece
#' @param Rec  Dataframe qui contient les variables st_tot0 et lnNb_Ess_Ha par Placette/GrEspece
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à l'échelle de la placette du module de recrutement basé sur les gaules
#' @param Para.rec_gaules Paramètres de l'équation de prévivion du nombre de recrues utilisant le nombre de gaules.
#' @return  Retourne Rec avec la colonne predPi_nbrec et xb_prob_rec
#' @details
#' RecGaules et Rec doivent être triés de la même façon.
#'
#' @export
rec_pi_Gaules<-function(Rec, RecGaules, t, RandomPlacGaules, Para.rec_gaules){

  select=dplyr::select
  n<-nrow(Rec)

  #Liste des effets

  listeGrEss1<-c(rep("AUT",n),rep("EPX",n),rep("ERR",n),rep("FEN",n), rep("FIN",n),rep("HEG",n),rep("RES",n))

  listeGrEss2<-c(rep("ERS",n),rep("HEG",n),rep("BOJ",n),rep("SAB",n))

  # Construction matrice X
  Xrec_pi<-matrix(0,ncol=14,nrow=n)
  Xrec_pi[,1]<-Rec$st_tot0
  Xrec_pi[,2]<-t
  Xrec_pi[,3:9]<-(Rec$GrEspece==listeGrEss1)*Rec$lnNb_Ess_Ha
  Xrec_pi[,10]<-(RecGaules$GrEspece=="BOJ")*RecGaules$lnNb_Gaules_Ess_Ha
  Xrec_pi[,11:14]<-(RecGaules$GrEspece==listeGrEss2)*RecGaules$lnNb_Gaules_68_Ess_Ha

  # selectionner les parametres
  ParaRec_pi<-Para.rec_gaules %>% filter(response=="pi")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_pi$ParameterEstimate,ncol=1)

  # Calcul logit
  logit <- Xrec_pi %*% BetaMat
  Rec$xb_prob_rec <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==10 & response=="pi") %>% select(Placette, RandomPlac)
  setDT(Rec)
  setDT(Random)
  Rec <- Random[, .(Placette, RandomPlac)][Rec, on = .(Placette)]

  # calcul de la prob
  Rec$predPi_nbrec <- 1/(1+exp(-(Rec$xb_prob_rec + Rec$RandomPlac)))
  Rec[, c("RandomPlac") := NULL]

  return(Rec)

}

#### Modèle de nombre de recrues par essence quand l'essence est présente basé sur les gaules  ####

#'Fonction qui prévoie le nombre de recrues par groupe d'espèce lorsqu'il
#'y a présence de recrues du groupe d'espèce. Cette fonction corespond à la
#'deuxième portion de la fonction zero-inflated de prévision du nombre
#'de recrues de Rijal et al. 2023.
#'
#'
#' @param RecGaules Dataframe qui contient les variables lnNb_Gaules_Ess_Ha et lnNb_Gaules_68_Ess_Ha par Placette/GrEspece
#' @param Rec  Dataframe qui contient les variables st_tot0 et St_Ess_Ha et lnNb_Ess_Ha par Placette/GrEspece
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires àl'échelle de la placette du module de recrutement basé sur les gaules
#' @param Para.rec_gaules Paramètres de l'équation de prévivion du nombre de recrues utilisant le nombre de gaules.
#' @return  Retourne Rec avec la colonne predCount_nbrec et xb_count_rec
#' @details
#' RecGaules et Rec doivent être triés de la même façon.
#' @export
#'
rec_count_Gaules<-function(Rec, RecGaules, t, RandomPlacGaules, Para.rec_gaules){
  select=dplyr::select
  n<-nrow(Rec)

  #Liste des effets

  listeGrEss1<-c(rep("AUT",n),rep("FEN",n))
  listeGrEss2<-c(rep("AUT",n),rep("SAB",n))
  listeGrEss3<-c(rep("EPX",n),rep("ERR",n),rep("FEN",n),rep("FIN",n))
  listeGrEss4<-c(rep("ERS",n),rep("HEG",n),rep("BOJ",n),rep("SAB",n))

  # Construction matrice X
  Xrec_count<-matrix(0,ncol=14,nrow=n)
  Xrec_count[,1]<-1
  Xrec_count[,2]<-Rec$st_tot0
  Xrec_count[,3:4]<-(Rec$GrEspece==listeGrEss1)*Rec$St_Ess_Ha
  Xrec_count[,5:6]<-(Rec$GrEspece==listeGrEss2)*Rec$lnNb_Ess_Ha
  Xrec_count[,7:10]<-(RecGaules$GrEspece==listeGrEss3)*RecGaules$lnNb_Gaules_Ess_Ha
  Xrec_count[,11:14]<-(RecGaules$GrEspece==listeGrEss4)*RecGaules$lnNb_Gaules_68_Ess_Ha

  # selectionner les parametres
  ParaRec_count<-Para.rec_gaules  %>% filter(response=="count")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_count$ParameterEstimate,ncol=1)

  # Calcul du xb
  logit <- Xrec_count %*% BetaMat
  Rec$xb_count_rec <- as.numeric(logit)

  # ajouter l'effet aléatoire de placette
  Random <- RandomPlacGaules %>% filter(SubModuleID==10 & response=="count") %>% select(Placette, RandomPlac)
  setDT(Rec)
  setDT(Random)
  Rec <- Random[, .(Placette, RandomPlac)][Rec, on = .(Placette)]

  # calcul du count
  Rec$predCount_nbrec <- exp(Rec$xb_count_rec + Rec$RandomPlac)
  Rec[, c("RandomPlac") := NULL]

  return(Rec)

}

#### Calcul du nombre de recrues par essences basé sur les gaules  ####

#' Fonction qui prévoie le nombre de recrues par groupe d'espèces.
#' avec les fonctions rec_pi, rec_lambda et rec_delta
#'
#' @param RecGaules Dataframe qui contient les variables lnNb_Gaules_Ess_Ha et lnNb_Gaules_68_Ess_Ha par Placette/GrEspece
#' @param Rec  Dataframe qui contient les variables st_tot0 et St_Ess_Ha et lnNb_Ess_Ha par Placette/GrEspece
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param CovParmsGaules Dataframe contenant la variance des effets aléatoires des équations du module de recrutement basés sur l'information
#'                        provenant des gaules (modules 10 à 16).
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à l'échelle de la placette du module de recrutement basé sur les gaules
#' @param Para.rec_gaules Paramètres de l'équation de prévivion du nombre de recrues utilisant le nombre de gaules
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return  Retourne un dataframe d'une ligne par placette/grespece avec la colonne NbRecrues
#'
#' @details
#' RecGaules et Rec doivent être triés de la même façon.
#'
#' @export
#'
rec_n_Gaules <- function(Rec, RecGaules, t, CovParmsGaules, RandomPlacGaules, Para.rec_gaules, seed_value=NULL){

  # Rec_data=Rec; t=t; Para.rec_n=Para.rec_n; RandomRec= RandomRec; seed_value=seed_value

  if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

  # liste des placettes
  list_plot <- unique(Rec$Placette)

  # paramètre de dispersion
  disp <- CovParmsGaules$ParameterEstimate[which(CovParmsGaules$SubModuleID==10 & CovParmsGaules$response=="disp")]

  # Rec: variables sur les arbres marchands, et contiendra le nombre de recrues marchandes
  # RecGaules: variables sur les gaules, nécessaires dans les équations pour prédire le nombre de recrues marchandes

  # calcul de la probabilité de recrues
  Rec <- rec_pi_Gaules(Rec, RecGaules, t, RandomPlacGaules, Para.rec_gaules)

  # calcul du Count
  Rec <- rec_count_Gaules(Rec, RecGaules, t, RandomPlacGaules, Para.rec_gaules)


  # répéter le fichier Rec 151 fois (maximum de 151 recrues générées)
  RecBase <- map_dfr(seq_len(151), ~Rec) %>% #dataframe de base pour le recrutement
    arrange(Placette, GrEspece) %>%
    mutate(m = rep(c(0:150), 10*length(list_plot))) # m = nombre de recrues


  # calcul de la probabilité de chacun des nombres de recrues de 0 à 150
  RecTot <- RecBase %>%
    mutate(mu = rep(c(0,rep(1,150)),10*length(list_plot))) %>%
    mutate(Pr = (gamma(m+1/disp))/(gamma(1/disp)*factorial(m))*(1/(predCount_nbrec*disp+1))^(1/disp)*((predCount_nbrec*disp)/(predCount_nbrec*disp+1))^m,
           Pr = (predPi_nbrec+(1-predPi_nbrec)*Pr)^(1-mu)*((1-predPi_nbrec)*Pr)^mu) %>%
    group_by(Placette, GrEspece) %>%
    mutate(CumPr = ifelse(m==150, 1, cumsum(Pr))) #Assure d'avoir un maximum de 150 recrues


  # déterminer le nombre de recrues à générer
  RecSelect <- RecTot %>%
    group_by(Placette, GrEspece) %>%
    mutate(Alea = runif(1)) %>%
    mutate(Valeur = CumPr > Alea) %>%
    filter(Valeur=="TRUE") %>%
    #summarise(NbRecrues = first(m)) %>%
    slice(1) %>% # slice au lieu de summarise pour pouvoir garder d'autres variables
    filter(m!=0) %>%
    rename(NbRecrues=m) %>%
    select(Placette, GrEspece, Pr, NbRecrues)


  return(RecSelect)

}
