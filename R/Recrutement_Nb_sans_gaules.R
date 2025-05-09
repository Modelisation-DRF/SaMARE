#### Modèle de probabilité de recrue par essence  ####

#'Fonction qui prévoie la probabilité de présence de recrues par grouoe d'espèce.
#'Cette fonction corespond à la première portion de la fonction zero-inflated
#' de prévision du nombre de recrues non basé sur les gaules.
#'
#'
#' @param Rec  Un dataframe de placette/grespece avec les colonnes Placette, GrEspece, GrEssRec, st_tot0, ntrt, t0_aj_, type_pe_Plac, logst_ess_1014
#' @param t  La longueur du pas de simulation en annee (en annees).
#' @param Para.rec_n Paramètres de l'équation de prévision du nombre de
#'                        recrues sans utiliser le nombre de gaules.
#' @param RandomRec Effets aléatoires du module de probabilité de
#'                        recrues sans utiliser le nombre de gaules.
#' @return  Retourne Rec avec la colonne predPi_nbrec et xb_prob_rec.
#' @export
rec_pi <- function(Rec ,t, Para.rec_n, RandomRec){
  select=dplyr::select
  n<-nrow(Rec)

  #Liste des effets

  listeGrEssRec<-c(rep("sab",n),rep("heg",n),rep("feu",n),rep("rex",n))

  # Construction matrice X
  Xrec_pi<-matrix(0,ncol=21,nrow=n)
  Xrec_pi[,1]<-1
  Xrec_pi[,2:5]<-(Rec$GrEssRec==listeGrEssRec)*1
  Xrec_pi[,6]<-log(t)
  Xrec_pi[,7]<-Rec$st_tot0
  Xrec_pi[,8]<-(Rec$ntrt>1)*1
  Xrec_pi[,9]<-(Rec$ntrt>1 & Rec$GrEssRec=="heg")*1
  Xrec_pi[,10]<-(Rec$ntrt>1 & Rec$GrEssRec=="feu")*1
  Xrec_pi[,11]<-ifelse(is.na(Rec$t0_aj_)==FALSE,Rec$t0_aj_,0)
  Xrec_pi[,12]<-(Rec$type_pe_Plac=="type0")*1
  Xrec_pi[,13]<-(Rec$type_pe_Plac=="type0" & Rec$GrEssRec=="sab")*1
  Xrec_pi[,14]<-Rec$logst_ess_1014
  Xrec_pi[,15]<-(Rec$GrEssRec=="sab")*Rec$logst_ess_1014
  Xrec_pi[,16]<-(Rec$GrEssRec=="feu")*Rec$logst_ess_1014
  Xrec_pi[,17]<-(Rec$type_pe_Plac=="type0")*Rec$logst_ess_1014
  Xrec_pi[,18]<-(Rec$type_pe_Plac=="type0" & Rec$GrEssRec=="heg")*Rec$logst_ess_1014
  Xrec_pi[,19]<-(Rec$type_pe_Plac=="type0" & Rec$GrEssRec=="feu")*Rec$logst_ess_1014
  Xrec_pi[,20]<-(Rec$type_pe_Plac=="type0" & Rec$GrEssRec=="rex")*Rec$logst_ess_1014
  Xrec_pi[,21]<-ifelse(is.na(Rec$t0_aj_)==FALSE,Rec$t0_aj_,0)*Rec$logst_ess_1014


  # selectionner les parametres
  ParaRec_pi<-Para.rec_n %>%
    filter(response=="pi")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_pi$ParameterEstimate,ncol=1)

  # Calcul
  logit <- Xrec_pi %*% BetaMat
  Rec$xb_prob_rec <- as.numeric(logit)

  setDT(Rec)
  setDT(RandomRec)
  Rec <- RandomRec[, .(Placette, RandomPlac)][Rec, on = .(Placette)]

  # calculer les prévisions
  Rec$predPi_nbrec <- exp(Rec$xb_prob_rec+Rec$RandomPlac)/(1+exp(Rec$xb_prob_rec+Rec$RandomPlac))
  Rec[, c("RandomPlac") := NULL]

  return(Rec)

}

#### Modèle du paramètre lambda de la fct de Weibull pour le nombre de recrues ####

#'Fonction qui prevoit le paramètre lambda de la fonction Weibull qui est
#'la deuxième portion du modèles zero-inflated du nombre de recrues de SaMARE.
#'
#' @param t  La longueur du pas de simulation en annee (en annees).
#' @param Rec  Un dataframe de placewtte/grespece avec les colonnes Placette, GrEspece, GrEssRec, type_pe_Plac, st_tot0, logst_ess_1014.
#' @param Para.rec_n  Paramètres de l'équation du lambda du modèle de nombre de recrues qui n'utilise
#'                   pas les recrues comme prédicteurs.
#' @return  Retourne Rec avec la colonne predLambda_nrec et xb_lambda_rec.

rec_lambda <- function(Rec, t, Para.rec_n){
  select=dplyr::select
  n<-nrow(Rec)

  #Liste des effets

  listeGrEssRec<-c(rep("sab",n),rep("heg",n),rep("feu",n),rep("rex",n))

  # Construction matrice X
  Xrec_lambda<-matrix(0,ncol=12,nrow=n)
  Xrec_lambda[,1]<-1
  Xrec_lambda[,2:5]<-(Rec$GrEssRec==listeGrEssRec)*1
  Xrec_lambda[,6]<-(Rec$type_pe_Plac=="type0")*1
  Xrec_lambda[,7]<-Rec$st_tot0
  Xrec_lambda[,8]<-log(t)
  Xrec_lambda[,9]<-Rec$logst_ess_1014
  Xrec_lambda[,10]<-(Rec$GrEssRec=="heg")*Rec$logst_ess_1014
  Xrec_lambda[,11]<-(Rec$GrEssRec=="rex")*Rec$logst_ess_1014
  Xrec_lambda[,12]<-(Rec$GrEssRec=="feu")*Rec$logst_ess_1014



  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_lambda<-Para.rec_n %>%
    filter(response=="lambda")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_lambda$ParameterEstimate,ncol=1)

  # Calcul accroissement
  Rec$xb_lambda_rec <- as.numeric(Xrec_lambda %*% BetaMat)

  Rec$predLambda_nrec <- exp(Rec$xb_lambda_rec)

  return(Rec)

}

#### Modèle du paramètre delta de la fct de Weibull pour le nombre de recrues ####

#'Fonction qui prevoit le paramètre delta de la fonction Weibull qui est
#'la deuxième portion du modèles zero-inflated du nombre de recrues de SaMARE.
#'
#' @param Rec Un dataframe dfe placettes/grespece avec les colonnes Placette, GrEspece, GrEssRec, st_tot0, ntrt, t0_aj_, logst_ess_1014
#' @param Para.rec_n Paramètres de l'équation du Delta modèle du nombre de recrues qui n'utilise
#'                   pas les recrues comme prédicteurs.
#' @return  Retourne Rec avec la colonne predDelta_nrec et xb_delta_rec.
#' @export
rec_delta <- function(Rec, Para.rec_n){
  select=dplyr::select
  n<-nrow(Rec)

  #Liste des effets

  listeGrEssRec<-c(rep("sab",n),rep("heg",n),rep("feu",n),rep("rex",n))

  # Construction matrice X
  Xrec_delta<-matrix(0,ncol=15,nrow=n)
  Xrec_delta[,1]<-1
  Xrec_delta[,2:5]<-(Rec$GrEssRec==listeGrEssRec)*1
  Xrec_delta[,6]<-Rec$st_tot0
  Xrec_delta[,7]<-(Rec$type_pe_Plac=="type0")*1
  Xrec_delta[,8]<-(Rec$ntrt>1)*1
  Xrec_delta[,9]<-(Rec$ntrt>1 & Rec$GrEssRec=="feu")*1
  Xrec_delta[,10]<-(Rec$ntrt>1 & Rec$GrEssRec=="heg")*1
  Xrec_delta[,11]<-ifelse(is.na(Rec$t0_aj_)==FALSE,Rec$t0_aj_,0)
  Xrec_delta[,12]<-Rec$logst_ess_1014
  Xrec_delta[,13]<-(Rec$GrEssRec=="heg")*Rec$logst_ess_1014
  Xrec_delta[,14]<-(Rec$GrEssRec=="rex")*Rec$logst_ess_1014
  Xrec_delta[,15]<-(Rec$GrEssRec=="feu")*Rec$logst_ess_1014



  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_delta<-Para.rec_n %>%
    filter(response=="delta")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_delta$ParameterEstimate,ncol=1)

  # Calcul accroissement
  Rec$xb_delta_rec <- as.numeric(Xrec_delta %*% BetaMat)

  Rec$predDelta_nrec <- exp(Rec$xb_delta_rec)

  return(Rec)

}


#### Calcul du nombre de recrues par essence ####

#' Fonction qui prévoie le nombre de recrues par groupe d'espèces.
#' avec les fonctions rec_pi, rec_lambda et rec_delta
#'
#' @param Rec_data  Un dataframe des 10 groupes d'espèces pour chacune des placettes, avec les colonnes Placette, GrEspece,
#'                  et les variables nécessaires pour les fonctions rec_pi, rec_delta, rec_lambda
#' @param t  La longueur du pas de simulation en annee (en annees).
#' @param Para.rec_n  Paramètres des équations pour le nombre de recrues
#' @param RandomRec Effets aléatoires du module de probabilité de
#'                        recrues sans utiliser le nombre de gaules.
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return  Retourne un dataframe de placette/grespece avec la colonne NbRecrues.
#' @export
rec_n <- function(Rec_data, t, Para.rec_n, RandomRec, seed_value=NULL){

  # Rec_data=Rec; t=t; Para.rec_n=Para.rec_n; RandomRec= RandomRec; seed_value=seed_value

  if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

  Rec <- Rec_data

  # liste des placettes
  list_plot <- unique(Rec$Placette)

  # estimer les 3 paramètres pour le calcul du nombre de recrues
  Rec <- rec_pi(Rec, t, Para.rec_n, RandomRec)
  Rec <- rec_delta(Rec, Para.rec_n)
  Rec <- rec_lambda(Rec, t, Para.rec_n)

  # répéter le fichier Rec 151 fois (maximum de 151 recrues générées)
  RecBase <- map_dfr(seq_len(151), ~Rec) %>% #dataframe de base pour le recrutement
    arrange(Placette, GrEspece) %>%
    mutate(m = rep(c(0:150), 10*length(list_plot))) # m = nombre de recrues

  # calcul de la probabilité de chacun des nombres de recrues de 0 à 150
  RecTot <- RecBase %>%
    mutate(mu = rep(c(0,rep(1,150)),10*length(list_plot))) %>%
    mutate(Pr = predPi_nbrec^(1-mu) * ((1-predPi_nbrec)*(exp(-predDelta_nrec*m^predLambda_nrec)-exp(-predDelta_nrec*(m+1)^predLambda_nrec))/exp(-predDelta_nrec))^mu) %>%
    group_by(Placette, GrEspece) %>%
    mutate(CumPr = cumsum(Pr))

  # déterminer le nombre de recrues à générer
  RecSelect <- RecTot %>%
    group_by(Placette, GrEspece) %>%
    mutate(Alea = runif(1)) %>%
    mutate(Valeur = CumPr > Alea) %>%
    filter(Valeur=="TRUE") %>%
    summarise(NbRecrues = first(m)) %>%
    filter(NbRecrues!=0)


  return(RecSelect)

}
