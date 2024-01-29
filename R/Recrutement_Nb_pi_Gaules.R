#'Fonction qui prévoie la probabilité d'absence  de recrues par grouoe d'espèce.
#'Cette fonction corespond à la première portion de la fonction zero-inflated
#' de prévision du nombre de recrues basé sur les gaules de Rijal et al. 2023.
#'
#'
#' @param RecGaules Dataframe qui contient les information sur la distribution
#'                  des gaules dans la placette.
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param Rec  Un dataframe qui contient la prévision du nombre de recrues par
#'             groupes d'espèces.
#' @param Iterj  Itération en cours.
#' @param RandomPlacGaules  Un dataframe contenant les effets aléatoires à
#'                          l'échelle de la placette du module de
#'                          recrutement basé sur les gaules et du module
#'                          d'évolution des gaules.
#' @param st_tot0  Surface terrière marchande (DHP >9.0cm) de la placette
#'                 au début du pas de simulation.
#' @param Para.rec_gaules Paramètres de l'équation de prévivion du nombre de
#'                        recrues utilisant le nombre de gaules.
#' @return  Retourne une prévision de la probabilité d'absence de recrues,
#'           prévisions basées sur les informations provenant des recrues.
#' @examples


rec_pi_Gaules<-function(Rec,RecGaules,t,st_tot0,Iterj,RandomPlacGaules,Para.rec_gaules){
  select=dplyr::select
  n<-nrow(Rec)

  #Liste des effets

  listeGrEss1<-c(rep("AUT",n),rep("EPX",n),rep("ERR",n),rep("FEN",n),
                 rep("FIN",n),rep("HEG",n),rep("RES",n))

  listeGrEss2<-c(rep("ERS",n),rep("HEG",n),rep("BOJ",n),rep("SAB",n))

  # Construction matrice X
  Xrec_pi<-matrix(0,ncol=14,nrow=n)
  Xrec_pi[,1]<-st_tot0
  Xrec_pi[,2]<-t
  Xrec_pi[,3:9]<-(Rec$GrEspece==listeGrEss1)*Rec$lnNb_Ess_Ha
  Xrec_pi[,10]<-(RecGaules$GrEspece=="BOJ")*RecGaules$lnNb_Gaules_Ess_Ha
  Xrec_pi[,11:14]<-(RecGaules$GrEspece==listeGrEss2)*RecGaules$lnNb_Gaules_68_Ess_Ha

  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_pi<-Para.rec_gaules %>%
              filter(Iter==Iterj & response=="pi")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_pi$ParameterEstimate,ncol=1)

  # Calcul
  logit <-Xrec_pi %*% BetaMat

  Random<-RandomPlacGaules$RandomPlac[which(RandomPlacGaules$SubModuleID==10 & RandomPlacGaules$response=="pi")]

  pred<-1/(1+exp(-(logit+Random)))

  return(pred)

}
