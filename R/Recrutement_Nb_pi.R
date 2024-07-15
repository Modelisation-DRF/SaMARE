#'Fonction qui prévoie la probabilité de présence de recrues par grouoe d'espèce.
#'Cette fonction corespond à la première portion de la fonction zero-inflated
#' de prévision du nombre de recrues non basé sur les gaules.
#'
#'
#' @param type_pe_Plac Variable indicatrice de la taille de la placette soit
#'                     400 m2, entre 2500 et 5000 m2 inclusivement ou
#'                     une autre dimension.
#' @param ntrt  Nombre de traitements de coupes partielles précédement
#'              effectuées sur la placette.
#' @param Rec  Un dataframe qui contient la prévision du nombre de recrues par
#'            groupes d'espèces.
#' @param Iterj Itération en cours.
#' @param t0_aj_  Temps écoulé depuis la dernière coupe partielle.
#' @param st_tot0  Surface terrière marchande (DHP >9.0cm) de la placette
#'                 au début du pas de simulation.
#' @param t  La longueur du pas de simulation en annee (en annees).
#' @return  Retourne le prédicteur linéaire de l'équation de la prévision
#'          de la présence de recrues par groupe d'espèce.
#' @examples
#'
#'  #resultat <- rec_pi(Rec,t,st_tot0,ntrt,t0_aj_,type_pe_Plac,Iterj,Para.rec_n)
#' print(resultat)

rec_pi<-function(Rec,t,st_tot0,ntrt,t0_aj_,type_pe_Plac,Iterj,Para.rec_n){
  select=dplyr::select
  n<-nrow(Rec)

  #Liste des effets

  listeGrEssRec<-c(rep("sab",n),rep("heg",n),rep("feu",n),rep("rex",n))

  # Construction matrice X
  Xrec_pi<-matrix(0,ncol=21,nrow=n)
  Xrec_pi[,1]<-1
  Xrec_pi[,2:5]<-(Rec$GrEssRec==listeGrEssRec)*1
  Xrec_pi[,6]<-log(t)
  Xrec_pi[,7]<-st_tot0
  Xrec_pi[,8]<-(ntrt>1)*1
  Xrec_pi[,9]<-(ntrt>1 & Rec$GrEssRec=="heg")*1
  Xrec_pi[,10]<-(ntrt>1 & Rec$GrEssRec=="feu")*1
  Xrec_pi[,11]<-ifelse(is.na(t0_aj_)==FALSE,t0_aj_,0)
  Xrec_pi[,12]<-(type_pe_Plac=="type0")*1
  Xrec_pi[,13]<-(type_pe_Plac=="type0" & Rec$GrEssRec=="sab")*1
  Xrec_pi[,14]<-Rec$logst_ess_1014
  Xrec_pi[,15]<-(Rec$GrEssRec=="sab")*Rec$logst_ess_1014
  Xrec_pi[,16]<-(Rec$GrEssRec=="feu")*Rec$logst_ess_1014
  Xrec_pi[,17]<-(type_pe_Plac=="type0")*Rec$logst_ess_1014
  Xrec_pi[,18]<-(type_pe_Plac=="type0" & Rec$GrEssRec=="heg")*Rec$logst_ess_1014
  Xrec_pi[,19]<-(type_pe_Plac=="type0" & Rec$GrEssRec=="feu")*Rec$logst_ess_1014
  Xrec_pi[,20]<-(type_pe_Plac=="type0" & Rec$GrEssRec=="rex")*Rec$logst_ess_1014
  Xrec_pi[,21]<-ifelse(is.na(t0_aj_)==FALSE,t0_aj_,0)*Rec$logst_ess_1014

  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_pi<-Para.rec_n %>%
    filter(Iter==Iterj & response=="pi")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_pi$ParameterEstimate,ncol=1)

  # Calcul accroissement
  logit <-Xrec_pi %*% BetaMat

  return(logit)

}
