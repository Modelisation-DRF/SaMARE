#'Fonction qui prevoit le paramètre delta de la fonction Weibull qui est
#'la deuxième portion du modèles zero-inflated du nombre de recrues de SaMARE.
#'
#'
#'
#' @param type_pe_Plac Variable indicatrice de la taille de la placette soit
#'                     400 m2, entre 2500 et 5000 m2 inclusivement ou
#'                     une autre dimension.
#' @param ntrt  Nombre de traitements de coupes partielles précédement
#'               effectuées sur la placette.
#' @param Rec Un dataframe qui contient la prévision du nombre de recrues par
#'            groupes d'espèces.
#' @param Iterj Itération en cours.
#' @param t0_aj_ Temps écoulé depuis la dernière coupe partielle.
#' @param st_tot0  Surface terrière marchande (DHP >9.0cm) de la placette
#'                 au début du pas de simulation.
#' @param Para.rec_n Paramètres de l'équation du nombre de recrues qui n'utilise
#'                   pas les recrues comme prédicteurs.
#' @return  Retourne le prédicteur linéaire du paramètre delta de la fonction
#'          de prévision du nombre de recrues.
#' @examples

rec_delta<-function(Rec,st_tot0,type_pe_Plac,ntrt,t0_aj_,Iterj,Para.rec_n){
  select=dplyr::select
  n<-nrow(Rec)

  #Liste des effets

  listeGrEssRec<-c(rep("sab",n),rep("heg",n),rep("feu",n),rep("rex",n))

  # Construction matrice X
  Xrec_delta<-matrix(0,ncol=15,nrow=n)
  Xrec_delta[,1]<-1
  Xrec_delta[,2:5]<-(Rec$GrEssRec==listeGrEssRec)*1
  Xrec_delta[,6]<-st_tot0
  Xrec_delta[,7]<-(type_pe_Plac=="type0")*1
  Xrec_delta[,8]<-(ntrt>1)*1
  Xrec_delta[,9]<-(ntrt>1 & Rec$GrEssRec=="feu")*1
  Xrec_delta[,10]<-(ntrt>1 & Rec$GrEssRec=="heg")*1
  Xrec_delta[,11]<-ifelse(is.na(t0_aj_)==FALSE,t0_aj_,0)
  Xrec_delta[,12]<-Rec$logst_ess_1014
  Xrec_delta[,13]<-(Rec$GrEssRec=="heg")*Rec$logst_ess_1014
  Xrec_delta[,14]<-(Rec$GrEssRec=="rex")*Rec$logst_ess_1014
  Xrec_delta[,15]<-(Rec$GrEssRec=="feu")*Rec$logst_ess_1014



  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_delta<-Para.rec_n %>%
    filter(Iter==Iterj & response=="delta")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_delta$ParameterEstimate,ncol=1)

  # Calcul accroissement
  Xbeta <-Xrec_delta %*% BetaMat

  return(Xbeta)

}
