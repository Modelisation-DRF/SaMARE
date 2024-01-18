

#'
#'
#' @param type_pe_Plac
#' @param ntrt
#' @param Rec    Nombre de  Recrues
#' @param Iterj  l'iteration souhaité
#' @param t0_aj_
#' @param st_tot0
#' @param Para.rec_n
#' @return
#' @examples

rec_delta<-function(Rec,st_tot0,type_pe_Plac,ntrt,t0_aj_,Iterj,Para.rec_n){

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
