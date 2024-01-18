

#'
#'
#' @param type_pe_Plac
#' @param t
#' @param Rec    Nombre de  Recrues
#' @param Iterj  l'iteration souhaité
#' @param st_tot0
#' @param Para.rec_n
#' @return
#' @examples


rec_lambda<-function(Rec,type_pe_Plac,st_tot0,t,Iterj,Para.rec_n){

  n<-nrow(Rec)

  #Liste des effets

  listeGrEssRec<-c(rep("sab",n),rep("heg",n),rep("feu",n),rep("rex",n))

  # Construction matrice X
  Xrec_lambda<-matrix(0,ncol=12,nrow=n)
  Xrec_lambda[,1]<-1
  Xrec_lambda[,2:5]<-(Rec$GrEssRec==listeGrEssRec)*1
  Xrec_lambda[,6]<-(type_pe_Plac=="type0")*1
  Xrec_lambda[,7]<-st_tot0
  Xrec_lambda[,8]<-log(t)
  Xrec_lambda[,9]<-Rec$logst_ess_1014
  Xrec_lambda[,10]<-(Rec$GrEssRec=="heg")*Rec$logst_ess_1014
  Xrec_lambda[,11]<-(Rec$GrEssRec=="rex")*Rec$logst_ess_1014
  Xrec_lambda[,12]<-(Rec$GrEssRec=="feu")*Rec$logst_ess_1014



  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_lambda<-Para.rec_n %>%
    filter(Iter==Iterj & response=="lambda")
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_lambda$ParameterEstimate,ncol=1)

  # Calcul accroissement
  Xbeta <-Xrec_lambda %*% BetaMat

  return(Xbeta)

}

