

#'
#'
#' @param type_pe_Plac
#' @param ntrt
#' @param Rec    Nombre de  Recrues
#' @param Iterj  l'iteration souhaité
#' @param t0_aj_
#' @param st_tot0
#' @param t
#' @return
#' @examples

rec_pi<-function(Rec,t,st_tot0,ntrt,t0_aj_,type_pe_Plac,Iterj){

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
