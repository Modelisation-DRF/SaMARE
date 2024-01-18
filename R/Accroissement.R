#' calcule l'Accroissement
#'
#' @param Accrois Data frame   contient les placette
#' @param st_tot0  Nombre variable echelle placette
#' @param t    Nombre  période de temps entre chaque simulation
#' @param fact_red  Nombre  facteur de Réduction de la mortalité
#' @param ntrt     Nombre  le nombre de traitements
#' @param type_pe_Plac Character  Type de placette
#' @param Iterj Nombre Iteration
#' @param Para.acc  Data frame  paramettres d'accroissement
#' @return
#' @examples   accrois(Accrois,21.09462,5,0,0,"type1",1,Para.acc)


accrois<-function(Accrois ,st_tot0, t, fact_red, ntrt, type_pe_Plac, Iterj, Para.acc){

  n<-nrow(Accrois)

  #Liste des effets
  listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
              rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))
  listeVigu0<-c(rep("NONVIG",n),rep("ViG",n))
  listeProd0<-c(rep("pate",n),rep("resineux",n),rep("sciage",n))
  listeNtrt<-c(rep(0,n),rep(1,n),rep(2,n))
  listeTypePe<-c(rep("type0",n),rep("type1",n),rep("type2",n))
  listeEssInter1<-c(rep("AUT",n*3),rep("BOJ",n*3),rep("EPX",n*3),rep("ERR",n*3),
                    rep("ERS",n*3),rep("FEN",n*3),rep("FIN",n*3),rep("HEG",n*3),
                    rep("RES",n*3),rep("SAB",n*3))

  # Construction matrice X
  Xacc<-matrix(0,ncol=67,nrow=n)
  Xacc[,1]<-1
  Xacc[,2]<-Accrois$DHPcm
  Xacc[,3]<-Accrois$DHPcm*Accrois$DHPcm
  Xacc[,4:13]<-(Accrois$GrEspece==listeEss)*1
  Xacc[,14:23]<-(Accrois$GrEspece==listeEss)*Accrois$DHPcm
  Xacc[,24:25]<-(Accrois$vigu0==listeVigu0)*1
  Xacc[,26:28]<-(Accrois$prod0==listeProd0)*1
  Xacc[,29]<-st_tot0
  Xacc[,30]<-log(t)
  Xacc[,31]<-fact_red
  Xacc[,32:34]<-(ntrt==listeNtrt)*1
  Xacc[,35:37]<-(type_pe_Plac==listeTypePe)*1
  Xacc[,38:67]<-(Accrois$GrEspece==listeEssInter1 & type_pe_Plac==listeTypePe)*1

  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaAcci<-Para.acc %>%
    filter(Iter==Iterj)
  # Construction matrice beta
  BetaMat<-matrix(ParaAcci$ParameterEstimate,ncol=1)

  # Calcul accroissement
  acc <-Xacc %*% BetaMat

  return(acc)

}
