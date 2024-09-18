#' Fonction qui calcule de l'Accroissement du diamètre en mm pour une période de temps
#'  égale au pas de simulation 't'.
#'
#' @param Accrois Un dataframe qui contient la liste des arbres à simuler ainsi
#'                que les informations à l'échelle de l'arbre qui sont
#'                utilisées pour prévoir l'accroissement en diamètre.
#' @param st_tot0  La surface terrière marchande de la placette en m2/ha au
#'                 début du pas de simulation.
#' @param t    La longueur du pas de simulation en annee (en annees).
#' @param fact_red  Facteur de correction appliqué lorsqu'une coupe partielle a
#'                  été effectuée 3 ans ou moins avant la prévision.
#' @param ntrt     Nombre de traitements de coupes partielles précédement
#'                 effectuées sur la placette.
#' @param type_pe_Plac Variable indicatrice de la taille de la placette soit
#'                      400 m2, soit entre 2500 et 5000 m2 inclusivement ou
#'                      soit une autre dimension.
#' @param Iterj Itération en cours.
#' @param Para.acc  Un dataframe  contenant les paramettres du module d'accroissement.
#' @return Retourne la prévision d'accroissement diamétral en mm pour un pas de simulation.
#'         Les    valeurs prédites sont faites sans effets aléatoires,
#'         ceux-ci sont ajoutés dans la fonction SaMARE.

#' @export


accrois<-function(Accrois ,st_tot0, t, fact_red, ntrt, type_pe_Plac, Iterj, Para.acc){

  select=dplyr::select

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
