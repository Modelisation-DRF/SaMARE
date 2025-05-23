#' Fonction qui calcule de l'accroissement du diamètre des arbres pour un pas de simulation
#'
#' @param Accrois Un dataframe qui contient la liste des arbres et les variables nécessaire à
#'                l'équation d'accroissement (au minimum Placette, ArbreID, DHPcm, GrEspece, vigu0, prod0, st_tot0, fact_red, ntrt, type_pe_Plac)
#' @param t    La longueur du pas de simulation en annee (en annees).
#' @param Para.acc  Un dataframe contenant les paramettres du module d'accroissement.
#' @param RandomAcc Un dataframe contenant les effets aléatoires du module d'accroissement.
#' @param Res Un dataframe d'une colonne contenant les erreurs residuelles du module d'accroissement.
#' @return Retourne Accrois avec les colonnes DHPcm1, aam et pred_acc.

#' @export


accrois<-function(Accrois, t, Para.acc, RandomAcc, Res){

  # Accrois = data_test
  # Res = Residus[,k+2]

  select=dplyr::select

  n <- nrow(Accrois)

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
  Xacc[,29]<-Accrois$st_tot0
  Xacc[,30]<-log(t)
  Xacc[,31]<-Accrois$fact_red
  Xacc[,32:34]<-(Accrois$ntrt==listeNtrt)*1
  Xacc[,35:37]<-(Accrois$type_pe_Plac==listeTypePe)*1
  Xacc[,38:67]<-(Accrois$GrEspece==listeEssInter1 & Accrois$type_pe_Plac==listeTypePe)*1


  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaAcci<-Para.acc
  #%>%
  #filter(Iter==Iterj)

  # Construction matrice beta
  BetaMat<-matrix(ParaAcci$ParameterEstimate,ncol=1)

  # Calcul accroissement
  acc <-Xacc %*% BetaMat

  Accrois$xb_acc <- as.numeric(acc)

  # ajouter les effet aléaoire de placette et residuelles
  setDT(RandomAcc)
  setDT(Accrois)
  Accrois <- RandomAcc[, .(Placette, RandomPlac, RandomStep)][Accrois, on = .(Placette)]
  # ajouter l'erreur résiduelle
  Accrois$resid <- as.numeric(Res)
  Accrois[, pred_acc := (((xb_acc + RandomPlac + RandomStep + resid)^2) - 1)/10 # en cm
          ][, pred_acc := fifelse(pred_acc<0, 0, pred_acc)
            ][, pred_acc := round(pred_acc * 10) * .1 # arrondi à 1 décimale
              ][, DHPcm1 := fifelse(Etat1=="vivant", DHPcm + pred_acc, NA)
                ][, aam := fifelse(Etat1=="vivant", pred_acc/t, NA)
                  ]
  Accrois[, c("resid", "RandomPlac", "RandomStep") := NULL]
  setorder(Accrois,Placette,ArbreID)


  return(Accrois)

}
