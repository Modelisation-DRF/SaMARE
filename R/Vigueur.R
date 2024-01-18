




#' @param Vig
#' @param type_pe_Plac
#' @param Iterj
#' @param rid1
#' @param Para.vig
#' @return
#' @examples
#'

vig<-function(Vig,type_pe_Plac,rid1,Iterj,Para.vig){

  n<-nrow(Vig)

  #Liste des effets
  listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
              rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))
  listeVigu0<-c(rep("NONVIG",n),rep("ViG",n))
  listeVigu0Inter3<-c(rep("NONVIG",n*3),rep("ViG",n*3))
  listeProd0<-c(rep("pate",n),rep("resineux",n),rep("sciage",n))
  listeTypePe<-c(rep("type0",n),rep("type1",n),rep("type2",n))
  listeRid1<-c(rep("2o",n),rep("3a",n),rep("3b",n),rep("3c",n),rep("3d",n),
               rep("4e",n),rep("4o",n),rep("DU",n),rep("SV",n))
  listeEssInter9<-c(rep("AUT",n*9),rep("BOJ",n*9),rep("EPX",n*9),rep("ERR",n*9),
                    rep("ERS",n*9),rep("FEN",n*9),rep("FIN",n*9),rep("HEG",n*9),
                    rep("RES",n*9),rep("SAB",n*9))


  #Construction de la matrice X

  Xvig<-matrix(0,ncol=131,nrow=n)
  Xvig[,1]<-1
  Xvig[,2:7]<-(Vig$vigu0==listeVigu0Inter3 & Vig$prod0==listeProd0)*1
  Xvig[,8:17]<-(Vig$GrEspece==listeEss)*1
  Xvig[,18]<-Vig$DHPcm
  Xvig[,19:28]<-(Vig$GrEspece==listeEss)*rep(log(Vig$DHPcm),10)
  Xvig[,29:31]<-(type_pe_Plac==listeTypePe)*1
  Xvig[,32:40]<-(rid1==listeRid1)*1
  Xvig[,41:130]<-(Vig$GrEspece==listeEssInter9 & rid1==listeRid1)*1
  Xvig[,131]<-Vig$aam


  # selectionner les parametres de vigueur de l'itération
  ParaVigi<-Para.vig %>%
    filter(Iter==Iterj)

  # Création matrice Beta
  BetaMat<-matrix(ParaVigi$ParameterEstimate,ncol=1)

  # Calcul de la probabilité de vigueur
  logit <- (Xvig %*% BetaMat)

  return(logit)

}
