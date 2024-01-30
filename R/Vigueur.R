#' Fonction qui prévoit l'évolution de la classe de vigueur des arbres.
#'La vigueur peut prendre 2 valeurs soit "vigoureux" ou "non-vigoureux".




#' @param Vig Un dataframe qui contient les arbres pour lesquels on veut
#'             prévoir l'évolution de la classe de vigueur des arbres.
#' @param type_pe_Plac Variable indicatrice de la taille de la placette soit
#'                      400 m2, soit entre 2500 et 5000 m2 inclusivement ou
#'                      une autre dimension.
#' @param Iterj Itération en cours.
#' @param rid1 variable de groupement de variables écologiques.
#' @param Para.vig Un dataframe  contenant les parametres du module d'évolution
#'                 de la vigueur des arbres.
#' @return  Retourne le prédicteur linéaire de l'équation de la probabilité que
#'         l'arbre soit de classe "vigoureux".
#' @examples
#'

vig<-function(Vig,type_pe_Plac,rid1,Iterj,Para.vig){
  select=dplyr::select

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
