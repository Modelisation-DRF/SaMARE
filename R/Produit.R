#' Fonction qui prévoit l'évolution de la classe de produit des tiges feuillus.
#'  Produit qui est contenu dans les classes "sciage" ou "pate".

#' @param Prod Un dataframe qui contient les arbres pour lesquels on veut
#'             prévoir l'évolution de la classe de produit des arbres feuillus.
#' @param type_pe_Plac Variable indicatrice de la taille de la placette soit
#'                      400 m2, soit entre 2500 et 5000 m2 inclusivement ou
#'                      une autre dimension.
#' @param rid1 Variable de groupement de variables écologiques.
#' @param Iterj Itération en cours.
#' @param Para.prod Un dataframe  contenant les parametres du module d'évolution
#'                 des produits des tiges feuillues.
#' @return Retourne le prédicteur linéaire de l'équation de la probabilité que
#'         la tige contienne un produit de type "sciage".
#' @export

produit<-function(Prod,type_pe_Plac,rid1,Iterj,Para.prod){
  select=dplyr::select

  n<-nrow(Prod)

  #Liste des effets
  listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
              rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))
  listeVigu0Inter3<-c(rep("NONVIG",n*3),rep("ViG",n*3))
  listeProd0<-c(rep("pate",n),rep("resineux",n),rep("sciage",n))
  listeTypePe<-c(rep("type0",n),rep("type1",n),rep("type2",n))
  listeRid1<-c(rep("2o",n),rep("3a",n),rep("3b",n),rep("3c",n),rep("3d",n),
               rep("4e",n),rep("4o",n),rep("DU",n),rep("SV",n))
  listeEssInter9<-c(rep("AUT",n*9),rep("BOJ",n*9),rep("EPX",n*9),rep("ERR",n*9),
                    rep("ERS",n*9),rep("FEN",n*9),rep("FIN",n*9),rep("HEG",n*9),
                    rep("RES",n*9),rep("SAB",n*9))


  #Construction de la matrice X

  Xprod<-matrix(0,ncol=140,nrow=n)
  Xprod[,1]<-1
  Xprod[,2:7]<-(Prod$vigu0==listeVigu0Inter3 & Prod$prod0==listeProd0)*1
  Xprod[,8:17]<-(Prod$GrEspece==listeEss)*rep(Prod$DHPcm,10)
  Xprod[,18:27]<-(Prod$GrEspece==listeEss)*rep(log(Prod$DHPcm),10)
  Xprod[,28:30]<-(type_pe_Plac==listeTypePe)*1
  Xprod[,31:40]<-(Prod$GrEspece==listeEss)*1
  Xprod[,41:49]<-(rid1==listeRid1)*1
  Xprod[,50:139]<-(Prod$GrEspece==listeEssInter9 & rid1==listeRid1)*1
  Xprod[,140]<-Prod$aam


  # selectionner les parametres de vigueur de l'itération
  ParaProdi<-Para.prod %>%
    filter(Iter==Iterj)

  # Création matrice Beta
  BetaMat<-matrix(ParaProdi$ParameterEstimate,ncol=1)

  # Calcul de la probabilité de vigueur
  logit <- (Xprod %*% BetaMat)

  return(logit)

}
