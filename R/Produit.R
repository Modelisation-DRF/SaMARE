#' Fonction qui prévoit l'évolution de la classe de produit des tiges feuillus.
#'
#' @param Prod Un dataframe qui contient les arbres avec au minimum les colonnes Placette, GrEspece, vigu0, type_pe_Plac, rid1, aam
#' @param Para.prod Un dataframe  contenant les parametres du module d'évolution
#'                 des produits des tiges feuillues.
#' @param RandomProd Un dataframe  contenant les effets aléatoires du module d'évolution
#'                 des produits des tiges feuillues.
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#' @return Retourne Prod avec les colonnes prod1 et prob_prod
#' @export

produit <- function(Prod, Para.prod, RandomProd, seed_value=NULL){

  if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

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
  Xprod[,28:30]<-(Prod$type_pe_Plac==listeTypePe)*1
  Xprod[,31:40]<-(Prod$GrEspece==listeEss)*1
  Xprod[,41:49]<-(Prod$rid1==listeRid1)*1
  Xprod[,50:139]<-(Prod$GrEspece==listeEssInter9 & Prod$rid1==listeRid1)*1
  Xprod[,140]<-Prod$aam



  # selectionner les parametres de vigueur de l'itération
  ParaProdi<-Para.prod
  #%>%
  #filter(Iter==Iterj)

  # Création matrice Beta
  BetaMat<-matrix(ParaProdi$ParameterEstimate,ncol=1)

  # Calcul de la probabilité de vigueur
  logit <- (Xprod %*% BetaMat)
  Prod$xb_prod <- as.numeric(logit)


  # ajout de l'effet aléatoire de placette
  setDT(RandomProd)
  setDT(Prod)
  Prod <- RandomProd[, .(Placette, RandomPlac)][Prod, on = .(Placette)]

  Prod[, `:=`(
    prob_prod = xb_prod + RandomPlac
  )
  ][, `:=`(
    prob_prod = exp(prob_prod)/(1+exp(prob_prod)),
    Alea = runif(.N)
  )
  ][, `:=`(
    prod1 = fifelse(Etat1=='mort', NA,
                    fifelse(prod0=="resineux", "resineux",
                            fifelse(GrEspece=="AUT"|(vigu0=="NONVIG" & DHPcm1<23.1), "pate",
                                    fifelse(Alea <= prob_prod, "sciage",
                                            "pate"))))
  )
  ]
  Prod[, c("Alea", "RandomPlac") := NULL]


  return(Prod)

}
