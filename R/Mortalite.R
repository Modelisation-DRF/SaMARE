
#' calcule de la mortalité
#'
#' @param Mort liste des mort
#' @param trt  Variable du peuplement residuel avec condition que si St >26 = TEM
#' @param temp     temperature Variables climatiques de la placette
#' @param type_pe_Plac Type de placette ex: type0, type1, ou type2
#' @param fact_red  facteur de Réduction de la mortalité
#' @param Iterj  Iteration choisie
#' @param Para.mort Parametres de mortalité
#' @return
#' @examples

mort<-function(Mort,trt,temp,type_pe_Plac,fact_red,t,Iterj,Para.mort){

  n<-nrow(Mort)

  #Liste des effets
  listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
              rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))
  listeVigu0<-c(rep("NONVIG",n),rep("ViG",n))
  listeProd0<-c(rep("pate",n),rep("resineux",n),rep("sciage",n))
  listeNtrt<-c(rep(2,n),rep(1,n),rep(0,n))
  listeTypePe<-c(rep("type0",n),rep("type1",n),rep("type2",n))
  listeEssInter2<-c(rep("AUT",n*2),rep("BOJ",n*2),rep("EPX",n*2),rep("ERR",n*2),
                    rep("ERS",n*2),rep("FEN",n*2),rep("FIN",n*2),rep("HEG",n*2),
                    rep("RES",n*2),rep("SAB",n*2))
  listeEssInter3<-c(rep("AUT",n*3),rep("BOJ",n*3),rep("EPX",n*3),rep("ERR",n*3),
                    rep("ERS",n*3),rep("FEN",n*3),rep("FIN",n*3),rep("HEG",n*3),
                    rep("RES",n*3),rep("SAB",n*3))

  #Construction de la matrice X

  Xmort<-matrix(0,ncol=75,nrow=n)
  Xmort[,1]<-1
  Xmort[,2:3]<-(Mort$vigu0==listeVigu0)*1
  Xmort[,4:13]<-(Mort$GrEspece==listeEss)*1
  Xmort[,14:33]<-(Mort$GrEspece==listeEssInter2 & Mort$vigu0==listeVigu0)*1
  Xmort[,34]<-Mort$DHPcm
  Xmort[,35]<-Mort$DHPcm*Mort$DHPcm
  Xmort[,36:38]<-(Mort$prod0==listeProd0)*1
  Xmort[,39:68]<-(Mort$GrEspece==listeEssInter3 & Mort$prod0==listeProd0)*1
  Xmort[,69]<-(trt=="CP")*1
  Xmort[,70]<-temp
  Xmort[,71:73]<-(type_pe_Plac==listeTypePe)*1
  Xmort[,74]<-fact_red
  Xmort[,75]<-log(t)

  # selectionner les parametres de mortalité de l'itération
  ParaMorti<-Para.mort %>%
    filter(Iter==Iterj)

  # Création matrice Beta
  BetaMat<-matrix(ParaMorti$ParameterEstimate,ncol=1)

  # Calcul de la probabilité de mortalité
  cloglog <- (Xmort %*% BetaMat)

  return(cloglog)

}
