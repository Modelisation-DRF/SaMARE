
#' Fonction qui prévoit la probabilité de mortalité de chacun des arbres
#' durant un pas de simulation.
#'
#' @param Mort Un dataframe qui contient la liste des arbres pour lesquels
#'            la probabilité de mortalité doit être évaluée ainsi que les
#'            caractéristiques de ses arbres qui seront utilisées
#'            pour en prévoir le risque de mortalité.
#' @param trt  Variable distinguant les peuplements traités des témoins, si St >26 = TEM.
#' @param temp  Température annuelle moyenne de la placette.
#' @param type_pe_Plac Variable indicatrice de la taille de la placette soit
#'                     400 m2, entre 2500 et 5000 m2 inclusivement ou
#'                     une autre dimension.
#' @param fact_red  Facteur de correction appliqué lorsqu'une coupe partielle a
#'                   été effectuée 3 ans ou moins avant la prévision.
#' @param Iterj  Itération en cours.
#' @param MCH Variable prenant la veleur de 1 en présence de maladie corticale du hêtre dans
#'            la placette et 0 lorsque la maladie est absente. Lorsque la maladie corticale
#'            est présente,la probabilité de mortalié des hêtres est estimée avec
#'            l'équation de l'avis technique AT-SSRF 20 de la Direction de la recherche forestière.
#' @param Para.mort Un dataframe  contenant les paramettres du module de mortalité.
#' @return Retourne le prédicteur linéaire de l'équation de la prévision du
#'         risque de mortalité. Les valeurs présites sont faites sans effets
#'         aléatoires, ceux-ci sont ajoutés dans la fonction SaMARE avant
#'         de convertir le prédicteur linéaire en probabilité de mortalité.
#' @examples
#'
#' # prévoit la probabilité de mortalité
#' resultat <- mort(Mort,trt,temp,type_pe_Plac,fact_red,t,Iterj,MCH,Para.mort)
#' print(resultat)
#'
#' @export

mort<-function(Mort,trt,temp,type_pe_Plac,fact_red,t,Iterj,MCH,Para.mort){

  select=dplyr::select

  nHEG<-nrow(Mort[which(Mort$GrEspece=="HEG"),])

  if (MCH==0|nHEG==0){

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

  }else{

    MortBck<-Mort
    MortHEG<-Mort %>% filter(GrEspece=="HEG")
    MortHEG<-MortHEG %>%
      mutate(cloglog=-5.4430+0.0621*DHPcm+log(t)) %>%
      select(ArbreID,cloglog)

    Mort<-Mort %>% filter(GrEspece!="HEG")

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
    Mort$cloglog <- (Xmort %*% BetaMat)

    MortTot<-Mort %>%
      select(ArbreID,cloglog) %>%
      rbind(MortHEG)
    Mort<-MortBck %>%
      inner_join(MortTot, by="ArbreID") %>%
      select(cloglog)

    return(Mort)


  }

}
