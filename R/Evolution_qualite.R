#' Fonction qui prévoit l'évolution de la classe de qualité des arbres.
# 'La fonction se base sur les équation des Filip Havreljuk pour SaMARE 2018
# 'elle ne prévoit pas la qualité des arbre qui n'ont pas de valeur de qualité dans
# 'les données d'origines (ex. arbres qui passent le seuil de la classe de 24 cm".
#'
#' @param PlacQual Un dataframe qui contient une liste d'arbres avec au minimum les colonnes Placette, ArbreID,
#'                 GrEspece ABCD Etat1 DHPcm1 type_pe_Plac vigu0 prod0 aam rid1 prec.
#' @param Para.EvolQualTot Un dataframe  contenant les paramètres du module d'évolution
#'                 de la qualité des arbres.
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#' @return  PlacQual avec les colonnes ABCD, Intercept1, Intercept2, Intercept3.
#' @export
#'

EvolQual <- function(PlacQual, Para.EvolQualTot, seed_value=NULL){

  # PlacQual=data_test_module_qual;
  # PlacQual= EvolQual

  PlacQual1 = PlacQual

  # evolution de la qualité des survivants (seuls les feuillus ont une qualité ABCD)
  PlacQual <- PlacQual1 %>%
    filter(ABCD %in% c("A","B","C","D") & Etat1=="vivant" & GrEspece %in% c("BOJ","ERR","ERS","FEN","FIN","HEG")) # changer etat pour etat1 pour le faire seulement pour les survivants, ça ne sert à rien de le faire sur les morts

  if (nrow(PlacQual)>0){


  if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

  select=dplyr::select

  # ne s'applique qu'aux feuillus, pas à AUT, donc à "BOJ","ERR","ERS","FEN","FIN","HEG",
  # pas aux "EPX","RES","SAB", "AUT"
  # 3 équations:
  # Eq1: 23.1 à 33.0: peuvent seulement être des C D
  # Eq2: 33.1 à 39.0: peuvent seulement être des B C D
  # Eq3: 39.1 et +  : peuvent seulement être des A B C D
  # il n'y a pas d'Eq3 pour ERR-FIN-FEN, utiliser Eq2
  # pour FIN, utiliser les équations de ERR
  PlacQual <- PlacQual %>%
            mutate(GrDHP = ifelse(DHPcm1<33.1, "C",
                                  ifelse(DHPcm1>=39.1 & GrEspece %in% c("BOJ","ERS","HEG"),
                                         "A", "B"))) %>%
            mutate(dtr = ifelse((DHPcm<33.1 & DHPcm1>=33.1) | (DHPcm<39.1 & DHPcm1>=39.1 & GrEspece %in% c("BOJ","ERS","HEG")), 1, 0),  # variable indiquant une transition
                   Intercept=1) %>%
            mutate(ABCD = ifelse(GrEspece %in% c("ERR","FIN","FEN") & ABCD=="A","B", ABCD))

  PlacQualB <- PlacQual %>% filter(DHPcm1>=33.1) %>% mutate(Intercept=2)
  PlacQualA <- PlacQual %>% filter(DHPcm1>=39.1 & GrEspece %in% c("BOJ","ERS","HEG")) %>% mutate(Intercept=3)
  PlacQual = rbind(PlacQual, PlacQualB, PlacQualA) %>% arrange(Placette, ArbreID, Intercept)



  n<-nrow(PlacQual)

  #Liste des effets

  listeRid1<-c(rep("2o",n),rep("3a",n),rep("3b",n),rep("3c",n),rep("4e",n),rep("4o",n),rep("DU",n))
  listeType<-c(rep("type0",n),rep("type1",n))
  listeQualB<-c(rep("B",n),rep("C",n))
  listeQualA<-c(rep("A",n),rep("B",n),rep("C",n))
  listeInterceptB<-c(rep(1,n),rep(2,n))
  listeInterceptA<-c(rep(1,n),rep(2,n),rep(3,n))
  listeVigueur<-c(rep("NONVIG",n),rep("NONVIG",n),rep("ViG",n))
  listeProduits<-c(rep("pate",n),rep("sciage",n),rep("pate",n))

  #Construction de la matrice X
#EffetsBOJ1
  XBOJ<-matrix(0,ncol=28,nrow=n)
  XBOJ[,1]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="C")
  XBOJ[,2]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="C" & PlacQual$ABCD=="C")
  XBOJ[,3:4]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="C" & PlacQual$type_pe_Plac==listeType)
  XBOJ[,5:7]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="C" & PlacQual$vigu0==listeVigueur & PlacQual$prod0==listeProduits)


  XBOJ[,8:9]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="B" &  PlacQual$Intercept==listeInterceptB)
  XBOJ[,10:11]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="B" &  PlacQual$ABCD==listeQualB)
  XBOJ[,12]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="B" &  PlacQual$dtr==1)
  XBOJ[,13:15]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="B" & PlacQual$vigu0==listeVigueur & PlacQual$prod0==listeProduits)


  XBOJ[,16:18]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="A" &  PlacQual$Intercept==listeInterceptA)
  XBOJ[,19:21]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="A" &  PlacQual$ABCD==listeQualA)
  XBOJ[,22:23]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="A" & PlacQual$type_pe_Plac==listeType)
  XBOJ[,24]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="A" &  PlacQual$dtr==1)
  XBOJ[,25]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="A")*PlacQual$aam
  XBOJ[,26:28]<-1*(PlacQual$GrEspece=="BOJ" & PlacQual$GrDHP=="A" & PlacQual$vigu0==listeVigueur & PlacQual$prod0==listeProduits)


  XERR<-matrix(0,ncol=11,nrow=n)
  XERR[,1]<-1*(PlacQual$GrEspece %in% c("ERR","FIN") & PlacQual$GrDHP=="C")
  XERR[,2]<-1*(PlacQual$GrEspece %in% c("ERR","FIN") & PlacQual$GrDHP=="C" & PlacQual$ABCD=="C")
  XERR[,3]<-1*(PlacQual$GrEspece %in% c("ERR","FIN") & PlacQual$GrDHP=="C")*PlacQual$prec
  XERR[,4]<-1*(PlacQual$GrEspece %in% c("ERR","FIN") & PlacQual$GrDHP=="C" & PlacQual$prod0=="pate")

  XERR[,5:6]<-1*(PlacQual$GrEspece %in% c("ERR","FIN") & PlacQual$GrDHP %in% c("A","B") &  PlacQual$Intercept==listeInterceptB)
  XERR[,7:8]<-1*(PlacQual$GrEspece %in% c("ERR","FIN") & PlacQual$GrDHP %in% c("A","B") &  PlacQual$ABCD==listeQualB)
  XERR[,9:11]<-1*(PlacQual$GrEspece %in% c("ERR","FIN") & PlacQual$GrDHP %in% c("A","B") & PlacQual$vigu0==listeVigueur & PlacQual$prod0==listeProduits)

  XERS<-matrix(0,ncol=38,nrow=n)
  XERS[,1]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="C")
  XERS[,2]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="C" & PlacQual$ABCD=="C")
  XERS[,3:4]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="C" & PlacQual$type_pe_Plac==listeType)
  XERS[,5]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="C")*PlacQual$aam
  XERS[,6:8]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="C" & PlacQual$vigu0==listeVigueur & PlacQual$prod0==listeProduits)


  XERS[,9:10]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="B" &  PlacQual$Intercept==listeInterceptB)
  XERS[,11:12]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="B" &  PlacQual$ABCD==listeQualB)
  XERS[,13:14]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="B" & PlacQual$type_pe_Plac==listeType)
  XERS[,15:17]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="B" & PlacQual$vigu0==listeVigueur & PlacQual$prod0==listeProduits)
  XERS[,18]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="B" &  PlacQual$dtr==1)
  XERS[,19]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="B")*PlacQual$aam


  XERS[,20:22]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="A" &  PlacQual$Intercept==listeInterceptA)
  XERS[,23:25]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="A" &  PlacQual$ABCD==listeQualA)
  XERS[,26:27]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="A" & PlacQual$type_pe_Plac==listeType)
  XERS[,28]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="A" &  PlacQual$dtr==1)
  XERS[,29:35]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="A" &  PlacQual$rid1==listeRid1)
  XERS[,36:38]<-1*(PlacQual$GrEspece=="ERS" & PlacQual$GrDHP=="A" & PlacQual$vigu0==listeVigueur & PlacQual$prod0==listeProduits)

  XFEN<-matrix(0,ncol=8,nrow=n)
  XFEN[,1]<-1*(PlacQual$GrEspece=="FEN" & PlacQual$GrDHP=="C")
  XFEN[,2]<-1*(PlacQual$GrEspece=="FEN" & PlacQual$GrDHP=="C" & PlacQual$ABCD=="C")
  XFEN[,3]<-(PlacQual$GrEspece=="FEN" & PlacQual$GrDHP=="C")*PlacQual$dens_tot0

  XFEN[,4:5]<-1*(PlacQual$GrEspece=="FEN" & PlacQual$GrDHP %in% c("A","B") &  PlacQual$Intercept==listeInterceptB)
  XFEN[,6:7]<-1*(PlacQual$GrEspece=="FEN" & PlacQual$GrDHP %in% c("A","B") &  PlacQual$ABCD==listeQualB)
  XFEN[,8]<-1*(PlacQual$GrEspece=="FEN" & PlacQual$GrDHP %in% c("A","B")  &  PlacQual$dtr==1)

  XHEG<-matrix(0,ncol=31,nrow=n)
  XHEG[,1]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="C")
  XHEG[,2]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="C" & PlacQual$ABCD=="C")
  XHEG[,3:4]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="C" & PlacQual$type_pe_Plac==listeType)
  XHEG[,5]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="C")*PlacQual$aam
  XHEG[,6:8]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="C" & PlacQual$vigu0==listeVigueur & PlacQual$prod0==listeProduits)

  XHEG[,9:10]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="B" &  PlacQual$Intercept==listeInterceptB)
  XHEG[,11:12]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="B" &  PlacQual$ABCD==listeQualB)
  XHEG[,13:14]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="B" & PlacQual$type_pe_Plac==listeType)
  XHEG[,15]<-(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="B")*PlacQual$prec
  XHEG[,16:18]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="B" & PlacQual$vigu0==listeVigueur & PlacQual$prod0==listeProduits)
  XHEG[,19]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="B")*PlacQual$aam

  XHEG[,20:22]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="A" &  PlacQual$Intercept==listeInterceptA)
  XHEG[,23:25]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="A" &  PlacQual$ABCD==listeQualA)
  XHEG[,26:27]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="A" & PlacQual$type_pe_Plac==listeType)
  XHEG[,28]<-(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="A")*PlacQual$prec
  XHEG[,29:31]<-1*(PlacQual$GrEspece=="HEG" & PlacQual$GrDHP=="A" & PlacQual$vigu0==listeVigueur & PlacQual$prod0==listeProduits)

  XTOT<-cbind(XBOJ,XERR,XERS,XFEN,XHEG)


  # Matrice des parametres
  BetaMat<-as.matrix(Para.EvolQualTot)

    # Calcul de la prevision
  pred <- (XTOT %*% BetaMat)

  PlacQual$xb_qual <- pred[,1]
  PlacQual$predb <- ifelse(PlacQual$GrDHP=="C", exp(PlacQual$xb_qual)/(1+exp(PlacQual$xb_qual)), 1/(1+exp(-PlacQual$xb_qual)))
  PlacQualb <- PlacQual %>%
            select(Placette, ArbreID, GrEspece, GrDHP, Intercept, predb) %>%
            pivot_wider(id_cols=c("Placette","ArbreID","GrEspece","GrDHP"), values_from=predb, names_from=Intercept, # j'ai enlevé "ABCD","vigu0","prod0","dtr"
                        names_prefix="Intercept")


 if("A" %in% PlacQualb$GrDHP){
   PlacQualb <- PlacQualb %>%
             mutate(Alea = runif(n=nrow(PlacQualb))) %>%
             mutate(ABCD1 = ifelse(Alea<=Intercept1, "D",
                                    ifelse(GrDHP=="C", "C",
                                           ifelse(Alea <= Intercept2, "C",
                                                  ifelse(GrDHP=="B", "B",
                                                          ifelse(Alea<=Intercept3, "B", "A")))))) %>%
    #select(Placette, ArbreID, Intercept1, Intercept2, Intercept3, ABCD1)
     select(-Alea, -GrDHP, -GrEspece)
 }else{

   if("B" %in% PlacQualb$GrDHP){
     PlacQualb <- PlacQualb %>%
                mutate(Alea = runif(n=nrow(PlacQualb))) %>%
                mutate(ABCD1 = ifelse(Alea<=Intercept1, "D",
                           ifelse(GrDHP=="C", "C",
                                  ifelse(Alea <= Intercept2, "C", "B")))) %>%
       #select(Placette, ArbreID, Intercept1, Intercept2, Intercept3, ABCD1)
       select(-Alea, -GrDHP, -GrEspece)

   }else{
     PlacQualb <- PlacQualb %>%
       mutate(Alea = runif(n=nrow(PlacQualb))) %>%
       mutate(ABCD1 = ifelse(Alea<=Intercept1, "D", "C")) %>%
       #select(Placette, ArbreID, Intercept1, Intercept2, Intercept3, ABCD1)
       select(-Alea, -GrDHP, -GrEspece)
    }

 }

  # remettre les arbres avec le fichier complet
  setDT(PlacQualb)
  PlacQual_fin <- merge(PlacQual1, PlacQualb, by = c("Placette", "ArbreID"), all.x = TRUE)
  setorder(PlacQual_fin, Placette, ArbreID)
  PlacQual_fin[, c("ABCD") := NULL]
  #setnames(PlacQual_fin, old = c("ABCD"), new=("ABCD_orig"))
  setnames(PlacQual_fin, old = c("ABCD1"), new=("ABCD"))


  } else{ # si pas d'arbres dont la qualité est à faire evoluer, on retourne le fichier d'intrant

    PlacQual_fin = PlacQual1

  }


  return(PlacQual_fin)

}
