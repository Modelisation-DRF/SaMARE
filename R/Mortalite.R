#' Fonction qui prévoit la probabilité de mortalité de chacun des arbres
#' durant un pas de simulation.
#'
#' @param Mort Un dataframe qui contient la liste des arbres avec au minimum les colonnes Placette, GrEspece, DHPcm,
#'             vigu0, prod0, trt, temp, type_pe_Plac, fact_red
#' @param t La longueur du pas de simulation en annee (en annees).
#' @param Para.mort Un dataframe  contenant les paramettres du module de mortalité.
#' @param RandomMort Un dataframe contenant les effets aléatoires du module de mortalité
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#' @inheritParams SimulSaMARE
#'
#' @return Retourne Mort avec les colonnes prob_mort , xb_mort et Etat1
#'
#' @import data.table
#' @export

mort<-function(Mort, t, MCH , Para.mort, RandomMort, seed_value=NULL){
  # Mort, t, MCH , Para.mort, RandomMort, seed_value=10


  if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

  # Mort: fichier des arbres
  # t
  # Iterj=Iteration
  # MCH
  # Para.mort
  # effet fixe à l'échelle de la placette: trt , temp, type_pe_Plac , fact_red, t


  select=dplyr::select

  # compter les HEG dans les placettes
  Mort_temp <- Mort %>%
    group_by(Placette) %>%
    mutate(nHEG = sum(Nombre*(GrEspece=="HEG")))

  # placettes avec du HEG:  # traiter les placettes sans HEG à part si MCH=1
  Mort_temp_heg <- Mort_temp %>% filter(nHEG>0)

  #nHEG <- nrow(Mort[which(Mort$GrEspece=="HEG"),])

  #if (MCH==0|nrow(Mort_temp_heg)==0){ # si on ne simule pas la maladie cortical ou s'il n'y a pas de heg, modèle de mortalité régulier

  # on va calculer la mortalité avec le modèle régulier pour tous les arbres, et ensuite, si MCH=1, on va écraser la mortalité des HEG par celle obtenue avec le modèle pour le heg
    n <- nrow(Mort_temp)

    #Liste des effets:
    listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
                rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))  # 10 ess x nb_lignes
    listeVigu0<-c(rep("NONVIG",n),rep("ViG",n)) # 2 vigueurs x nb_lignes
    listeProd0<-c(rep("pate",n),rep("resineux",n),rep("sciage",n))
    listeNtrt<-c(rep(2,n),rep(1,n),rep(0,n))
    listeTypePe<-c(rep("type0",n),rep("type1",n),rep("type2",n))
    listeEssInter2<-c(rep("AUT",n*2),rep("BOJ",n*2),rep("EPX",n*2),rep("ERR",n*2),
                      rep("ERS",n*2),rep("FEN",n*2),rep("FIN",n*2),rep("HEG",n*2),
                      rep("RES",n*2),rep("SAB",n*2))
    listeEssInter3<-c(rep("AUT",n*3),rep("BOJ",n*3),rep("EPX",n*3),rep("ERR",n*3),
                      rep("ERS",n*3),rep("FEN",n*3),rep("FIN",n*3),rep("HEG",n*3),
                      rep("RES",n*3),rep("SAB",n*3))

    # Construction de la matrice X
    # matrice X: une ligne par arbre et 75 colonnes
    # 75 colonnes: 75 effets
    Xmort<-matrix(0,ncol=75,nrow=n)
    Xmort[,1]<-1                                                                # 1 intercept
    Xmort[,2:3]<-(Mort$vigu0==listeVigu0)*1                                     # 2 vigueurs, une colonne chacune
    Xmort[,4:13]<-(Mort$GrEspece==listeEss)*1                                   # 10 essences, une colonne chacune
    Xmort[,14:33]<-(Mort$GrEspece==listeEssInter2 & Mort$vigu0==listeVigu0)*1   # 20 essence x  vigueurs
    Xmort[,34]<-Mort$DHPcm                                                      # 1 dhp
    Xmort[,35]<-Mort$DHPcm*Mort$DHPcm                                           # 1 dhp^2
    Xmort[,36:38]<-(Mort$prod0==listeProd0)*1                                   # 3 produits
    Xmort[,39:68]<-(Mort$GrEspece==listeEssInter3 & Mort$prod0==listeProd0)*1   # 30 essence x produits
    Xmort[,69]<-(Mort$trt=="CP")*1                                              # 1 CP
    Xmort[,70]<-Mort$temp                                                       # 1 temp
    Xmort[,71:73]<-(Mort$type_pe_Plac==listeTypePe)*1                           # 3 type de placettes
    Xmort[,74]<-Mort$fact_red                                                   # 1 fact_red
    Xmort[,75]<-log(t)                                                          # 1 t
                                                                                # 75 effets au total

    # selectionner les parametres de mortalité de l'itération
    # une ligne par paramètre = 75 lignes
    #ParaMorti<-Para.mort
    #%>%
    #filter(Iter==Iterj) # mais il n'y a que cette itération dans le fichier

    # Création matrice Beta: 1 colonne de 75 lignes, dans le même ordre que les effet ci-dessus
    BetaMat<-matrix(Para.mort$ParameterEstimate,ncol=1)

    # Calcul de la probabilité de mortalité
    cloglog <- (Xmort %*% BetaMat)

    #return(cloglog)

  #}else{ # si MCH=1 et qu'il y a des HEG dans le fichier

    # s'il MHC=1 et qu'il y a des HEG dans le fichier
    if (MCH==1 & nrow(Mort_temp_heg)>0) {

      # indice des lignes où il y a du HEG
      indices <- Mort_temp[,'GrEspece'] == 'HEG'
      cloglog[indices, 1] <- as.matrix(-5.4430 + 0.0621*Mort_temp[indices,'DHPcm'] + log(t))

    }

    # ajout des effets aléatoires de placettes et calcul de l'etat mort/vivant
    # si MCH=1, on va ajouter les effets aléatoires de placette et de step qui viennent du modèle de mortalité de base
    # aux prévisions de mortalité obtenues avec la mortalité du HEG par MCH,
    Mort_temp$xb_mort <- as.numeric(cloglog)

    setDT(Mort_temp)
    setDT(RandomMort)

    # Équivalent de `left_join`
    Mort_temp <- RandomMort[, .(Placette, RandomPlac, RandomStep)][Mort_temp, on = .(Placette)]
    Mort_temp[, `:=`(
      prob_mort = 1 - exp(-exp(xb_mort + RandomPlac + RandomStep)),
      Alea = runif(.N))][,
                         `:=`(Etat1 = fifelse(Alea <= prob_mort, "mort", Etat))
                         ]
    Mort_temp[, c("Alea", "RandomPlac", "RandomStep", "nHEG") := NULL]
    return(Mort_temp)

}
