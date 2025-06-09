#' Fonction qui effectue la simulation de l'évolution des arbres de plusieurs placettes
#' avec le simulateur SaMARE pour une itération. Cette fonction effectue la simulation et
#' appelle chacune des fonction permettant de prévoir la mortalité, l'accroissement,
#' le recrutement et l'évolution des paramètres de simulation.
#'
#' @param Random Un dataframe contenant des effets aléatoires à l'échelle de
#'               la placette et de la période de simulation pour les modules
#'               de SaMARE qui n'utilisent pas les gaules.
#' @param RandomGaules  Un dataframe contenant des effets aléatoires à l'échelle de
#'                      la placette et de la période de simulation pour les modules
#'                      de SaMARE qui utilisent les gaules.
#' @param Data    Un dataframe contenant une liste des arbres de dimension marchande
#'                des placettes à simuler ainsi que des variables nécessaires à
#'                la simulation.
#' @param Gaules  Un dataframe contenant le nombre de gaules par classe de diamètre
#'                et par groupe d'essence d'une placette à
#'                simuler ainsi que des variables nécessaires à la simulation.
#' @param Iteration le numéro de l'itération
#' @param CovParms Un dataframe contenant la variance des effets aléatoires des
#'                  équations des modules de base de SaMARE (modules 1 à 9 et 17 à 19).
#' @param CovParmsGaules Un dataframe contenant la variance des effets aléatoires des
#'                        équations des modules de SaMARE basés sur l'information
#'                        provenant des gaules (modules 10 à 16).
#' @param Para  Un dataframe contenant les paramètres des modules de 1 à 9 et 17 à 19
#'               (modules de base) de SaMARE.
#' @param ParaGaules Un dataframe contenant les paramètres des modules 17 à 19
#'               (modules basés sur les gaules) de SaMARE.
#' @param Omega Un dataframe contenant pour chaque module de base de SaMARE
#'              (modules 1 à 9 17 à 19) le éléments du triangle inférieur de
#'               la matrice de variance-covariance.
#' @param OmegaGaules Un dataframe contenant pour chaque module basé sur les gaules
#'                    de SaMARE (modules 10 à 16) les éléments du triangle inférieur de
#'                    la matrice de variance-covariance.
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#' @inheritParams SimulSaMARE
#'
#' @return Retourne un dataframe avec une liste d'arbres vivants et mort avec leur DHP
#'         pour chaque étape de 5 ans de l'horizon de simulation.
#' @import data.table
#' @export

SaMARE<- function(Random, RandomGaules, Data, Gaules, Iteration, Horizon, RecruesGaules,
                  MCH, CovParms, CovParmsGaules, Para, ParaGaules, Omega, OmegaGaules,
                  seed_value=NULL){

  # Random =RandomTest; Data = data; Gaules =NA
  # Horizon = 6 ; Iteration = 1;seed_value=3;
  # RecruesGaules =0;CovParms=MatchModuleCovparms;CovParmsGaules=CovparmGaules;
  # Para=MatchModuleParameters;ParaGaules=ParametresGaules;Omega=MatchModuleOmega; OmegaGaules=OmegaGaulesFormat; MCH = 0
  # set.seed(NULL)
  # set.seed(3)

  if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

  select=dplyr::select

  # Maintenant en fichiers interne dans sysdata
  # t<-5
  # Liste d'Especes
  # Especes <- c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB")


  # Création du fichier de départ original
  setDT(Data)
  # PlacOri <- Data %>%
  #   mutate(Annee = AnneeDep, Iter=Iteration)
  PlacOri <- Data[, `:=`(
    Annee = Annee_Inventaire,
    Iter=Iteration
  )]


  # Paramètres des effets fixes
  ParaTot <- Para

  # Paramètres des effets fixes du module de gaules
  ParaTotGaules <- ParaGaules

  #### 1. Génération aléatoire des parametres de chacun des modules ####

  # Paramètres des effets fixes
  # la fonction ParaOmega génère une serie de paramètres aléatoires en fonction de la matrice de covar et du vecteur de paramètres
  # ces paramètres des effets fixes seront utilisés pour toutes les placettes et tous les pas de simulation, pour cette itération
  # la fonction ParaOmega a été construite pour générer les paramètres pour plusieurs itérations, mais ici on en génère que pour une seule, donc ici ParaOri = ParaIter
  Para.mort <- ParaOmega(ModuleID = 1, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])
  Para.acc <- ParaOmega(ModuleID = 2, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])
  Para.vig <- ParaOmega(ModuleID = 3, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])
  Para.prod <- ParaOmega(ModuleID = 4, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_n <- ParaOmega(ModuleID = 5, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_dhp <- ParaOmega(ModuleID = 6, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_vig <- ParaOmega(ModuleID = 7, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_prod <- ParaOmega(ModuleID = 8, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])
  Para.ConvMSCRVig <- ParaOmega(ModuleID = 17, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])
  Para.ConvMSCRProd1024 <- ParaOmega(ModuleID = 18, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])
  Para.ConvMSCRProd24 <- ParaOmega(ModuleID = 19, ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) #%>% mutate(Iter=PlacOri$Iter[1])

  # Effets aleatoires de la placette de l'iteration en cours
  #RandPlac<-Random %>% filter(Placette==PlacOri$Placette[1] & Iter==PlacOri$Iter[1])
  RandPlac <- Random %>% filter(Iter==Iteration)


  #### 2. Préparation pour les modules d'évolution des gaules ####
  if (RecruesGaules==1){

  # Génération aléatoire des parametres des effets fixes des modules de gaules
  Para.rec_gaules<-ParaOmega(ModuleID = 10,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.nb_gaules<-ParaOmega(ModuleID = 11,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.ratio_gaules<-ParaOmega(ModuleID = 12,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_ERS<-ParaOmega(ModuleID = 13,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_HEG<-ParaOmega(ModuleID = 14,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_BOJ<-ParaOmega(ModuleID = 15,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_SAB<-ParaOmega(ModuleID = 16,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])

  # Création de la placette de simulation des gaules
  PlacGaules <- Gaules %>%
      #filter(Placette==ListeIter$Placette) %>%
      mutate(Iter=Iteration)

  # compiler les gaules
  Nb_Gaules_Ha_tr1 <- PlacGaules %>% lazy_dt() %>%
    mutate(NbHa = Nombre/Sup_PE) %>% group_by(Placette) %>%
    mutate(Nb_Gaules_Ha = sum(NbHa)) %>%  group_by(Placette, Nb_Gaules_Ha, GrEspece) %>%
    summarise(Nb_Gaules_Ess_Ha = sum(NbHa)) %>%
    filter(GrEspece %in% c('ERS','HEG','BOJ','SAB')) %>%
    select(Placette, Nb_Gaules_Ha, GrEspece, Nb_Gaules_Ess_Ha) %>%
    group_by(Placette, Nb_Gaules_Ha) %>%
    pivot_wider(names_from = GrEspece, names_prefix = 'Nb_Gaules_', values_from = Nb_Gaules_Ess_Ha) %>% as.data.frame()

  Nb_Gaules_Ha_tr2 <- PlacGaules %>% lazy_dt() %>%
    mutate(NbHa68 = ifelse(DHPcm>5, Nombre/Sup_PE, 0)) %>% group_by(Placette, GrEspece) %>%
    summarise(Nb_Gaules_68_Ess_Ha = sum(NbHa68)) %>%
    filter(GrEspece %in% c('ERS','HEG','BOJ','SAB')) %>% select(Placette, GrEspece, Nb_Gaules_68_Ess_Ha) %>%
    group_by(Placette) %>%
    pivot_wider(names_from = GrEspece, names_prefix = 'Nb_Gaules_68_', values_from = Nb_Gaules_68_Ess_Ha) %>%  as.data.frame()

  gaule_init <- left_join(Nb_Gaules_Ha_tr1, Nb_Gaules_Ha_tr2, by='Placette')
  nom_gaules <- names(gaule_init)
  nom_attendu <- c("Nb_Gaules_BOJ", "Nb_Gaules_ERS", "Nb_Gaules_HEG", "Nb_Gaules_SAB",
                   "Nb_Gaules_68_BOJ", "Nb_Gaules_68_ERS", "Nb_Gaules_68_HEG", "Nb_Gaules_68_SAB")
  for (nom in nom_attendu){
    if (!nom %in% nom_gaules)  gaule_init[[nom]] <- 0
  }

  # Générer les effets aleatoires placette Gaules
  RandomPlacGaules <- RandomGaules %>% filter(Iter==Iteration)

  }


  #### 3. Création de la placette de simulation ####

  Plac <- PlacOri %>%
    filter(Etat %in% c(10,11,12,40,42,30,32,50,52,70,71,72)) %>%
    mutate(Etat = ifelse(Etat %in% c(11,71,72), "martele", "vivant"),
           ArbreID=seq(1:n()))


  #### 4. Génération des paramètres aléatoires du module d'evolution de la qualite ####

  PlacQual<-Plac %>%
    filter(ABCD %in% c("A","B","C","D") & Etat=="vivant")

  ABCD_pres <- NULL
  if (nrow(PlacQual)>=1){

    # indiquer quelles placettes a la qualité fournie
    ABCD_pres <- Plac %>%
      group_by(Placette) %>%
      summarise(ABCD_presence = ifelse(sum(ABCD %in% c("A","B","C","D") & Etat=="vivant")>0, 'oui', 'non'))


    ParaBOJ<-ParametresEvolQual[which(ParametresEvolQual$Ess_groupe=="BOJ"),]
    ParaERR<-ParametresEvolQual[which(ParametresEvolQual$Ess_groupe=="ERR"),]
    ParaERS<-ParametresEvolQual[which(ParametresEvolQual$Ess_groupe=="ERS"),]
    ParaFEN<-ParametresEvolQual[which(ParametresEvolQual$Ess_groupe=="FEN"),]
    ParaHEG<-ParametresEvolQual[which(ParametresEvolQual$Ess_groupe=="HEG"),]

    OmegaBOJ<-OmegaEvolQual[which(OmegaEvolQual$Ess_groupe=="BOJ"),]
    OmegaERR<-OmegaEvolQual[which(OmegaEvolQual$Ess_groupe=="ERR"),]
    OmegaERS<-OmegaEvolQual[which(OmegaEvolQual$Ess_groupe=="ERS"),]
    OmegaFEN<-OmegaEvolQual[which(OmegaEvolQual$Ess_groupe=="FEN"),]
    OmegaHEG<-OmegaEvolQual[which(OmegaEvolQual$Ess_groupe=="HEG"),]


    Para.EvolQualBOJ1<-ParaOmega(ModuleID = 1,ParaOri=ParaBOJ, ParaIter=ParaBOJ,Omega=OmegaBOJ,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualBOJ2<-ParaOmega(ModuleID = 2,ParaOri=ParaBOJ, ParaIter=ParaBOJ,Omega=OmegaBOJ,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualBOJ3<-ParaOmega(ModuleID = 3,ParaOri=ParaBOJ, ParaIter=ParaBOJ,Omega=OmegaBOJ,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualERR1<-ParaOmega(ModuleID = 1,ParaOri=ParaERR, ParaIter=ParaERR,Omega=OmegaERR,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualERR2<-ParaOmega(ModuleID = 2,ParaOri=ParaERR, ParaIter=ParaERR,Omega=OmegaERR,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualERS1<-ParaOmega(ModuleID = 1,ParaOri=ParaERS, ParaIter=ParaERS,Omega=OmegaERS,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualERS2<-ParaOmega(ModuleID = 2,ParaOri=ParaERS, ParaIter=ParaERS,Omega=OmegaERS,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualERS3<-ParaOmega(ModuleID = 3,ParaOri=ParaERS, ParaIter=ParaERS,Omega=OmegaERS,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualFEN1<-ParaOmega(ModuleID = 1,ParaOri=ParaFEN, ParaIter=ParaFEN,Omega=OmegaFEN,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualFEN2<-ParaOmega(ModuleID = 2,ParaOri=ParaFEN, ParaIter=ParaFEN,Omega=OmegaFEN,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualHEG1<-ParaOmega(ModuleID = 1,ParaOri=ParaHEG, ParaIter=ParaHEG,Omega=OmegaHEG,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualHEG2<-ParaOmega(ModuleID = 2,ParaOri=ParaHEG, ParaIter=ParaHEG,Omega=OmegaHEG,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualHEG3<-ParaOmega(ModuleID = 3,ParaOri=ParaHEG, ParaIter=ParaHEG,Omega=OmegaHEG,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])

    Para.EvolQual<-list(Para.EvolQualBOJ1,Para.EvolQualBOJ2,Para.EvolQualBOJ3,Para.EvolQualERR1,Para.EvolQualERR2,Para.EvolQualERS1,
                        Para.EvolQualERS2,Para.EvolQualERS3,Para.EvolQualFEN1,Para.EvolQualFEN2,Para.EvolQualHEG1,Para.EvolQualHEG2,
                        Para.EvolQualHEG3)
    Para.EvolQualTot<-c()

    for(i in 1:13){

      Parai<-Para.EvolQual[[i]][,3]
      Parai<-Parai[which(Parai$ParameterEstimate!=0),]
      Para.EvolQualTot<- rbind(Para.EvolQualTot,Parai)

    }

     rm(ParaBOJ,ParaERR,ParaERS,ParaFEN,ParaHEG,OmegaBOJ,OmegaERR,OmegaERS,OmegaFEN,OmegaHEG,Para.EvolQualBOJ1,Para.EvolQualBOJ2,
       Para.EvolQualBOJ3,Para.EvolQualERR1,Para.EvolQualERR2,Para.EvolQualERS1,Para.EvolQualERS2,Para.EvolQualERS3,Para.EvolQualFEN1,
       Para.EvolQualFEN2,Para.EvolQualHEG1,Para.EvolQualHEG2,Para.EvolQualHEG3,Para.EvolQual)

  }
  rm(PlacQual)



  #### 5. Préparation des variables explicatives à l'échelle de la placette fixe dans le temps ####
  # NOTE: quand on ajoutera le simulateur de coupe, trt et ntrt ne seront plus fixe dans le temps
  Plac <- Plac %>%
    group_by(Placette) %>%
    mutate(t0 = Annee_Coupe,
           trt = ifelse(sum(Etat == "martele")>0, "CP", # s'il y a au moins 1 arbre de martelé dans la placette c'est une CP
                        ifelse((is.na(ntrt)==TRUE | ntrt==0),"TEM", # si ntrt est vide, c'est un témoin
                               ifelse(Annee_Inventaire-t0<5 & (sum((DHPcm/200)^2*3.1416*Nombre)/Sup_PE)>26, "TEM", # Si ST>26 et que la coupe date de moins de 5 ans, c'est un témoin
                                      "CP"))), # sinon c'est une CP
           # ntrt = ifelse(trt=="CP" & sum(Etat == "martele")>0, ntrt+1,
           #               ifelse(trt=="CP", ntrt,
           #                      0)),  # cette variable ne devrait être upgradée ici, mais plutôt après la création de la placette initiale, pour le cas où il y a des arbres martelé et car quand on va intégrer le simulateur de coupe, cette variable devra être modifiée à l'intérieur de la boucle sur les horizons
           type_pe_Plac = ifelse(Sup_PE==0.04,"type0", # effets réels
                                 ifelse(Sup_PE>=0.25 & Sup_PE<=0.5,"type1",  # dispos de jardinage 2023 (0.25), 2026 (0.25), Arcale (0.25), Windsor (0.25), CPI (0.5), Dubuc (0.5)
                                        "type2")), # Mitchi (1 ha), Lac jaune (0.1), Rouge (0.16)
           pente = ifelse(is.na(Pente==TRUE),6.5,
                          Pente),
           dom = substr(Reg_Eco,1,1),
           dom = ifelse(dom %in% c("2","3","4"), dom, "4"), ## valeur de 4 attribué aux domaines pas dans la liste d'effets

           rid1 = ifelse(Reg_Eco %in% c("1a","2a","2b","2c"), "2o",
                         ifelse(Reg_Eco %in% c("4a","4b","4c"), "4o",
                                ifelse(Reg_Eco %in% c("4d","4e","4f","4g","4h"), "4e",
                                       Reg_Eco))),
           rid1 = ifelse(rid1 %in% c("2o","3a","3b","3c","3d","4e","4o","DU","SV"), rid1, "4o"),  # Ajouter DU et SV qui manquaient et mis "4o" quand manquant

           vegp = substr(Type_Eco,1,3)
           ) %>%
    rename(latitude = Latitude,
           longitude = Longitude,
           altitude = Altitude,
           reg = Reg_Eco,
           teco = Type_Eco,
           prec = Ptot,
           temp = Tmoy,
           grwd = GrwDays) %>%
    select(Placette, Annee_Inventaire, Annee, trt, ntrt, t0, pente, Sup_PE, ArbreID, NoArbre, GrEspece, Espece,
           Etat, DHPcm, Nombre, Vigueur, MSCR, ABCD,
           Iter, type_pe_Plac, dom, rid1, vegp, latitude, longitude, altitude, reg, teco, prec, temp, grwd)



  #### 6. Simulation de la placette sur le nombre de steps ####

  # Initialisation du fichier qui contiendra les résultats de simulation de la placette
  outputTot<-c()

  # liste des variables à l'Échelle de la placettes qui entrent dans les équations
  var_plot <- c("Annee_Inventaire", "trt", "ntrt", "t0", "pente", "Sup_PE", "type_pe_Plac", "dom", "rid1",
                "vegp", "latitude", "longitude", "altitude", "reg", "teco",
                "prec", "temp", "grwd", "t0_aj_", "fact_red", "st_tot0", "dens_tot0", "mch")

  for (k in 1:Horizon){
    # k=1

    ##### 6.1 Pour k=1: on utilise le fichier de depart de la placette #####
    if   (k==1){

      Plac <- Plac %>%
        mutate(Annee = Annee_Inventaire+t) # on upgrade l'année

      ###### 6.1.1 Création des vigueur et de produit  ######

      Plac <- AttribVigu0(Plac, Para.ConvMSCRVig)
      Plac <- AttribProd0(Plac, Para.ConvMSCRProd1024, Para.ConvMSCRProd24)
      Plac <- Plac %>% select(-Vigueur)


      ###### 6.1.2 Dataframe avec les conditions initiales de la placette ######

      # On remet l'année à l'année de départ et residuel, ce fichier contient des arbres martelés
      outputInitial <- Plac %>%
        mutate(Annee=Annee_Inventaire, Residuel=0) %>%
        select(Placette, Annee_Inventaire, Annee, ArbreID, NoArbre, Nombre, GrEspece,
               Espece, Etat, DHPcm, vigu0, prod0, ABCD, MSCR, Residuel, Iter, t0, trt, ntrt) # je vais laisser ntrt ici pour pourvoir faire les validations de samare, mais dans le fichier final retourné à l'utilisateur, on mettre ntrt du fichier d'intrant


      ###### 6.1.3 Modification placette initiale pour martelage ######

      # s'il y a des arbres martelés dans une placette, on garde tous les arbres (vivant+martele) avec residuel=0,
      # mais on fait une copie des arbres vivant avec residuel=1
      # s'il y a des arbres martelés, les enlever, ne garder que les vivants et mettre Residuel=1
      outputInitial_mart <- Plac %>%
        group_by(Placette) %>%
        mutate(martele = ifelse(sum(Etat == "martele")>0, T, F)) %>%
        filter(Etat=="vivant", martele==T) %>%
        mutate(Annee=Annee_Inventaire,Residuel=1,
               ntrt = ntrt+1, t0=Annee_Inventaire) %>%  # ajout pour que dans le fichier des résultats, ces variables réflètent mieux la réalité, mais ne sont pas utilisées dans simul
        select(Placette, Annee_Inventaire, Annee, ArbreID, NoArbre, Nombre, GrEspece,
               Espece, Etat, DHPcm, vigu0, prod0, ABCD, MSCR, Residuel, Iter, t0, trt, ntrt)

      outputInitial <- rbind(outputInitial,outputInitial_mart)

      # dans les placettes à simuler, ne garder que les vivants (on enlève les martelés)
      # s'il y a des arbres martelé dans la placette, on met t0 (anne_coupe) à AnneeDep
      Plac <- Plac %>%
        group_by(Placette) %>%
        mutate(t0 = ifelse(sum(Etat == "martele")>0, Annee_Inventaire, t0),

               ntrt = ifelse(trt=="CP" & sum(Etat == "martele")>0, ntrt+1,  # j'ai déplacé ça ici, après la placette initiale, mais il faudra aussi le faire à l'intérieur de la boucle quand simulateur de coupe sera là
                             ifelse(trt=="CP", ntrt,
                                    0))) %>%
        filter(Etat=="vivant")


      ###### 6.1.4 Génération des résidus de l'arbre pour tous les pas de simulation ######

      # CovParms contient la variances des effets aléatoires et la variance de l'erreur résiduelle (et sa structure) de chaque module
      # aller chercher la variance de l'erreur résiduelle (présente dans un seul module (le 2))
      Residual <- CovParms$ParameterEstimate[which(CovParms$CovParm=="Residual")]

      # aller chercher les parametres de la strucure de l'erreur résiduelle du module 2 (ARMA(1,1))
      Rho <- CovParms$ParameterEstimate[which(CovParms$CovParm=="RHO")]
      Gamma <- CovParms$ParameterEstimate[which(CovParms$CovParm=="Gamma")]

      # fonction pour générer un element de la matrice de var-cov ARMA(1,1)
      f <- function(i, j, var_res, gamma, rho) { (((i-j)!=0)*(var_res * gamma*rho^(abs(j-i)-1)))+( ((i-j)==0)*var_res) } # correlation arma
      # créer et remplir la matrice de var-cov de l'erreur résiduelle de l'arbre pour ces pas de simulation
      varcov <- expand.grid(i=1:Horizon, j=1:Horizon)
      varcov <- matrix(f(varcov$i, varcov$j, Residual, Gamma, Rho), nrow=Horizon)

      # générer un vecteur d'erreur residuelle de la longueur du nombre de pas de simulation, pour chaque arbre de chaque placette
      Residus <- rockchalk::mvrnorm(n=nrow(Plac), mu=rep(0,Horizon), Sigma = varcov, empirical=F)
      Residus <- cbind(Plac$Placette, Plac$ArbreID, Residus)
      colnames(Residus)<-c("Placette","ArbreID",paste("Periode","_",c(1:Horizon),sep=""))



      ###### 6.1.5 Preparation des variables explicatives des gaules pour simulation ######
      if (RecruesGaules==1){
        #RecGaules<-data.frame("GrEspece"=c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB")) # on ne s'en sert pas ici et il est recréer plus loin

        suppressMessages(
          Nb_Gaules_Ha <- PlacGaules %>%
            mutate(NbHa = Nombre/Sup_PE,
                   NbHa68 = ifelse(DHPcm>5, Nombre/Sup_PE, 0)) %>%
            group_by(Placette, GrEspece) %>%
            summarise(Nb_Gaules_Ess_Ha = sum(NbHa),
                      lnNb_Gaules_Ess_Ha = log(sum(NbHa)+1),
                      lnNb_Gaules_24_Ess_Ha = log(sum(NbHa)-sum(NbHa68)+1),
                      lnNb_Gaules_68_Ess_Ha = log(sum(NbHa68)+1)))

        ### Vérifier ce que je fais avec ça, rendu ici, le data outputTot est NULL, ça crée donc une variable dans ce data, mais ça ne marche pas dans la version de Junior
        # je vais ignorer ça pour l'instant.
        #outputTot$Nb_Gaules_Ha <- sum(Nb_Gaules_Ha$Nb_Gaules_Ess_Ha)
        #outputTot$Nb_Gaules_68_Ha <- sum(exp(Nb_Gaules_Ha$lnNb_Gaules_68_Ess_Ha)-1)


      }

    ##### 6.2 2e pas de simulation ou plus #####
    }
    else {
      #k=2
      # On utilise le dernier pas de simulation comme point de départ
      Plac <- outputTot %>%
        filter(Annee == Annee_Inventaire+(k-1)*t & Etat!="mort") %>%
        mutate(Annee=Annee_Inventaire+k*t, Etat="vivant") # on met les recrues à vivant pour le prochain step

    }

    ##### 6.3 Calcul des variables à l'échelle de la placette qui evolue dans le temps #####

    Plac <- Plac %>%
      group_by(Placette) %>%
      mutate(


        t0_aj_ = ifelse(trt=="CP" & (Annee-t0<=25), Annee-t0-4.9, 0), ###### on ajuste pour que lorsque la step débute immédiatement après coupe t0_aj a une valeur de 0.1. Et maximum de 20 ans pour l'effet de coupe

        fact_red = ifelse(trt=="TEM", 0,
                          ifelse(t0_aj_ <= 3, 1,
                                 0)),    #Réduction de la mortalité

        # calcul variable echelle placette
        st_tot0 = sum((DHPcm/200)^2 *3.1416*Nombre)/Sup_PE,  # calcul variable echelle placette
        dens_tot0 = sum(Nombre)/Sup_PE,
        mch=MCH # on va créer mch ici, comme ça on aura la possibilité de la faire varier à chaque pas de simulation et de la voir dans le fichier de résultats
        )


    ##### 6.4 Effet aléatoire de step #####
    RandPlacetteStep <- RandPlac %>% filter(Step==k)


    ##### 6.5 Mortalite #####

    # Effets aléatoires pour la mortalité
    RandomMort <- RandPlacetteStep %>% filter(SubModuleID==1)

    Mort <- Plac

    # Application de la fonction de mortalité
    Plac <- mort(Mort, t, MCH , Para.mort, RandomMort, seed_value=NULL)
    Plac <- Plac %>% select(-xb_mort, -prob_mort)


    ##### 6.6 Accroissement en diamètre #####

    # Effets aléatoire pour l'accroissement
    RandomAcc <- RandPlacetteStep %>% filter(SubModuleID==2)

    Accrois <- Plac
    # on applique la fonction d'accroissement
    Plac <- accrois(Accrois, t, Para.acc, RandomAcc, Residus[,k+2])
    Plac <- Plac %>% select(-xb_acc, -pred_acc)


    ##### 6.7 Vigueur #####

    Vig <- Plac

    RandomVig <- RandPlacetteStep %>% filter(SubModuleID==3)

    # Application de la fonction de vigueur
    # ça fonctionne si on laisse les morts dans le fichier, ça met NA
    Plac <- vig(Vig, Para.vig, RandomVig, seed_value=NULL)
    Plac <- Plac %>% select(-xb_vig, -prob_vig)


    ##### 6.8 Produit #####

    # on ne calcule pas de probabilité de produit sur le type resineux des arbres survivants
    #indice_prod0 <- which(Plac[,"prod0"]!="resineux" & Plac[,"Etat1"] !="mort")

    RandomProd <- RandPlacetteStep %>% filter(SubModuleID==4)

    #Prod <- Plac %>% filter(prod0!="resineux" & Etat1 !="mort")
    Prod <- Plac

    # Application de la fonction de produit
    Plac <- produit(Prod, Para.prod, RandomProd, seed_value=NULL)
    Plac <- Plac %>% select(-xb_prod, -prob_prod)


    # S'il y a au moins une placette avec qualité
    if (!is.null(ABCD_pres)) {

      ##### 6.9 Evolution Qualite des survivants avec qualité #####

      # sélectionner seulement les placettes avec qualité en intrant
      EvolQual <- left_join(ABCD_pres[ABCD_pres$ABCD_presence=='oui',], Plac, by='Placette')
      setDT(EvolQual)
      EvolQual <- EvolQual(EvolQual, Para.EvolQualTot, seed_value=NULL)


     ##### 6.10 Attribution Qualite pour les arbres qui viennent de passer le seuil de 23 cm quand la qualité est founie en intrant #####

      EvolQual <- AttribQualFct(EvolQual, seed_value=NULL)

     # Remettre toutes les placettes
     Plac <- left_join(Plac[,-c('ABCD')], EvolQual[,c("Placette","ArbreID","ABCD")], by=c("Placette","ArbreID"))

   }



   ##### 6.10 Recrutement: preparation du fichiers #####


    # Préparation du fichier de recrutement
    Rec1 <- data.frame("GrEspece"=c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),
                       "GrEssRec"=c("feu","feu","rex","feu","ers","feu","rex","heg","rex","sab"))
    # répéter Rec1 autant de fois que le nombre de placettes
    # list_plot <- unique(Plac$Placette)
    # Rec <- NULL
    # for (i in 1:length(list_plot)){
    #   Rec_temp <- Rec1 %>% mutate(Placette=list_plot[i])
    #   Rec <- bind_rows(Rec,Rec_temp)
    # }
    # liste des placettes
    list_plot <- Plac %>% select(Placette) %>% unique()
    # Répéter les 10 especes pour chacune des placettes
    Rec <- crossing(list_plot, Rec1)

    ##### 6.11 Module de recrutement avec gaules #####
    if (RecruesGaules==1){

        # préparation des variables #####
        Rec <- Rec %>% select(-GrEssRec)

        # on ajoute les especes manquantes et met leur variables à 0
        RecGaules <- left_join(Rec, Nb_Gaules_Ha, by=c('Placette','GrEspece')) %>%
          replace(is.na(.),0) %>%
          group_by(Placette) %>%
          mutate(Ratio = Nb_Gaules_Ess_Ha/sum(Nb_Gaules_Ess_Ha))

        # ST marchandes et N march par placettes/espece
        St_Ess_Ha <- Plac %>%
          filter(Etat=="vivant") %>%
          mutate(Stm2ha = (DHPcm/200)^2*3.1416*Nombre/Sup_PE,
                 NbHa = Nombre/Sup_PE) %>%
          group_by(Placette, GrEspece) %>%
          summarise(St_Ess_Ha = sum(Stm2ha),
                    lnSt_Ess_Ha = log(sum(Stm2ha)+1),
                    lnNb_Ess_Ha = log(sum(NbHa)+1),
                    NbHa=sum(NbHa))

        # ajouter les especes manquantes au fichier des info marchandes
        Rec <- left_join(Rec, St_Ess_Ha, by=c('Placette','GrEspece')) %>%
          replace(is.na(.),0)

        # ajouter les variables à l'échelle de la placette
        Rec <- Plac %>%
          select(Placette, all_of(var_plot)) %>%
          group_by(Placette) %>%
          slice(1) %>%
          left_join(Rec,by='Placette')


        # Calculer le nombre de recrues
        RecSelect <- rec_n_Gaules(Rec, RecGaules, t, CovParmsGaules, RandomPlacGaules, Para.rec_gaules, seed_value=NULL) %>% select(-Pr)


       ###############################################


        ##### 6.12 Module de mise à jour du nombre de gaules

        # Préparer fichier Gaules
        Ratio <- data.frame("GrEspece"=c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"))
        Ratio <- crossing(list_plot, Ratio)


        # Calcul du nombre de gaules par essences à partir de ratios
        Ratio <- ratio_Gaules(Ratio, Rec, RecGaules, t, RandomPlacGaules, Para.nb_gaules, Para.ratio_gaules)


        # Calcul du nombre de gaules 6 et 8 cm par essence
        Nb68 <- gaules_68(Ratio, Rec, RecGaules, t, RandomPlacGaules, Para.68_ERS, Para.68_HEG, Para.68_BOJ, Para.68_SAB)


        # Remplace le fichier initial Nb_Gaules_Ha pour la prochaine step
        Nb_Gaules_Ha <- Nb68

    }
    ##### 6.13 Module du nombre de recrues sans les gaules #####
    else {

      # calcul de la ST des 10-14cm par essence
      StEss_1014 <- Plac %>%
        mutate(Stm2ha = ifelse(Etat=="vivant" & DHPcm <15.1, (DHPcm/200)^2*3.1416*Nombre/Sup_PE, 0)) %>%
        group_by(Placette, GrEspece) %>%
        summarise(logst_ess_1014 = log(sum(Stm2ha)+0.01))

      Rec <- left_join(Rec, StEss_1014, by=c('Placette','GrEspece')) %>%
        mutate(logst_ess_1014 = ifelse(is.na(logst_ess_1014)==TRUE, log(0.01), logst_ess_1014))

      # ajouter les variables à l'échelle de la placette
      Rec <- Plac %>%
        select(Placette, all_of(var_plot)) %>%
        group_by(Placette) %>%
        slice(1) %>%
        left_join(Rec,by='Placette')

      # effet aléatoire de placette du module de probabilite de recrue
      RandomRec <- RandPlacetteStep %>% filter(SubModuleID==5)

      # Calculer le nombre de recrues par groupe d'essences
      RecSelect <- rec_n(Rec, t, Para.rec_n, RandomRec, seed_value=NULL)


    }


    # Mise en forme de Plac pour fusion avec recrues
    Plac <- Plac %>%
      select(-c(DHPcm, Etat, vigu0, prod0))

    # Si fichier vide: Arret simulation et écris les valeurs dans un fichier log
    if (nrow(Plac)==0){

      break
    }
    if (nrow(Plac[which(Plac$Etat1=="vivant"),])==0){

      break
    }


    ##### 6.14 Si au moins une recrues de générées, préparer le fichier #####
    if (nrow(RecSelect)>=1){

      # aller chercher le dernier numéro d'arbre de chaque placette
      Plac_last <- Plac %>% group_by(Placette) %>% slice_tail() %>% select(Placette, ArbreID) %>% rename(ArbreID_last=ArbreID)

      # numéroter les recrues
      suppressMessages(
        RecSelect <- RecSelect %>% left_join(Plac_last, by='Placette') %>%
          group_by(Placette,GrEspece) %>%
          slice(rep(1:n(), first(NbRecrues))) %>%
          group_by(Placette) %>%
          mutate(ArbreID=c(1:n())+ArbreID_last) %>%
          ungroup()
        )

      # ajouter les variable à l'échelle de la placette au fichier des recrues
      RecSelect <- Plac %>%
        select(Placette, all_of(var_plot)) %>%
        group_by(Placette) %>%
        slice(1) %>%
        right_join(RecSelect, by='Placette') %>%
        select(-ArbreID_last, -NbRecrues)


      ##### 6.15 Module DHP Recrues #####

      # paramètres pour dhp des recrues
      RandomRecDhp <- RandPlacetteStep %>% filter(SubModuleID==6)
      varRecDhp <- CovParms$ParameterEstimate[which(CovParms$CovParm=="sigma2_res")]
      theta <- CovParms$ParameterEstimate[which(CovParms$CovParm=="theta")]


      # on applique la fonction du DHP des recrues
      RecSelect <- rec_dhp(RecSelect, t, Para.rec_dhp, RandomRecDhp, varRecDhp, theta, seed_value=NULL)


      ##### 6.16 Module Vigueur des recrues #####

      RandomRecVig <- RandPlacetteStep %>% filter(SubModuleID==7)

      # calculer la prob vigueur
      RecSelect <- rec_vig(RecSelect, Para.rec_vig, RandomRecVig, seed_value=NULL)


      ##### 6.17 Module Produit des recrues #####

      RandomRecProd <- RandPlacetteStep %>% filter(SubModuleID==8)

      # calculer la prob vigueur
      RecSelect <- rec_prod(RecSelect, Para.rec_prod, RandomRecProd, seed_value=NULL)


      ##### 6.18 Résidus des recrues #####

      # Résidus de l'arbre
      if (k < Horizon){

        # créer et remplir la matrice de var-cov ARMA(1,1)
        varcov <- expand.grid(i=1:(Horizon-k), j=1:(Horizon-k))
        varcov <- matrix(f(varcov$i, varcov$j, Residual, Gamma, Rho), nrow=Horizon-k)

        ResidusRec <- matrix(rockchalk::mvrnorm(n=nrow(RecSelect), mu=rep(0,Horizon-k), Sigma = varcov, empirical=F),ncol=(Horizon-k))
        # ResidusRec <- rockchalk::mvrnorm(n=nrow(RecSelect), mu=rep(0,Horizon-k), Sigma = varcov, empirical=F)
        ResidusRec0 <- matrix(0,nrow=nrow(RecSelect),ncol=k) # colonnes de 0 pour les steps passés
        ResidusRec <- cbind(RecSelect$Placette, RecSelect$ArbreID, ResidusRec0, ResidusRec)

        Residus <- rbind(Residus, ResidusRec)

      }

      ##### 6.19 Ajout des recrues au fichier des Placettes #####

      RecSelect <- RecSelect %>%
        mutate(Annee = Annee_Inventaire+k*t,
               NoArbre = NA,
               Espece = ifelse(GrEspece %in% c("BOJ","ERR","ERS","HEG","SAB"), GrEspece, NA),
               Etat1 = "recrue",
               Nombre = Sup_PE/0.25, # le modèle de recrutement prédit un nombre de recrues dans 1/4 ha (2500 m2), il faut ramener le nombre en fct de la superficie de la placette
               # dans Capsis, c'est ceil(Nombre), et la dernière recrue générée est une fraction pour tenir compte qu'avec ceil, on en génère trop (le ceil dans capsis sert surtout à fsaire une boucle sur le nombre de recrue, il faut donc un entier)
               # Après avoir parler avec Hugues, on a décidé simplement de ne pas faire Ceil, de laisser les fractions de recrues
               Iter = Iteration) %>%
        select(-pred_dhp, -xb_rec_vig, -prob_vig_rec, -xb_rec_prod, -prob_prod_rec)

      # ajouter les recrues aux autres arbre, ne garder que les variables essentielles
      # les variables à l'échelle de l'arbres

      #names(Plac)
      #names(RecSelect)
      #setdiff(names(Plac), names(RecSelect))
      Plac <- Plac %>% #select(-aam, -xb_mort, -prob_mort, -xb_acc, -pred_acc, -xb_vig, -prob_vig, -xb_prod, -prob_prod) %>%
        bind_rows(RecSelect)

      # ABCD et MSCR pas dans le fichier des recrues

    } # Fin du recrutement


    ##### 6.20 S'il n'y a pas de recrues prédites #####
    # else{
    #   Plac[, aam := NULL]
    # }


    ##### 6.21 Préparation du fichier de la fin de step #####

    # enlever les variables
    #Plac <- Plac %>% select(-aam, -xb_mort, -prob_mort, -xb_acc, -pred_acc, -xb_vig, -prob_vig, -xb_prod, -prob_prod, -Intercept1, -Intercept2, -Intercept3, -PredQual, -ABCD_orig)
    #Plac <- Plac %>% select(-aam, -xb_mort, -prob_mort, -xb_acc, -pred_acc, -xb_vig, -prob_vig, -xb_prod, -prob_prod)
    # Plac <- Plac %>% select(-aam, -st_tot0, -dens_tot0, -t0_aj_, -fact_red)
    Plac <- Plac %>% select(-aam, -st_tot0, -dens_tot0, -fact_red)


    # Retrait des residus des arbres morts
    if (k < Horizon){
      m <- which(Plac$Etat1!="mort", arr.ind = TRUE) # où sont les vivants
      #Residus <- matrix(Residus[m,], nrow=length(m), ncol=Horizon+1)
      Residus <- Residus[m,]
    }

    # Renommer les variables
    Plac <- Plac %>%
      rename(Etat=Etat1,DHPcm=DHPcm1,vigu0=vigu1,prod0=prod1)


    ##### 6.22 Préparation de la fin de step des gaules #####

    if (RecruesGaules==1){

      # transposer le fichier pour avoir les essences en colonne
      Nb_Gaules_Ha_tr1 <- Nb_Gaules_Ha %>%
        group_by(Placette) %>%
        mutate(Nb_Gaules_Ha = sum(Nb_Gaules_Ess_Ha)) %>% # recalculer le total des gaules, car à cause des arrondis, la somme des essence de ne plus tout à fait Nb_Gaules_Ha
        filter(GrEspece %in% c('ERS','HEG','BOJ','SAB')) %>%
        select(Placette, Nb_Gaules_Ha, GrEspece, Nb_Gaules_Ess_Ha) %>%
        group_by(Placette, Nb_Gaules_Ha) %>%
        pivot_wider(names_from = GrEspece, names_prefix = 'Nb_Gaules_', values_from = Nb_Gaules_Ess_Ha)

      Nb_Gaules_Ha_tr2 <- Nb_Gaules_Ha %>% filter(GrEspece %in% c('ERS','HEG','BOJ','SAB')) %>% select(Placette, GrEspece, Nb_Gaules_68_Ess_Ha) %>%
        group_by(Placette) %>%
        pivot_wider(names_from = GrEspece, names_prefix = 'Nb_Gaules_68_', values_from = Nb_Gaules_68_Ess_Ha)

      #Plac$Nb_Gaules_68_Ha<-sum(exp(Nb_Gaules_Ha$lnNb_Gaules_68_Ess_Ha)-1) # je ne mettrais pas cette variable, car on n'a pas le nombre de 68 pour toutes les essences, alors le total n'est pas représentatif
      Nb_Gaules_Ha_tr <- left_join(Nb_Gaules_Ha_tr1, Nb_Gaules_Ha_tr2, by='Placette')

      Plac <- Plac %>% select(-contains('Nb_Gaules_'))
      Plac <- left_join(Plac, Nb_Gaules_Ha_tr, by='Placette')

    }

    # on ajoute les donnees du pas de simulation en cours aux autres pas de simulation pour la placette
    outputTot <- bind_rows(outputTot, Plac)


  }  # fin de la boucle des simulations



  #### 7. Traitement MSRC ####

  ##### Ceci aurait pu être mis en dehors de la fct SaMARE, directement dans la fonction principale, à la fin de toutes les itérations

  ##### 7.1 S'il y a eu simulation #####
  if (!is.null(outputTot)){

    setDT(outputTot)
    setDT(outputInitial)
    if (RecruesGaules==1){
      setDT(gaule_init)
      outputInitial <- merge(outputInitial, gaule_init, by='Placette', all.x=TRUE)
    }


    # enlever les variables de trop
    # outputTot[, c("Annee_Inventaire", "trt", "ntrt", "t0", "pente", "type_pe_Plac", "dom", "rid1","vegp",
    #               "latitude", "longitude", "altitude", "reg", "teco", "prec", "temp", "grwd", "MSCR", "Sup_PE") := NULL]
    outputTot[, c("pente", "type_pe_Plac", "dom", "rid1","vegp",
                  "latitude", "longitude", "altitude", "reg", "teco", "prec", "temp", "grwd", "MSCR", "Sup_PE") := NULL] #  "trt", "ntrt", "t0", "Annee_Inventaire"


    ###### 7.1.1 Si le fichier initial contient des valeurs manquantes dans MSCR ######


    # on supprime la colonne et on va estimer le MSCR pour tous les pas de simulation, incluant la step 0
    if (any(is.na(outputInitial$MSCR))){


      outputInitial[, MSCR := NULL]
      outputTot[, Residuel := 0]
      outputTot <- rbind(outputInitial, outputTot, fill=TRUE)

      # attribuer une valeurs MSCR
      MSCR <- AttribMSCR(outputTot, Para) %>% select(-contains("pred"), -contains("prob"), -Alea)
      outputTot <- MSCR

    }
    ###### 7.1.2 Si la colonne MSCR ne contient aucune données manquantes dans le data initial ######

    # on va estimer MSCR seulement pour les step>0
    else{

      outputTot[, Residuel := 0]
      MSCR <- AttribMSCR(outputTot, Para) %>% select(-contains("pred"), -contains("prob"), -Alea)

     # ajouter le data des valeurs de départ
      outputTot <- rbind(outputInitial, MSCR, fill=TRUE)
    }

  }

  ##### 7.2 S'il n'y a pas eu de simulation #####

  else{ # puisque le nombre de pas de simulation doit être au moins 1, il y aura toujours un fichier outputTot non NULL

    ###### 7.2.1 Si le fichier initial contient des valeurs manquantes dans la colonne MSCR ######

    # on supprime la colonne et on va estimer le MSCR pour tous
    if (any(is.na(outputInitial$MSCR))){

      # supprimer MSCR
      outputTot <- outputInitial[, MSCR := NULL]

      MSCR <- AttribMSCR(outputTot, Para) %>% select(-contains("pred"), -contains("prob"), -Alea)
      outputTot <- MSCR

    }
    ###### 7.2.2 Si la colonne MSCR n'a pas de données manquantes ######

    else{
      # retourner le fichier initial directement
      outputTot <- outputInitial

    }

  }

  setDT(outputTot)

  return(outputTot)

}

