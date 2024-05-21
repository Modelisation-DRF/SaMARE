#' Fonction qui effectue la simulation de l'évolution des arbres d'une placette
#' du simulateur SaMARE. Cette fonction effectue la simulation et
#' appele chacune des fonction permettant de prévoir la mortalité, l'accroissement,
#' le recrutement et l'évolution des paramètres de simulation.
#'
#'
#' @param Random Un dataframe contenant des effets aléatoires à l'échelle de
#'               la placette et de la période de simulation pour les modules
#'               de SaMARE qui n'utilisent pas les gaules.
#' @param RandomGaules  Un dataframe contenant des effets aléatoires à l'échelle de
#'                      la placette et de la période de simulation pour les modules
#'                      de SaMARE qui utilisent les gaules.
#' @param Data    Un dataframe contenant une liste des arbres de dimenssion marchande
#'                d'une placette à simuler ainsi que des variables nécessaire à
#'                la simulation.
#' @param Gaules  Un dataframe contenant la distribution du nombre de gaules par
#'                classe de diamètre et par groupe d'essence d'une placette à
#'                simuler ainsi que des variables nécessaire à la simulation.
#' @param ListeIter Un dataframe contenant le numéro de la placette à simuler
#'                  et le numéro de l'iteration à effecuer.
#' @param AnneeDep Année de départ de la simulation.
#' @param Horizon Nombre de pas de 5 ans de simulation à effectuer.
#' @param RecruesGaules  Variable prenant la valeur de "1" pour utiliser les
#'                       paramètres de recrutement basé sur l'inventaire des gaules
#'                       de la placette et de "0" pour utiliser le module de
#'                       recrutement basé sur les arbres de dimension marchande.
#' @param MCH Variable prenant la veleur de 1 en présence de maladie corticale du hêtre dans
#'            la placette et 0 lorsque la maladie est absente. Lorsque la maladie corticale
#'            est présente,la probabilité de mortalié des hêtres est estimée avec
#'            l'équation de l'avis technique AT-SSRF 20 de la Direction de la recherche forestière.
#' @param CovParms Un dataframe contenant la variance des effets aléatoires des
#'                  équations des modules de base de SaMARE (modules 1 à 9 et 17 à 19).
#' @param CovParmsGaules Un dataframe contenant la variance des effets aléatoires des
#'                        équations des modules de SaMARE basés sur l'information
#'                        provenant des gaules (modules 10 à 16).
#' @param Para  Un dataframe contenant les paramètres des modules de 1 à 9 et 17 à 19
#'               (modules de base) de SaMARE.
#' @param ParaGaules Un dataframe contenant les paramètres des modules 17à 19
#'               (modules basés sur les gaules) de SaMARE.
#' @param Omega Un dataframe contenant pour chaque module de base de SaMARE
#'              (modules 1 à 9 17 à 19) le éléments du triangle inférieur de
#'               la matrice de variance-covariance.
#' @param OmegaGaules Un dataframe contenant pour chaque module basé sur les gaules
#'                    de SaMARE (modules 10 à 16) les éléments du triangle inférieur de
#'                    la matrice de variance-covariance.
#' @return Retourne un dataframe avec une liste d'arbres vivants et mort ainsi que
#'        leur DHP pour chaque étape de 5 ans de l'horizon de simulation.
#' @examples


SaMARE<- function(Random, RandomGaules, Data, Gaules, ListeIter, AnneeDep, Horizon, RecruesGaules,
                  MCH,CovParms,CovParmsGaules,Para,ParaGaules,Omega,OmegaGaules){
   select=dplyr::select
  t<-5


 #####################################################################
  ################## convertion MSCR #################################

  # Para.ConvMSCRVig<-Para %>%
  #   filter(SubModuleID==17)  #parametres pour vigueur
  # Para.ConvMSCRProd1024<-Para %>%
  #   filter(SubModuleID==18)  #parametres pour produits<24 cm
  # Para.ConvMSCRProd24<-Para %>%
  #   filter(SubModuleID==19)  #parametre pour produits>24cm

  #Liste d'Especes
  Especes<- c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB")


  ################################################################################
  ######## Calcul des variables a l'echelle de la  placette #####################
  ###############################################################################

  #Création placette origine
  # Selectionner la placette et initialiser l'annee de depart et la correction du biais

  PlacOri <- Data %>%
    filter(Placette==ListeIter$Placette) %>%
    mutate(Annee = AnneeDep, Iter=ListeIter$Iter)

  #########Creation paramètres

  ParaTot<-map_dfr(seq_len(1), ~Para) %>%
    mutate(Iter=1)

  ##############Gaules


  ParaTotGaules<-map_dfr(seq_len(1), ~ParaGaules) %>%
    mutate(Iter=1)


  ########################Calcul des parametres des modules

  Para.mort<-ParaOmega(ModuleID = 1,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.acc<-ParaOmega(ModuleID = 2,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.vig<-ParaOmega(ModuleID = 3,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.prod<-ParaOmega(ModuleID = 4,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_n<-ParaOmega(ModuleID = 5,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_dhp<-ParaOmega(ModuleID = 6,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_vig<-ParaOmega(ModuleID = 7,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_prod<-ParaOmega(ModuleID = 8,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.ConvMSCRVig<-ParaOmega(ModuleID = 17,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.ConvMSCRProd1024<-ParaOmega(ModuleID = 18,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.ConvMSCRProd24<-ParaOmega(ModuleID = 19,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])

  ###################Calcul des parametres des gaules

  Para.rec_gaules<-ParaOmega(ModuleID = 10,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.nb_gaules<-ParaOmega(ModuleID = 11,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.ratio_gaules<-ParaOmega(ModuleID = 12,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_ERS<-ParaOmega(ModuleID = 13,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_HEG<-ParaOmega(ModuleID = 14,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_BOJ<-ParaOmega(ModuleID = 15,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_SAB<-ParaOmega(ModuleID = 16,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])

  ##########################Création de la placette de simulation

  Plac<-PlacOri %>%
    filter(Etat %in% c(10,11,12,40,42,30,32,50,52,70,71,72)) %>%
    mutate(Etat=ifelse(Etat==11,"martele","vivant"),ArbreID=seq(1:n())) %>%
    select(Placette,Annee,ArbreID,NoArbre,GrEspece,Espece,Etat,
           DHPcm,Nombre,Vigueur,Iter,MSCR,ABCD)



  ###################Calcul des parametres d'es'evolution de la qualite

  PlacQual<-Plac %>%
    filter(ABCD %in% c("A","B","C","D") & Etat=="vivant")

  if (nrow(PlacQual)>=1){

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


    Para.EvolQualBOJ1<-ParaOmega(ModuleID = 1,ParaOri=ParaBOJ, ParaIter=ParaBOJ,Omega=OmegaBOJ,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualBOJ2<-ParaOmega(ModuleID = 2,ParaOri=ParaBOJ, ParaIter=ParaBOJ,Omega=OmegaBOJ,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualBOJ3<-ParaOmega(ModuleID = 3,ParaOri=ParaBOJ, ParaIter=ParaBOJ,Omega=OmegaBOJ,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualERR1<-ParaOmega(ModuleID = 1,ParaOri=ParaERR, ParaIter=ParaERR,Omega=OmegaERR,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualERR2<-ParaOmega(ModuleID = 2,ParaOri=ParaERR, ParaIter=ParaERR,Omega=OmegaERR,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualERS1<-ParaOmega(ModuleID = 1,ParaOri=ParaERS, ParaIter=ParaERS,Omega=OmegaERS,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualERS2<-ParaOmega(ModuleID = 2,ParaOri=ParaERS, ParaIter=ParaERS,Omega=OmegaERS,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualERS3<-ParaOmega(ModuleID = 3,ParaOri=ParaERS, ParaIter=ParaERS,Omega=OmegaERS,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualFEN1<-ParaOmega(ModuleID = 1,ParaOri=ParaFEN, ParaIter=ParaFEN,Omega=OmegaFEN,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualFEN2<-ParaOmega(ModuleID = 2,ParaOri=ParaFEN, ParaIter=ParaFEN,Omega=OmegaFEN,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualHEG1<-ParaOmega(ModuleID = 1,ParaOri=ParaHEG, ParaIter=ParaHEG,Omega=OmegaHEG,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualHEG2<-ParaOmega(ModuleID = 2,ParaOri=ParaHEG, ParaIter=ParaHEG,Omega=OmegaHEG,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])
    Para.EvolQualHEG3<-ParaOmega(ModuleID = 3,ParaOri=ParaHEG, ParaIter=ParaHEG,Omega=OmegaHEG,NbIter=1) %>%mutate(Iter=PlacOri$Iter[1])

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


  #Placette Gaules de simulation
  if (RecruesGaules==1){

    PlacGaules<-Gaules %>%
      filter(Placette==ListeIter$Placette) %>%
      mutate(Iter=ListeIter$Iter)

  }


  ###################Variables d'information sur la placette####################

  #Placette
  Placette<-PlacOri$Placette[1]

  #Iteration
  Iterj<-PlacOri$Iter[1]

  #Superficie Placette
  Sup_PE<-PlacOri$Sup_PE[1]

  #Annee Derniere Coupe
  t0<-PlacOri$Annee_Coupe[1]

  #Variable du peuplement residuel avec condition que si St >26 = TEM
  trt<-ifelse("martele" %in% Plac$Etat,"CP",
              ifelse((is.na(PlacOri$ntrt[1])==TRUE | PlacOri$ntrt[1]==0),"TEM",
                     ifelse(AnneeDep-PlacOri$Annee_Coupe[1]<5 &
                              (sum((Plac$DHPcm/200)^2*3.1416*Plac$Nombre)/Sup_PE)>26,"TEM","CP")))

  #Nombre de traitements
  ntrt=ifelse(trt=="CP" & "martele" %in% Plac$Etat,PlacOri$ntrt[1]+1,
              ifelse(trt=="CP",PlacOri$ntrt[1],0))

  #Type de placette
  type_pe_Plac<-ifelse(PlacOri$Sup_PE[1]==0.04,"type0",
                       ifelse(PlacOri$Sup_PE[1]>=0.25 & PlacOri$Sup_PE[1]<=0.5,"type1","type2"))


  #Variables de classification écologiques
  latitude<-PlacOri$Latitude[1]

  longitude<-PlacOri$Longitude[1]

  altitude<-PlacOri$Altitude[1]

  pente<-ifelse(is.na(PlacOri$Pente[1]==TRUE),6.5,PlacOri$Pente[1])

  dom<-substr(PlacOri$Reg_Eco[1],1,1) %>% ifelse(!. %in% c("2","3","4"),"4",. )##valeur de 4 attribué aux domaines pas dans la liste d'effets

  reg<-PlacOri$Reg_Eco[1]

  rid1<-ifelse(reg %in% c("1a","2a","2b","2c"),"2o",ifelse(reg %in% c("4a","4b","4c"),"4o",
                                                      ifelse(reg %in% c("4d","4e","4f","4g","4h"),"4e",reg))) %>%
         ifelse(!. %in% c("2o","3a","3b","3c","3d","4e","4o","DU","SV"),"4o",.)  #Ajouté DU et SV qui manquaient et mis "4o" quand manquant
  teco<-PlacOri$Type_Eco[1]

  vegp<-substr(PlacOri$Type_Eco[1],1,3)

  # Variables climatiques de la placette
  prec <- PlacOri$Ptot[1]

  temp <- PlacOri$Tmoy[1]

  grwd<-  PlacOri$GrwDays[1]

  ##################Effets Aleatoires  de la placette#############################

  RandPlac<-Random %>% filter(Placette==PlacOri$Placette[1] & Iter==PlacOri$Iter[1])


  ################## Effets Aleatoires placette Gaules############################

  if(RecruesGaules==1){
    RandomPlacGaules<-RandomGaules %>% filter(Placette==PlacOri$Placette[1] & Iter==PlacOri$Iter[1])
  }


  ###############################################################################
  ########################Simulation de la placette##############################
  ##############################################################################

  # Initialisation du fichier qui contiendra les résultats de simulation de la placette

  outputTot<-c()

  ###############################################################################
  #################### boucle pour les k périodes de 5 ans a simuler #############

  for (k in 1:Horizon){


    ##################Mise à jour des variable pour la période se simulation########

    # Si premier pas de simulation, on utilise le fichier de depart de la placette



    if   (k==1){

      Plac <- Plac %>%
        mutate(Annee=AnneeDep+t)

      ##################Atribution de vigueur et de produit##############


      ParaViglist=list(Para.ConvMSCRVig)


      Vigu0<-Plac %>%
        group_by(Placette,NoArbre) %>%
        nest() %>%
        mutate(vigu0 = mapply(AttribVigu0,data,MoreArgs=ParaViglist)) %>%
        unnest(vigu0) %>%
        select(-data)


      ParaProdlist=list(Para.ConvMSCRProd1024,Para.ConvMSCRProd24)

      Prod0<-Plac %>%
        group_by(Placette,NoArbre) %>%
        nest() %>%
        mutate(prod0 = mapply(AttribProd0,data,MoreArgs=ParaProdlist)) %>%
        unnest(prod0) %>%
        select(-data)

      suppressMessages(
        Plac<-Plac %>%
          left_join(Vigu0) %>%
          left_join(Prod0))


      ########Dataframe avec les conditions initiales de la placette##############

      outputInitial<-Plac %>%
        mutate(Annee=AnneeDep,Residuel=0) %>%
        select(Placette, Annee, ArbreID, NoArbre, Nombre, GrEspece,
               Espece, Etat, DHPcm, vigu0, prod0, ABCD, MSCR, Residuel, Iter)

      #######Modification placette et création de la mesure résiduelle si martelage

      if ("martele" %in% Plac$Etat){

        outputInitial<-Plac %>%
          filter(Etat=="vivant") %>%
          mutate(Annee=AnneeDep,Residuel=1) %>%
          select(Placette, Annee, ArbreID, NoArbre, Nombre, GrEspece,
                 Espece, Etat, DHPcm, vigu0, prod0, ABCD, MSCR, Residuel, Iter) %>%
          rbind(outputInitial,.)

        Plac<-Plac %>%
          filter(Etat=="vivant") %>%
          select(-MSCR)

        t0=AnneeDep

      }

  ########################### Résidus de l'arbre####################################

      Periodes<-c("ArbreID",paste("Periode","_",c(1:Horizon),sep=""))

      Residus<-matrix(0,nrow=nrow(Plac),ncol=Horizon+1)

      colnames(Residus)<-Periodes

      Residus[,1]<-Plac$ArbreID

      Residual<-CovParms$ParameterEstimate[which(CovParms$CovParm=="Residual")]

       Rho<-CovParms$ParameterEstimate[which(CovParms$CovParm=="RHO")]

       Gamma<-CovParms$ParameterEstimate[which(CovParms$CovParm=="Gamma")]

      for (i in 1:Horizon){

        Residus[,i+1]<-rnorm(nrow(Residus),mean=0,sqrt(Residual*Gamma*(Rho^(i-1))))
      }

      # fonction pour générer un element de la matrice de var-cov ARMA(1,1)
     # f <- function(i, j, var_res, gamma, rho) { (((i-j)!=0)*(var_res * gamma*rho^(abs(j-i)-1)))+( ((i-j)==0)*var_res) } # correlation arma
      # créer et remplir la matrice de var-cov
      # varcov <- expand.grid(i=1:Horizon, j=1:Horizon)
      # varcov <- matrix(f(varcov$i, varcov$j, Residual, Gamma, Rho), nrow=Horizon)
      #
      # Residus <- rockchalk::mvrnorm(n=nrow(Plac), mu=rep(0,Horizon), Sigma = varcov, empirical=T)
      # Residus <- cbind(Plac$ArbreID, Residus)
      # colnames(Residus)<-c("ArbreID",paste("Periode","_",c(1:Horizon),sep=""))
      #
      #

######### Mise en forme des statistiques de gaules qui seront mises à jour par la suite

      if (RecruesGaules==1){
        RecGaules<-data.frame("GrEspece"=c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"))

        suppressMessages(
          Nb_Gaules_Ha<-PlacGaules %>%
            mutate(NbHa=Nombre/Sup_PE, NbHa68=ifelse(DHPcm>5,Nombre/Sup_PE,0)) %>%
            group_by(GrEspece) %>%
            summarise(Nb_Gaules_Ess_Ha=sum(NbHa),
                      lnNb_Gaules_Ess_Ha=log(sum(NbHa)+1),
                      lnNb_Gaules_24_Ess_Ha=log(sum(NbHa)-sum(NbHa68)+1),
                      lnNb_Gaules_68_Ess_Ha=log(sum(NbHa68)+1)))

        outputTot$Nb_Gaules_Ha<-sum(Nb_Gaules_Ha$Nb_Gaules_Ess_Ha)

        outputTot$Nb_Gaules_68_Ha<-sum(exp(Nb_Gaules_Ha$lnNb_Gaules_68_Ess_Ha)-1)

      }

    }  else {    # Si 2e pas de simulation ou plus, on prend le fichier qui contient les simulations et on garde seulement le dernier pas

      Plac <- outputTot %>%
        filter(Annee == AnneeDep+(k-1)*t & Etat!="mort") %>%
        mutate(Annee=AnneeDep+k*t, Etat="vivant")

    }

    #Temps depuis coupe
    if(trt=="CP"){
      t0_aj_<-Plac$Annee[1]-t0-4.9}else{t0_aj_=0} ######on ajuste pour que lorsque la step débute immédiatement après coupe t0_aj a une valeur de 0.1

    #Réduction de la mortalité
    fact_red<-ifelse(trt=="TEM",0,ifelse(t0_aj_<=3,1,0))

    # calcul variable echelle placette
    st_tot0 <- sum((Plac$DHPcm/200)^2*3.1416*Plac$Nombre)/Sup_PE


    dens_tot0 <- sum(Plac$Nombre)/Sup_PE

    # Random iteration
    RandPlacetteStep<-RandPlac %>% filter(Step==k)

    # Recrutement Gaules
    if (RecruesGaules==1){
      RecGaules<-data.frame("GrEspece"=c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"))

      suppressMessages(
        RecGaules<-RecGaules %>%
          left_join(Nb_Gaules_Ha) %>%
          mutate(Nb_Gaules_Ess_Ha=ifelse(is.na(Nb_Gaules_Ess_Ha)==TRUE,0,Nb_Gaules_Ess_Ha),
                 lnNb_Gaules_Ess_Ha=ifelse(is.na(lnNb_Gaules_Ess_Ha)==TRUE,log(1),lnNb_Gaules_Ess_Ha),
                 lnNb_Gaules_24_Ess_Ha=ifelse(is.na(lnNb_Gaules_24_Ess_Ha)==TRUE,log(1),lnNb_Gaules_24_Ess_Ha),
                 lnNb_Gaules_68_Ess_Ha=ifelse(is.na(lnNb_Gaules_68_Ess_Ha)==TRUE,log(1),lnNb_Gaules_68_Ess_Ha),
                 Ratio=Nb_Gaules_Ess_Ha/sum(Nb_Gaules_Ess_Ha)))
    }

    ############################### Mortalite ######################################

    Mort <- Plac

    # Effets aléatoires pour la mortalité
    RandomMort<-RandPlacetteStep %>% filter(SubModuleID==1)

    # Application de la fonction de mortalité
    pred<-mort(Mort,trt,temp,type_pe_Plac,fact_red,t,Iterj,MCH,Para.mort)


    Mort$pred_mort<-pred

    Plac <- Mort %>%
      mutate(pred_mort=(1-exp(-exp(pred_mort+RandomMort$RandomPlac+RandomMort$RandomStep))),Alea=runif(n()),
             Etat1=as.character(ifelse(Alea<=pred_mort,"mort",Etat))) %>%
      select(-pred_mort,-Alea)

    ##################### Accroissement en diamètre#################################

    # fonction d'accroissement en dhp pour etre appliquee a un arbre
    #accijk=(Xijk*B+bi+bik/Dt^0.5)2

    # fichier des arbres de la placette pour appliquer le module d'accroissement
    Accrois <- Plac

    # Effets aléatoire pour l'accroissement
    RandomAcc <- RandPlacetteStep %>% filter(SubModuleID==2)



    # on applique la fonction d'accroissement

    pred<-accrois(Accrois,st_tot0,t,fact_red,ntrt,type_pe_Plac,Iterj,Para.acc)

    Accrois$pred_acc<-((pred+RandomAcc$RandomPlac+RandomAcc$RandomStep+Residus[,k+1])^2)-1

    Plac<- Accrois %>%
      mutate(pred_acc=ifelse(pred_acc<0,0,pred_acc)) %>%
      mutate(DHPcm1=as.numeric(ifelse(Etat1=="vivant",DHPcm+(round(pred_acc)/10),NA)),
             aam=as.numeric(ifelse(Etat1=="vivant",round(pred_acc)/(10*t),NA))) %>%
      select(-pred_acc) %>%
      arrange(ArbreID)


    ########################VIGUEUR#################################################
    Vig <- Plac %>% filter(Etat=="vivant")

    # Application de la fonction de vigueur
    pred<-vig(Vig,type_pe_Plac,rid1,Iterj,Para.vig)

    RandomVig<-RandPlacetteStep %>% filter(SubModuleID==3)

    Vig$pred_vig<-pred+RandomVig$RandomPlac

    suppressMessages(
      Plac <- Vig %>%
        mutate(pred_vig=(exp(pred_vig)/(1+exp(pred_vig))),Alea=runif(n()),
               vigu1=as.character(ifelse(Alea<=pred_vig,"ViG","NONVIG"))) %>%
        select(ArbreID,vigu1) %>%
        right_join(Plac) )

    #################PRODUIT#######################################################

    Prod <- Plac %>% filter(prod0!="resineux" & Etat1 !="mort")

    # Application de la fonction de produit

    pred<-produit(Prod,type_pe_Plac,rid1,Iterj,Para.prod)

    RandomProd<-RandPlacetteStep %>% filter(SubModuleID==4)

    Prod$pred_prod<-pred+RandomProd$RandomPlac

    suppressMessages(
      Plac <- Prod %>%
        mutate(pred_prod=(exp(pred_prod)/
                            (1+exp(pred_prod))),Alea=runif(n()),
               prod1=ifelse(GrEspece=="AUT"|(vigu0=="NONVIG" & DHPcm1<23.1),"pate",
                            ifelse(Alea<=pred_prod,"sciage","pate"))) %>%
        select(ArbreID,prod1) %>%
        right_join(Plac) %>%
        mutate(prod1=as.character(ifelse(prod0=="resineux","resineux",prod1))) %>%
        arrange(ArbreID))

    #############################Evolution Qualite##############################

    PlacQual<-Plac %>%
      filter(ABCD %in% c("A","B","C","D") & Etat=="vivant")

    if (nrow(PlacQual)>0){

      TigesQual<-EvolQual(PlacQual,type_pe_Plac,prec,rid1,dens_tot0,Para.EvolQualTot)
      suppressMessages(
        Plac<-Plac %>%
          left_join(TigesQual) %>%
          mutate(ABCD=ABCD1) %>%
          select(-ABCD1))

    }

    rm(PlacQual)

    ##############################Attribution Qualite###########################

   if (length(PlacOri$ABCD[which(PlacOri$ABCD %in% c("A","B","C","D"))])>0){

     PlacSansQual<-Plac %>%
      filter(GrEspece %in% c("BOJ","ERR","ERS","FEN","FIN","HEG") & (is.na(ABCD)==TRUE | is.null(ABCD)==TRUE | ABCD=="") & DHPcm1>=23.05 & DHPcm<23.05)

    if (nrow(PlacSansQual)>0){

      PlacSansQual<-AttribQualFct(PlacSansQual,rid1)

      suppressMessages(
        Plac<-Plac %>%
          left_join(PlacSansQual) %>%
          mutate(ABCD=ifelse(is.na(PredQual)==FALSE,PredQual,ABCD)) %>%
          select(-PredQual))
    }

   }



    ##################RECRUTEMENT##################################################

    #################Nombre de  Recrues
    Rec<-data.frame("GrEspece"=c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"))

    if (RecruesGaules==1){
      suppressMessages(
        St_Ess_Ha<-Plac %>%
          filter(Etat=="vivant") %>%
          mutate(Stm2ha=(DHPcm/200)^2*3.1416*Nombre/Sup_PE, NbHa=Nombre/Sup_PE) %>%
          group_by(GrEspece) %>%
          summarise(St_Ess_Ha=sum(Stm2ha),lnSt_Ess_Ha=log(sum(Stm2ha)+1),
                    lnNb_Ess_Ha=log(sum(NbHa)+1),NbHa=sum(NbHa)))

      suppressMessages(
        Rec<-Rec %>%
          left_join(St_Ess_Ha) %>%
          mutate(lnSt_Ess_Ha=ifelse(is.na(lnSt_Ess_Ha)==TRUE,log(1),lnSt_Ess_Ha),
                 St_Ess_Ha=ifelse(is.na(St_Ess_Ha)==TRUE,0,St_Ess_Ha),
                 NbHa=ifelse(is.na(NbHa)==TRUE,0,NbHa),
                 lnNb_Ess_Ha=ifelse(is.na(lnNb_Ess_Ha)==TRUE,log(1),lnNb_Ess_Ha)))

      disp<-CovParmsGaules$ParameterEstimate[which(CovParmsGaules$SubModuleID==10 & CovParmsGaules$response=="disp")]

      Rec$Pi<-rec_pi_Gaules(Rec,RecGaules,t,st_tot0,Iterj,RandomPlacGaules,Para.rec_gaules)

      Rec$Count<-rec_count_Gaules(Rec,RecGaules,t,st_tot0,Iterj,RandomPlacGaules,Para.rec_gaules)

      RecBase<-map_dfr(seq_len(151), ~Rec) %>% #dataframe de base pour le recrutement
        arrange(GrEspece) %>%
        mutate(m=rep(c(0,1:150),10))


      RecTot<-RecBase %>%
        mutate(mu=rep(c(0,rep(1,150)),10)) %>%
        mutate(Pr=(gamma(m+1/disp))/(gamma(1/disp)*factorial(m))*(1/(Count*disp+1))^(1/disp)*
                 ((Count*disp)/(Count*disp+1))^m) %>%
        mutate(Pr=(Pi+(1-Pi)*Pr)^(1-mu)*((1-Pi)*Pr)^mu) %>%
        group_by(GrEspece) %>%
        mutate(CumPr=ifelse(m==150,1,cumsum(Pr)))#Assure d'avoir un ,aximum de 150 recrues

      suppressMessages(
        RecSelect<-RecTot%>%
          group_by(GrEspece) %>%
          mutate(Alea=runif(1)) %>%
          mutate(Valeur=CumPr >Alea) %>%
          filter(Valeur=="TRUE") %>%
          summarise(NbRecrues=first(m))%>%  #
          filter(NbRecrues!=0))

      ################## Mise à jour Nombre de gaules

      predNbGaules<-round (nb_Gaules(Rec,RecGaules,t,st_tot0,altitude,latitude,trt,t0_aj_,
                                     longitude,temp,pente,Iterj,RandomPlacGaules,Para.nb_gaules))

      ################## Ratio Gaules
      Ratio<-data.frame("GrEspece"=c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"))

      Ratio$Pi<-ratio_pi_Gaules(Ratio,Rec,RecGaules,t,st_tot0,latitude,longitude,
                                Iterj,RandomPlacGaules,Para.ratio_gaules)

      Ratio$Count<-ratio_count_Gaules(Ratio,Rec,RecGaules,t,st_tot0,latitude,longitude,
                                      prec,trt,t0_aj_,Iterj,RandomPlacGaules,Para.ratio_gaules)

      Ratio<-Ratio %>%
        mutate(TotRatio=(1-Pi)*Count,FinalRatio=TotRatio/sum(TotRatio),
               Nb_Gaules_Ha=as.numeric(predNbGaules)) %>%
        mutate(Nb_Gaules_Ess_Ha=as.numeric(FinalRatio*Nb_Gaules_Ha),
               lnNb_Gaules_Ess_Ha=as.numeric(log(Nb_Gaules_Ess_Ha+1)))

      ########## Nb 6 et 8

      Pred68ERS<-round((1-pi68ERS(RecGaules,Ratio,Iterj,RandomPlacGaules,Para.68_ERS))*
                         count68ERS(RecGaules,Ratio,dens_tot0,grwd,Iterj,RandomPlacGaules,Para.68_ERS))

      Pred68HEG<-round((1-pi68HEG(RecGaules,Ratio,Rec,Iterj,RandomPlacGaules,Para.68_HEG))*
                         count68HEG(RecGaules,Ratio,Rec,Iterj,RandomPlacGaules,Para.68_HEG))*0.71 #Correction basé sur le biais moyen observé

      Pred68BOJ<-round((1-pi68BOJ(RecGaules,Ratio,Rec,trt,t0_aj_,altitude,Iterj,RandomPlacGaules,Para.68_BOJ))*
                         count68BOJ(RecGaules,Ratio,t,trt,t0_aj_,latitude,Iterj,RandomPlacGaules,Para.68_BOJ))

      Pred68SAB<-round((1-pi68SAB(RecGaules,Ratio,dens_tot0,Iterj,RandomPlacGaules,Para.68_SAB))*
                         count68SAB(RecGaules,Ratio,Rec,trt,t0_aj_,dens_tot0,Iterj,RandomPlacGaules,Para.68_SAB))


      ##########Mise à jour Gaules

      Nb68<-Ratio %>%
        mutate(lnNb_Gaules_24_Ess_Ha=ifelse(GrEspece=="ERS",ifelse(Nb_Gaules_Ess_Ha[5]>Pred68ERS,log(Nb_Gaules_Ess_Ha[5]-Pred68ERS+1),0),
                                            ifelse(GrEspece=="HEG",ifelse(Nb_Gaules_Ess_Ha[8]>Pred68HEG,log(Nb_Gaules_Ess_Ha[8]-Pred68HEG+1),0),
                                                   ifelse(GrEspece=="BOJ",ifelse(Nb_Gaules_Ess_Ha[2]>Pred68BOJ,log(Nb_Gaules_Ess_Ha[2]-Pred68BOJ+1),0),
                                                          ifelse(GrEspece=="SAB",ifelse(Nb_Gaules_Ess_Ha[10]>Pred68SAB,log(Nb_Gaules_Ess_Ha[10]-Pred68SAB+1),0),0))))) %>%

        mutate(lnNb_Gaules_68_Ess_Ha=ifelse(GrEspece=="ERS",log(Pred68ERS+1),
                                            ifelse(GrEspece=="HEG",log(Pred68HEG+1),
                                                   ifelse(GrEspece=="BOJ",log(Pred68BOJ+1),
                                                          ifelse(GrEspece=="SAB",log(Pred68SAB+1),0))))) %>%
        select(GrEspece,Nb_Gaules_Ha,Nb_Gaules_Ess_Ha,lnNb_Gaules_Ess_Ha,lnNb_Gaules_24_Ess_Ha,lnNb_Gaules_68_Ess_Ha)



      Nb_Gaules_Ha<-Nb68 #########Remplace le fichier initial Nb_Gaules_Ha

    }else{ ##############Début du module de recrutement sans les gaules

      suppressMessages(
        StEss_1014<-Plac %>%
          filter(Etat=="vivant" & DHPcm <15.1) %>%
          mutate(Stm2ha=(DHPcm/200)^2*3.1416*Nombre/Sup_PE) %>%
          group_by(GrEspece) %>%
          summarise(logst_ess_1014=log(sum(Stm2ha)+0.01)))



      suppressMessages(
        Rec<-Rec %>%
          left_join(StEss_1014) %>%
          mutate(logst_ess_1014=ifelse(is.na(logst_ess_1014)==TRUE,log(0.01),logst_ess_1014),
                 GrEssRec=c("feu","feu","rex","feu","ers","feu","rex","heg","rex","sab")))


      RandomRec<-RandPlacetteStep %>% filter(SubModuleID==5)

      Rec$predPi<-(exp(rec_pi(Rec,t,st_tot0,ntrt,t0_aj_,type_pe_Plac,Iterj,Para.rec_n)+RandomRec$RandomPlac))/
        (1+exp(rec_pi(Rec,t,st_tot0,ntrt,t0_aj_,type_pe_Plac,Iterj,Para.rec_n)+RandomRec$RandomPlac))

      Rec$predDelta<-exp(rec_delta(Rec,st_tot0,type_pe_Plac,ntrt,t0_aj_,Iterj,Para.rec_n))

      Rec$predLambda<-exp(rec_lambda(Rec,type_pe_Plac,st_tot0,t,Iterj,Para.rec_n))




      RecBase<-map_dfr(seq_len(151), ~Rec) %>% #dataframe de base pour le recrutement
        arrange(GrEspece) %>%
        mutate(m=rep(c(0:150),10))


      RecTot<-RecBase %>%
        mutate(mu=rep(c(0,rep(1,150)),10)) %>%
        mutate(Pr=predPi^(1-mu)*((1-predPi)*(exp(-predDelta*m^predLambda)-
                                               exp(-predDelta*(m+1)^predLambda))/exp(-predDelta))^mu) %>%
        group_by(GrEspece) %>%
        mutate(CumPr=cumsum(Pr))

      suppressMessages(
        RecSelect<-RecTot%>%
          group_by(GrEspece) %>%
          mutate(Alea=runif(1)) %>%
          mutate(Valeur=CumPr >Alea) %>%
          filter(Valeur=="TRUE") %>%
          summarise(NbRecrues=first(m))%>%
          filter(NbRecrues!=0))
    }


    Plac<-Plac %>%
      select(Placette,Annee,ArbreID,NoArbre,Nombre,GrEspece,Espece,Etat1,DHPcm1,
             vigu1,prod1,ABCD,Iter) ######Mise en forme de Plac pour fusion avec recrues

    if (nrow(Plac)==0){

      break
    }

    if (nrow(Plac[which(Plac$Etat1=="vivant"),])==0){

      break
    }######Arrete simulation et écris les valeurs dans un fichier log


    if (nrow(RecSelect)>=1){  ######Si présence de recrues sinon on saute cette partie de code

      suppressMessages(
        RecSelect<-RecSelect %>%
          group_by(GrEspece) %>%
          slice(rep(1:n(), first(NbRecrues))) %>%
          ungroup %>%
          mutate(ArbreID=c(1:n())+last(Plac$ArbreID)))


      #############DHP Recrues
      RandomRecDhp<-RandPlacetteStep %>% filter(SubModuleID==6)

      varRecDhp<-CovParms$ParameterEstimate[which(CovParms$CovParm=="sigma2_res")]

      theta<-CovParms$ParameterEstimate[which(CovParms$CovParm=="theta")]

      RandomRecProd<-RandPlacetteStep %>% filter(SubModuleID==8)

      # on applique la fonction du DHP des recrues

      RecSelect<-RecSelect %>%
        mutate(pred_dhp=rec_dhp(RecSelect,st_tot0,dens_tot0,t,ntrt,Iterj,Para.rec_dhp),
               eijk=rnorm(n(),mean=0,sd=sqrt(varRecDhp*pred_dhp^theta)),
               DHPcm1=((pred_dhp+RandomRecDhp$RandomPlac+RandomRecDhp$RandomStep+eijk)^2+90)/10,
               DHPcm1=ifelse(DHPcm1>=9.1,DHPcm1,9.1)) %>%
        select(ArbreID,GrEspece,DHPcm1)
      RecSelect$DHPcm1<-RecSelect$DHPcm1[,1]

      ########################Vigueur des recrues#####################################
      RandomRecVig<-RandPlacetteStep %>% filter(SubModuleID==7)

      RecSelect<-RecSelect %>%
        mutate(pred_vig=rec_vig(RecSelect,latitude,Iterj,Para.rec_vig),
               AleaVig=runif(n()),
               predVigRand=exp(pred_vig+RandomRecVig$RandomPlac+RandomRecVig$RandomStep)/
                 (1+exp(pred_vig+RandomRecVig$RandomPlac+RandomRecVig$RandomStep)),
               vigu1=ifelse(AleaVig<=predVigRand,"ViG","NONVIG")) %>%
        select(ArbreID,GrEspece,DHPcm1,vigu1)
      RecSelect$vigu1<-RecSelect$vigu1[,1]


      #######################Produit des recrues#####################################
      RandomRecProd<-RandPlacetteStep %>% filter(SubModuleID==8)

      RecSelect<-RecSelect %>%
        mutate(pred_prod=rec_prod(RecSelect,type_pe_Plac,rid1,Iterj,Para.rec_prod),
               AleaProd=runif(n()),
               predProdRand=exp(pred_prod+RandomRecProd$RandomPlac+RandomRecProd$RandomStep)/
                 (1+exp(pred_prod+RandomRecProd$RandomPlac+RandomRecProd$RandomStep)),
               prod1=ifelse(GrEspece %in% c("SAB","RES","EPX"),"resineux",
                            ifelse(GrEspece=="AUT"|(vigu1=="NONVIG"),"pate",
                                   ifelse(AleaProd<=predProdRand,"sciage","pate")))) %>%
        select(ArbreID,GrEspece,DHPcm1,vigu1,prod1)

      #########Ajout des résidus des recrues

      # Résidus de l'arbre
      if (k < Horizon){
        ResidusRec<-matrix(0,nrow=nrow(RecSelect),ncol=Horizon+1)
        ResidusRec[,1]<-RecSelect$ArbreID

        for (i in (k+1):Horizon){
          ResidusRec[,i+1]<-rnorm(nrow(ResidusRec),
                                  mean=0,sqrt(Residual*Gamma*(Rho^(i-k-1))))
        }
        # créer et remplir la matrice de var-cov ARMA(1,1)
        # varcov <- expand.grid(i=1:(Horizon-k), j=1:(Horizon-k))
        # varcov <- matrix(f(varcov$i, varcov$j, Residual, Gamma, Rho), nrow=Horizon-k)
        #
        # ResidusRec <- rockchalk::mvrnorm(n=nrow(RecSelect), mu=rep(0,Horizon-k), Sigma = varcov, empirical=T)
        # ResidusRec0<-matrix(0,nrow=nrow(RecSelect),ncol=k) # colonnes de 0 pour les steps passés
        # ResidusRec <- cbind(RecSelect$ArbreID, ResidusRec0, ResidusRec)

         Residus<-rbind(Residus,ResidusRec)


      }
      ########Ajout des recrues au fichier des Placettes
      RecSelect<-RecSelect %>%
        mutate(Placette=Placette,Annee=AnneeDep+k*t,NoArbre=NA,
               Espece=ifelse(GrEspece %in% c("BOJ","ERR","ERS","HEG","SAB"),GrEspece,NA),
               Etat1="recrue", Nombre=Sup_PE/0.25, Iter=PlacOri$Iter[1])

      Plac<-Plac %>%
        bind_rows(RecSelect)

    }  #######Fin du recrutement


    # on ajoute les donnees du pas de simulation en cours aux autres pas de simulation pour la placette

    if (k < Horizon){
      m<-which(Plac$Etat1!="mort",arr.ind = TRUE)
      Residus<-matrix(Residus[m,],nrow=length(m), ncol=Horizon+1)  #####Enlève les arbres morts des résidus pour la prochaine step de croissance
    }


    Plac<-Plac%>%
      rename(Etat=Etat1,DHPcm=DHPcm1,vigu0=vigu1,prod0=prod1)


    if (RecruesGaules==1){
      Plac$Nb_Gaules_Ha<-sum(Nb_Gaules_Ha$Nb_Gaules_Ess_Ha)
      Plac$Nb_Gaules_ERS<-Nb_Gaules_Ha$Nb_Gaules_Ess_Ha[which(Nb_Gaules_Ha$GrEspece=="ERS")]
      Plac$Nb_Gaules_HEG<-Nb_Gaules_Ha$Nb_Gaules_Ess_Ha[which(Nb_Gaules_Ha$GrEspece=="HEG")]
      Plac$Nb_Gaules_BOJ<-Nb_Gaules_Ha$Nb_Gaules_Ess_Ha[which(Nb_Gaules_Ha$GrEspece=="BOJ")]
      Plac$Nb_Gaules_SAB<-Nb_Gaules_Ha$Nb_Gaules_Ess_Ha[which(Nb_Gaules_Ha$GrEspece=="SAB")]
      Plac$Nb_Gaules_68_Ha<-sum(exp(Nb_Gaules_Ha$lnNb_Gaules_68_Ess_Ha)-1)
      Plac$Nb_Gaules_68_ERS<-exp(Nb_Gaules_Ha$lnNb_Gaules_68_Ess_Ha[which(Nb_Gaules_Ha$GrEspece=="ERS")])-1
      Plac$Nb_Gaules_68_HEG<-exp(Nb_Gaules_Ha$lnNb_Gaules_68_Ess_Ha[which(Nb_Gaules_Ha$GrEspece=="HEG")])-1
      Plac$Nb_Gaules_68_BOJ<-exp(Nb_Gaules_Ha$lnNb_Gaules_68_Ess_Ha[which(Nb_Gaules_Ha$GrEspece=="BOJ")])-1
      Plac$Nb_Gaules_68_SAB<-exp(Nb_Gaules_Ha$lnNb_Gaules_68_Ess_Ha[which(Nb_Gaules_Ha$GrEspece=="SAB")])-1

    }

    outputTot <- bind_rows(outputTot, Plac)


  }  # fin de la boucle des simulations


  ParaConvVigMSCRlist=list(Para)

  if (!is.null(outputTot)){


    if (any(is.na(outputInitial$MSCR))){

      outputInitial<-outputInitial %>% select(-MSCR)

      suppressMessages(
        outputTot <- outputTot %>%
          mutate(Residuel=0) %>%
          bind_rows(outputInitial,.))

      MSCR<-outputTot %>%
        group_by(Placette,Annee,ArbreID,Residuel,Iter) %>%
        nest() %>%
        mutate(MSCR = mapply(AttribMSCR,data,MoreArgs=ParaConvVigMSCRlist)) %>%
        unnest(MSCR) %>%
        select(-data)

      suppressMessages(
        outputTot <- outputTot %>%
          left_join(MSCR))
    }else{

      MSCR<-outputTot %>%
        group_by(Placette,Annee,ArbreID,Iter) %>%
        nest() %>%
        mutate(MSCR = mapply(AttribMSCR,data,MoreArgs=ParaConvVigMSCRlist)) %>%
        unnest(MSCR) %>%
        select(-data)

      suppressMessages(
        outputTot <- outputTot %>%
          left_join(MSCR) %>%
          mutate(Residuel=0) %>%
          bind_rows(outputInitial,.))

    }

  }else{

    if (any(is.na(outputInitial$MSCR))){

      outputTot <- outputInitial %>% select(-MSCR)

      MSCR<-outputTot %>%
        group_by(Placette,Annee,ArbreID,Residuel,Iter) %>%
        nest() %>%
        mutate(MSCR = mapply(AttribMSCR,data,MoreArgs=ParaConvVigMSCRlist)) %>%
        unnest(MSCR) %>%
        select(-data)

      suppressMessages(
        outputTot <- outputTot %>%
          left_join(MSCR))
    }else{

      outputTot <- outputInitial

    }

  }

  return(outputTot)

}

