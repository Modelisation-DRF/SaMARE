



#' la simultation a l'échelle de placette
#'
#' @param Random Résultat de la fonction RandomPlacStep pour obtenir un effet aléatoire.
#' @param RandomGaules  Résultat de la fonction RandPlacStepGaules pour obtenir un effet aléatoire pour les gaules .
#' @param Data    Donne des arbres à simuler.
#' @param Gaules  Donne des Gaules
#' @param ListeIter     l'itération choisie
#' @param AnneeDep l'année de depart
#' @param Horizon Nombre de cycles de 5 ans souhaité
#' @param RecruesGaules  Liste des nouvelles recrues
#' @param CovParms Paramètre pour un effet aléatoire
#' @param CovParmsGaules Paramètre pour un effet aléatoire pour gaules
#' @param Para
#' @param ParaGaules
#' @param Omega
#' @param OmegaGaules
#' @return
#' @examples


SaMARE<- function(Random, RandomGaules, Data, Gaules, ListeIter, AnneeDep, Horizon, 
                  RecruesGaules,CovParms,CovParmsGaules,Para,ParaGaules,Omega,OmegaGaules){
  
  t<-5
  
  ################## convertion MSCR #################################
  Para_Convertion_MSCR <- Para  %>%  filter(SubModuleID==17)
  
  Para.ConvMSCRVig<-Para %>% 
    filter(SubModuleID==17)  #parametres pour vigueur
  Para.ConvMSCRProd1024<-Para %>% 
    filter(SubModuleID==18)  #parametres pour produits<24 cm
  Para.ConvMSCRProd24<-Para %>% 
    filter(SubModuleID==19)  #parametre pour produits>24cm
  
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
  
  
  ########################Parametres par modules
  # 1;mort
  # 2;accroissement
  # 3;vigeur
  # 4;produit
  # 5;recrutement_n
  # 6;recrutement_dbh
  # 7;recrutement_vigor
  # 8;recrutement_product
  # 9;MSCR n'est pas actuellement programmé
  Para.mort<-ParaOmega(ModuleID = 1,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.acc<-ParaOmega(ModuleID = 2,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.vig<-ParaOmega(ModuleID = 3,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.prod<-ParaOmega(ModuleID = 4,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_n<-ParaOmega(ModuleID = 5,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_dhp<-ParaOmega(ModuleID = 6,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_vig<-ParaOmega(ModuleID = 7,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.rec_prod<-ParaOmega(ModuleID = 8,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  #Para.mscr<-ParaOmega(ModuleID = 9,ParaOri=Para,ParaIter=ParaTot,Omega=Omega,NbIter=NbIter)
  
  #########Gaules
  # 10;recrutement
  # 11;nombre total de gaules
  # 12;ratio especes nombre de gaules
  # 13;nombre de gaules de 6 et 8 cm ERS
  # 14;nombre de gaules de 6 et 8 cm HEG
  # 15;nombre de gaules de 6 et 8 cm BOJ
  # 16;nombre de gaules de 6 et 8 cm SAB
  Para.rec_gaules<-ParaOmega(ModuleID = 10,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.nb_gaules<-ParaOmega(ModuleID = 11,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.ratio_gaules<-ParaOmega(ModuleID = 12,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_ERS<-ParaOmega(ModuleID = 13,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_HEG<-ParaOmega(ModuleID = 14,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_BOJ<-ParaOmega(ModuleID = 15,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  Para.68_SAB<-ParaOmega(ModuleID = 16,ParaOri=ParaGaules,ParaIter=ParaTotGaules,Omega=OmegaGaules,NbIter=1) %>% mutate(Iter=PlacOri$Iter[1])
  
  
  
  
  #Création de la placette de simulation
  Plac<-PlacOri %>%
    filter(Etat %in% c(10,11,12,40,42,30,32,50,52,70,71,72)) %>% 
    mutate(Etat="vivant",ArbreID=seq(1:n())) %>% 
    select(Placette,Annee,ArbreID,NoArbre,GrEspece,Espece,Etat,
           DHPcm,Nombre,Vigueur,Iter,MSCR) 
  # microbenchmark(
  #   "Hugues" = {
  #   Plac<-PlacOri %>%
  #         filter(Etat %in% c(10,11,12,40,42,30,32,50,52,70,71,72)) %>% 
  #         mutate(Etat="vivant",ArbreID=seq(1:n())) %>% 
  #         select(Placette,Annee,ArbreID,NoArbre,GrEspece,Espece,Etat,
  #                 DHPcm,Nombre,Vigueur,Iter) 
  #   }, 
  #   "Emmanuel" = {
  #   PlacOri = data.table(PlacOri)
  #   Plac1 <- PlacOri[Etat %in% c(10,11,12,40,42,30,32,50,52,70,71,72),
  #                   .(Placette, Annee, ArbreID = 1:.N, NoArbre, GrEspece, Espece,
  #                     Etat = "vivant", DHPcm, Nombre, Vigueur, Iter)]
  #   })
  # Unit: milliseconds
  # expr    min      lq      mean   median       uq     max neval
  # Hugues 9.2316 9.94425 11.780699 10.32605 11.05695 25.2064   100
  # Emmanuel 2.7998 2.95940  3.574109  3.12205  3.42655 11.5763   100
  
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
  trt<-ifelse(is.na(PlacOri$Annee_Coupe[1])==TRUE,"TEM",
              ifelse(AnneeDep-PlacOri$Annee_Coupe[1]<5 &
                       (sum((Plac$DHPcm/200)^2*3.1416*Plac$Nombre)/Sup_PE)>26,"TEM","CP"))
  
  #Nombre de traitements
  ntrt=ifelse(trt=="CP",PlacOri$ntrt[1],0)
  
  #Type de placette
  type_pe_Plac<-ifelse(PlacOri$Sup_PE[1]==0.04,"type0",
                       ifelse(PlacOri$Sup_PE[1]>=0.25 & PlacOri$Sup_PE[1]<=0.5,"type1","type2"))
  # microbenchmark(
  #   "Hugues"={    type_pe_Plac<-ifelse(PlacOri$Sup_PE[1]==0.04,"type0",
  #                                      ifelse(PlacOri$Sup_PE[1]>=0.25 & PlacOri$Sup_PE[1]<=0.5,"type1","type2"))},
  #   "Emmanuel"={
  #     sup_PE_1 <- PlacOri$Sup_PE[1]
  #     type_pe_Plac <- if (sup_PE_1 == 0.04) {
  #       "type0"
  #     } else if (sup_PE_1 >= 0.25 & sup_PE_1 <= 0.5) {
  #       "type1"
  #     } else {
  #       "type2"
  #     }
  #   }
  #   
  # )
  # Unit: microseconds
  # expr     min  lq  mean median   uq  max neval
  # Hugues   6.7 7.1 8.089    7.4 7.85 25.2   100
  # Emmanuel 2.1 2.3 2.896    2.5 2.70 28.4   100
  
  
  #Variables de classification écologiques
  latitude<-PlacOri$Latitude[1]
  
  longitude<-PlacOri$Longitude[1]
  
  altitude<-PlacOri$Altitude[1]
  
  pente<-PlacOri$Pente[1]
  
  dom<-substr(PlacOri$Reg_Eco[1],1,1) %>% ifelse(!. %in% c("2","3","4"),"4",. )##valeur de 4 attribué aux domaines pas dans la liste d'effets
  
  reg<-PlacOri$Reg_Eco[1]
  
  rid1<-ifelse(reg %in% c("2a","2b","2c"),"2o",ifelse(reg %in% c("4a","4b","4c"),"4o",
                                                      ifelse(reg %in% c("4d","4e","4f","4g","4h"),"4e",reg))) %>% 
    ifelse(!. %in% c("2o","3a","3b","3c","3d","4e","4o"),NA,.)
  teco<-PlacOri$Type_Eco[1]
  
  vegp<-substr(PlacOri$Type_Eco[1],1,3)
  
  # Variables climatiques de la placette
  prec <- PlacOri$Ptot[1]
  
  temp <- PlacOri$Tmoy[1]
  
  grwd<-  PlacOri$GrwDays[1]
  
  ##################Effets Aleatoires  de la placette#############################
  
  RandPlac<-Random %>% filter(Placette==PlacOri$Placette[1] & Iter==PlacOri$Iter[1])
  # Random = data.table(Random)
  # microbenchmark(
  #   "NDecla" = {RandPlac<-Random %>% filter(Placette==PlacOri$Placette[1] & Iter==PlacOri$Iter[1])},
  #   "Decla" = {RandPlac<-Random %>% filter(Placette==Placette & Iter==Iterj)},
  #   "DTNDecla" = {RandPlac<-Random[Placette==PlacOri$Placette[1] & Iter==PlacOri$Iter[1]] },
  #   "DTDecla" = {RandPlac<-Random[Placette==Placette & Iter==Iterj] }
  # )
  # Unit: milliseconds
  # expr    min      lq     mean  median      uq     max neval
  # NDecla 1.7036 1.90685 2.450274 1.95635 2.26665 13.2997   100
  # Decla 1.8578 2.08915 2.383974 2.18060 2.36830  5.8746   100
  # DTNDecla 1.8656 2.06750 2.496483 2.17975 2.34115  6.0176   100
  # DTDecla 1.1907 1.31215 1.680191 1.37555 1.48625 11.9961   100  
  
  ################## Effets Aleatoires placette Gaules############################
  
  RandomPlacGaules<-RandomGaules %>% filter(Placette==PlacOri$Placette[1] & Iter==PlacOri$Iter[1])
  
  ######################## Résidus de l'arbre####################################
  
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
        mutate(Annee=AnneeDep+k*t)


      if (any(is.na(Plac$Vigueur) & !is.na(Plac$MSCR))){
        predVig0<-ConvMSCRVig(Plac,Para.ConvMSCRVig)

        Plac<-Plac %>%
          mutate(vig0=predVig0,Alea=runif(n()),
                 vig0=ifelse(vig0>=Alea,"vig","NONVIG")) %>%
          select(-Alea)
        
        if (any(Plac$DHPcm <= 23.0)){

          predprod0<-ConvMSCRProd1024(Plac,Para.ConvMSCRProd1024)
        }else {
          predprod0<-ConvMSCRProd24(Plac,Para.ConvMSCRProd24)
        }

        Plac<-Plac %>%
          mutate(prod0=predprod0,Alea=runif(n()),
                 prod0=ifelse(GrEspece %in% c("EPX","RES","SAB"),"resineux",
                              ifelse(prod0>=Alea,"sciage","pate"))) %>%
          select(-Alea)

      }else if (any(is.na(Plac$Vigueur) & is.na(Plac$MSCR))){
        Plac$Vigueur =2
      }


      Plac <- Plac %>%
         #Fixe à vigoureux sans sciage les arbres sans vigueur initiale majoritairement des Non commerciaux
        mutate(vigu0=ifelse(Vigueur %in% c(1,2,5),"ViG","NONVIG")) %>% 
        mutate(prod0=ifelse(Vigueur %in% c(1,3),"sciage",
                            ifelse(Vigueur %in% c(2,4),"pate","resineux"))) %>% 
        select(-Vigueur)
      
      # Initialisation du fichier qui contiendra les résultats de simulation de la placette
      #  et les données au début de la simulation
      
      outputTot<-Plac %>%
        mutate(Annee=AnneeDep) %>% 
        select(Placette, Annee, ArbreID, NoArbre, Nombre, GrEspece,
               Espece, Etat, DHPcm, vigu0, prod0, Iter)
      
      # Mise en forme des statistiques de gaules qui seront mises à jour par la suite
      
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
      
      
      # outputTot = data.table(outputTot)
      # plac <- outputTot[Annee == AnneeDep+(k-1)*t & Etat!="mort", .(Annee=AnneeDep+k*t, Etat="vivant" )]
      # 
    }
    
    #Temps depuis coupe
    t0_aj_<-Plac$Annee[1]-t0-4.9 ######on ajuste pour que lorsque la step débute immédiatement après coupe t0_aj a une valeur de 0.1
    
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
    pred<-mort(Mort,trt,temp,type_pe_Plac,fact_red,t,Iterj,Para.mort)
    
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
    
    pred<-prod(Prod,type_pe_Plac,rid1,Iterj,Para.prod)
    
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
      
      Rec$predPi<-(exp(rec_pi(Rec,t,st_tot0,ntrt,t0_aj_,type_pe_Plac,Iterj)+RandomRec$RandomPlac))/
        (1+exp(rec_pi(Rec,t,st_tot0,ntrt,t0_aj_,type_pe_Plac,Iterj)+RandomRec$RandomPlac))
      
      Rec$predDelta<-exp(rec_delta(Rec,st_tot0,type_pe_Plac,ntrt,t0_aj_,Iterj))
      
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
             vigu1,prod1,Iter) ######Mise en forme de Plac pour fusion avec recrues
    
    
    
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
               DHPcm1=((pred_dhp+RandomRecDhp$RandomPlac+RandomRecDhp$RandomStep+eijk)^2+90)/10) %>% 
        select(ArbreID,GrEspece,DHPcm1)
      
      
      ########################Vigueur des recrues#####################################
      RandomRecVig<-RandPlacetteStep %>% filter(SubModuleID==7)
      
      RecSelect<-RecSelect %>% 
        mutate(pred_vig=rec_vig(RecSelect,latitude,Iterj,Para.rec_vig),
               AleaVig=runif(n()),
               predVigRand=exp(pred_vig+RandomRecVig$RandomPlac+RandomRecVig$RandomStep)/
                 (1+exp(pred_vig+RandomRecVig$RandomPlac+RandomRecVig$RandomStep)),
               vigu1=ifelse(AleaVig<=predVigRand,"ViG","NONVIG")) %>% 
        select(ArbreID,GrEspece,DHPcm1,vigu1)
      
      
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
        Residus<-rbind(Residus,ResidusRec)
        
        
      }
      
      ########Ajout des recrues au fichier des Placettes
      RecSelect<-RecSelect %>% 
        mutate(Placette=Placette,Annee=AnneeDep+k*t,NoArbre=NA,
               Espece=ifelse(GrEspece %in% c("BOJ","ERR","ERS","HEG","SAB"),GrEspece,NA), 
               Etat1="recrue", Nombre=Sup_PE/0.25, Iter=PlacOri$Iter[1])
      
      Plac<-Plac %>%
        rbind(RecSelect) 
      
    }  #######Fin du recrutement
    
    
    # on ajoute les donnees du pas de simulation en cours aux autres pas de simulation pour la placette
    
    if (k < Horizon){
      m<-which(Plac$Etat1=="mort",arr.ind = TRUE)
      
      if (length(m)>0){
        Residus<-Residus[-m,]  #####Enlève les arbres morts des résidus pour la prochaine step de croissance
      }
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
  
  outputTot <- outputTot %>% 
               mutate(Residuel= ifelse(trt=="cp",1,0))
               
  return(outputTot)  
  
  
}
