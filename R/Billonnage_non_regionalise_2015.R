



ABCD_DHP215<- function (data, type){
  select=dplyr::select


  if (type == "ABCD"){

      CovParaPetro_abcd<-CovParaPetro_abcd %>%  filter(Cov>0)

    Vol_Billon<-ParaPetro_abcd %>%
      filter(Module=="Vol") %>%
      mutate(betaVol=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaVol, names_prefix = "Vol")

    Pres_Billon <- ParaPetro_abcd %>%
      filter(Module=="Pres") %>%
      mutate(betaPres=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaPres, names_prefix = "Pres") %>%
      full_join(Vol_Billon, by=c("Essence_billon","Produit", "QualiteABCD")) %>%
      mutate(Presdhpcm2_classepetro=ifelse(is.na(Presdhpcm2_classepetro)==TRUE,0,Presdhpcm2_classepetro))
    names(Pres_Billon)

    par_qual <- Pres_Billon %>%
      filter(!is.na(QualiteABCD)) %>%
      select(c(Essence_billon, Produit, QualiteABCD, Presclassepetro_abcd, Volclassepetro_abcd))

    par_num <- Pres_Billon %>%
      filter(is.na(QualiteABCD)) %>%
      select(c(Essence_billon, Produit, Presdhpcm_classepetro, Presdhpcm2_classepetro, Voldhpcm_classepetro))

    suppressMessages(
      ListeCorresPetro<-ListeCorresPetro%>% #certaines classes sont fusionnées
        filter(Equation=="ABCD"))

    ##########################################################
    ###################Calcul des volumes de billons##########
    ##########################################################


    #mise en forme de données
    data <-data %>%
      left_join(ListeCorresPetro, by = c("Espece"="Essence_billon", "ABCD"="VAL_INIT"))

    Sim_biol_2015 <- data %>%
      mutate(Essence_billon=ifelse(Espece=="CHR", "CHX", Espece), QualiteABCD=VAL_FIN,F1=NA) %>% #ajout
      filter(Essence_billon %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHX")) %>%
      #left_join(par_eco, by=c("Essence_billon"), relationship="many-to-many") %>%
      left_join(par_qual, by=c("Essence_billon", "QualiteABCD"), relationship="many-to-many") %>%
      left_join(par_num, by=c("Essence_billon", "Produit"), relationship="many-to-many") %>%
      inner_join(CovParaPetro_abcd, by=c("Essence_billon", "Produit")) %>%
      #left_join(CovParaPetro_abcd, by=c("Essence_billon", "Produit"), relationship="many-to-many") %>% si on veut conserver toutes les donnes, meme ou petro ne s'applique pas
      mutate(Cov=ifelse(is.na(Cov)==TRUE,0,Cov)) %>%
      mutate(BetaPres= Presclassepetro_abcd+
               DHPcm*Presdhpcm_classepetro+
               DHPcm^2*Presdhpcm2_classepetro,
             BetaVol=  Volclassepetro_abcd+
               DHPcm*Voldhpcm_classepetro,
             Pres=exp(BetaPres)/(1+exp(BetaPres)),
             Vol=exp(BetaVol+0.5*Cov),
             VolBillonM3=Pres*Vol) %>%
      mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE) %>%
      select(Annee,Residuel,ArbreID,NoArbre,Placette,Nombre,GrEspece,Espece,
             Etat,DHPcm,Iter,MSCR,hauteur_pred,vol_dm3,Produit,VolBillonM3,Stm2ha,Sup_PE) %>%
      pivot_wider(names_from = Produit, values_from = VolBillonM3)

    if(!"F1" %in% names(Sim_biol_2015)){
      Sim_biol_2015 <-Sim_biol_2015 %>%
        mutate(F1=NA)
    }

  }else if (type == "1234"){



      CovParaPetro_1234<-CovParaPetro_1234 %>%
        filter(Cov>0)

    Vol_Billon <-ParaPetro_1234_ %>%
      filter(Module=="Vol") %>%
      mutate(betaVol=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaVol, names_prefix = "Vol") %>%
      #mutate_all(~ ifelse(is.na(.) | . == "NULL", 0, .))
      mutate_at(vars(Volclassepetro_vig, Volclassepetro_prod, Voldhpcm_classepetro), ~ ifelse(is.na(.) | . == "NULL", 0, .))  #ajout

    Pres_Billon <- ParaPetro_1234_ %>%
      filter(Module=="Pres") %>%
      mutate(betaPres=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaPres, names_prefix = "Pres") %>%
      mutate_at(vars(Presclassepetro_vig, Presclassepetro_prod, Presdhpcm_classepetro, Presdhpcm2_classepetro),
                ~ ifelse(is.na(.) | . == "NULL", 0, .)) %>%   #ajout
      full_join(Vol_Billon, by=c("Essence_billon","Produit", "Vigueur1234"))

    names(Pres_Billon)

    par_vig <- Pres_Billon %>%
      filter(!is.na(Vigueur1234) & Vigueur1234 %in% c("vigour", "nonvig")) %>%
      select(c(Essence_billon, Produit, Vigueur1234, Presclassepetro_vig, Volclassepetro_vig)) %>%
      mutate(Vig1234=ifelse(Vigueur1234=="vigour","ViG","NONVIG")) %>%
      mutate(Volclassepetro_vig = ifelse(is.na(Volclassepetro_vig) |  Volclassepetro_vig == "NULL", 0, Volclassepetro_vig)) %>%
      select(-Vigueur1234)
      #ajout

    par_prod <- Pres_Billon %>%
      filter(!is.na(Vigueur1234) & Vigueur1234 %in% c("sciage", "pate")) %>%
      select(c(Essence_billon, Produit, Vigueur1234, Presclassepetro_prod, Volclassepetro_prod)) %>%
      rename(Prod1234=Vigueur1234)%>%
      mutate(Presclassepetro_prod = ifelse(is.na(Presclassepetro_prod) | Presclassepetro_prod== "NULL", 0, Presclassepetro_prod),
             Volclassepetro_prod = ifelse(is.na(Volclassepetro_prod) | Volclassepetro_prod== "NULL", 0, Volclassepetro_prod)) #ajout

    par_num <- Pres_Billon %>%
      filter(is.na(Vigueur1234)) %>%
      select(c(Essence_billon, Produit, Presdhpcm_classepetro, Presdhpcm2_classepetro, Voldhpcm_classepetro)) %>%
      mutate(Presdhpcm_classepetro=ifelse(is.na(Presdhpcm_classepetro)==TRUE,0,Presdhpcm_classepetro),
             Presdhpcm2_classepetro=ifelse(is.na(Presdhpcm2_classepetro)==TRUE,0,Presdhpcm2_classepetro))

    ###################Calcul des volumes de billons##########

    #mise en forme de données


    Sim_biol_2015 <- data %>%
      mutate(Essence_billon=ifelse(Espece=="CHR", "CHX", Espece),F1=NA) %>% #ajout
      filter(Essence_billon %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHX")) %>%
      rename(Vig1234 = vigu0,
             Prod1234 = prod0) %>%
      left_join(par_vig, by=c("Essence_billon", "Vig1234"), relationship="many-to-many") %>%
      left_join(par_prod, by=c("Essence_billon", "Prod1234", "Produit"), relationship="many-to-many") %>%
      left_join(par_num, by=c("Essence_billon", "Produit"), relationship="many-to-many") %>%
      inner_join(CovParaPetro_1234, by=c("Essence_billon", "Produit"),relationship="many-to-many") %>%
      mutate(Cov=ifelse(is.na(Cov)==TRUE,0,Cov),
             Presclassepetro_prod=ifelse(is.na(Presclassepetro_prod)==TRUE,0,Presclassepetro_prod),
             Volclassepetro_prod=ifelse(is.na(Volclassepetro_prod)==TRUE,0,Volclassepetro_prod))%>%
      mutate(BetaPres= Presclassepetro_vig+
               Presclassepetro_prod+
               DHPcm*Presdhpcm_classepetro+
               DHPcm^2*Presdhpcm2_classepetro,
             BetaVol=  Volclassepetro_vig+
               Volclassepetro_prod+
               DHPcm*Voldhpcm_classepetro,
             Pres=exp(BetaPres)/(1+exp(BetaPres)),
             Vol=exp(BetaVol+0.5*Cov),
             VolBillonM3=Pres*Vol) %>%
      mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE) %>%
      select(Annee,Residuel,ArbreID,NoArbre,Placette,Nombre,GrEspece,Espece,
             Etat,DHPcm,Iter,MSCR,hauteur_pred,vol_dm3,Produit,VolBillonM3,Stm2ha,Sup_PE) %>%
      pivot_wider(names_from = Produit, values_from = VolBillonM3)

    if(!"F1" %in% names(Sim_biol_2015)){
      Sim_biol_2015 <-Sim_biol_2015 %>%
        mutate(F1=NA)
    }

  }else if (type == "MSCR"){




      CovParaPetro_mscr<-CovParaPetro_mscr%>%
        filter(Cov>0)

    Vol_Billon<-ParaPetro_mscr %>%
      filter(Module=="Vol") %>%
      mutate(betaVol=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaVol, names_prefix = "Vol")

    Pres_Billon <- ParaPetro_mscr %>%
      filter(Module=="Pres") %>%
      mutate(betaPres=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaPres, names_prefix = "Pres") %>%
      full_join(Vol_Billon, by=c("Essence_billon","Produit", "PrioriteMSCR")) %>%
      mutate(Presdhpcm_classepetro=ifelse(is.na(Presdhpcm_classepetro)==TRUE,0,Presdhpcm_classepetro),
             Presdhpcm2_classepetro=ifelse(is.na(Presdhpcm2_classepetro)==TRUE,0,Presdhpcm2_classepetro))
    names(Pres_Billon)

    par_qual <- Pres_Billon %>%
      filter(!is.na(PrioriteMSCR)) %>%
      select(c(Essence_billon, Produit, PrioriteMSCR, Presclassepetro_mscr, Volclassepetro_mscr))

    par_num <- Pres_Billon %>%
      filter(is.na(PrioriteMSCR)) %>%
      select(c(Essence_billon, Produit, Presdhpcm_classepetro, Presdhpcm2_classepetro, Voldhpcm_classepetro))


      ListeCorresPetro<-ListeCorresPetro %>% #certaines classes sont fusionnées
        filter(Equation=="MSCR")

    ###################Calcul des volumes de billons##########


    #mise en forme de données
    data <-data%>%
      left_join(ListeCorresPetro, by = c("Espece"="Essence_billon", "MSCR"="VAL_INIT"))

    Sim_biol_2015 <- data %>%
      mutate(Essence_billon=ifelse(Espece=="CHR", "CHX", Espece),PrioriteMSCR=VAL_FIN,F1=NA) %>% #ajout
      filter(Essence_billon %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHX")) %>%
      left_join(par_qual, by=c("Essence_billon", "PrioriteMSCR"), relationship="many-to-many") %>%
      left_join(par_num, by=c("Essence_billon", "Produit"), relationship="many-to-many") %>%
      inner_join(CovParaPetro_mscr, by=c("Essence_billon", "Produit")) %>%
      mutate(Cov=ifelse(is.na(Cov)==TRUE,0,Cov)) %>%
      mutate(BetaPres= Presclassepetro_mscr+
               DHPcm*Presdhpcm_classepetro+
               DHPcm^2*Presdhpcm2_classepetro,
             BetaVol=  Volclassepetro_mscr+
               DHPcm*Voldhpcm_classepetro,
             Pres=exp(BetaPres)/(1+exp(BetaPres)),
             Vol=exp(BetaVol+0.5*Cov),
             VolBillonM3=Pres*Vol)%>%
      mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE) %>%
      select(Annee,Residuel,ArbreID,NoArbre,Placette,Nombre,GrEspece,Espece,
             Etat,DHPcm,Iter,MSCR,hauteur_pred,vol_dm3,Produit,VolBillonM3,Stm2ha,Sup_PE) %>%
      pivot_wider(names_from = Produit, values_from = VolBillonM3)

    if(!"F1" %in% names(Sim_biol_2015)){
      Sim_biol_2015 <-Sim_biol_2015 %>%
        mutate(F1=NA)
    }


  }else if (type == "DHP"){

    Para<-ParaPetroFinal


    CovParms<-CovParmPetro %>%
      filter(Cov>0)



    Vol_Billon<-Para %>%
      filter(Module=="Vol") %>%
      mutate(betaVol=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaVol, names_prefix = "Vol")

    Pres_Billon <- Para %>%
      filter(Module=="Pres") %>%
      mutate(betaPres=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaPres, names_prefix = "Pres") %>%
      mutate(Presdhpcm2=ifelse(is.na(Presdhpcm2)==TRUE,0,Presdhpcm2),
             Presdhpcm2_classepetro=ifelse(is.na(Presdhpcm2_classepetro)==TRUE,0,Presdhpcm2_classepetro)) %>%
      full_join(Vol_Billon, by=c("Essence_billon","Produit"))

    #############################################################
    ###################Calcul des volumes de billons###########
    ##########################################################

    Sim_biol_2015<-data %>%
      mutate(Essence_billon=ifelse(is.na(Espece)==TRUE, GrEspece, Espece),F1=NA) %>%
      filter(Essence_billon %in% c("BOJ","ERS","BOP","ERR","CHX","HEG")) %>%
      left_join(Pres_Billon, by=c("Essence_billon"), relationship="many-to-many") %>%
      inner_join(CovParms, by=c("Essence_billon", "Produit")) %>%
      mutate(Cov=ifelse(is.na(Cov)==TRUE,0,Cov)) %>%
      mutate(BetaPres=Presclassepetro+DHPcm*Presdhpcm+DHPcm*Presdhpcm_classepetro+
               DHPcm^2*Presdhpcm2+DHPcm^2*Presdhpcm2_classepetro,
             BetaVol=Volclassepetro+DHPcm*Voldhpcm+DHPcm*Voldhpcm_classepetro,
             Pres=exp(BetaPres)/(1+exp(BetaPres)),
             Vol=exp(BetaVol+0.5*Cov),
             VolBillonM3=Pres*Vol) %>%
      mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE) %>%
      select(Annee,Residuel,ArbreID,NoArbre,Placette,Nombre,GrEspece,Espece,
             Etat,DHPcm,Iter,MSCR,hauteur_pred,vol_dm3,Produit,VolBillonM3,Stm2ha,Sup_PE) %>%
      pivot_wider(names_from = Produit, values_from = VolBillonM3)

    if(!"F1" %in% names(Sim_biol_2015)){
      Sim_biol_2015 <-Sim_biol_2015 %>%
      mutate(F1=NA)
    }
  }


  return(Sim_biol_2015)
}
