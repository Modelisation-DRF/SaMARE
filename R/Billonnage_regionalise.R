

ABCD_DHP_regio<- function (data, type){

  select=dplyr::select

  if (type == "ABCD"){

    CovParmPetroABCD <- CovParmPetroABCD %>% filter(Cov>0)


    Vol_Billon<-ParaPetroFinal_New %>%
      filter(Module=="Vol") %>%
      mutate(betaVol=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaVol, names_prefix = "Vol")

    Pres_Billon <- ParaPetroFinal_New %>%
      filter(Module=="Pres") %>%
      mutate(betaPres=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaPres, names_prefix = "Pres") %>%
      full_join(Vol_Billon, by=c("Essence_billon","Produit", "QualiteABCD", "eco")) %>%
      mutate(Presdhpcm2_classepetro=ifelse(is.na(Presdhpcm2_classepetro)==TRUE,0,Presdhpcm2_classepetro),
             Voldhpcm2_classepetro=ifelse(is.na(Voldhpcm2_classepetro)==TRUE,0,Voldhpcm2_classepetro))
    #names(Pres_Billon)

    par_qual <- Pres_Billon %>%
      filter(!is.na(QualiteABCD)) %>%
      select(c(Essence_billon, Produit, QualiteABCD, Presclassepetro_qual, Volclassepetro_qual))

    par_eco <- Pres_Billon %>%
      filter(!is.na(eco)) %>%
      select(c(Essence_billon, Produit, eco, Presclassepetro_eco, Volclassepetro_eco))

    par_num <- Pres_Billon %>%
      filter(is.na(eco) & is.na(QualiteABCD)) %>%
      select(c(Essence_billon, Produit, Presdhpcm_classepetro, Presdhpcm2_classepetro, Voldhpcm_classepetro, Voldhpcm2_classepetro))


    sim_ABCD_DHP <- data %>%
      mutate(Essence_billon=Espece) %>% #ajout
      filter(Essence_billon %in% c("ERS", "BOJ")) %>%
      left_join(par_eco, by=c("Essence_billon", "eco"), relationship="many-to-many") %>%
      left_join(par_qual, by=c("Essence_billon", "QualiteABCD", "Produit"), relationship="many-to-many") %>%
      left_join(par_num, by=c("Essence_billon", "Produit"), relationship="many-to-many") %>%
      inner_join(CovParmPetroABCD, by=c("Essence_billon", "Produit")) %>%
      mutate(Cov=ifelse(is.na(Cov)==TRUE,0,Cov)) %>%
      mutate(BetaPres= Presclassepetro_eco+
               Presclassepetro_qual+
               DHPcm*Presdhpcm_classepetro+
               DHPcm^2*Presdhpcm2_classepetro,
             BetaVol=  Volclassepetro_eco+
               Volclassepetro_qual+
               DHPcm*Voldhpcm_classepetro+
               DHPcm^2*Voldhpcm2_classepetro,
             Pres=exp(BetaPres)/(1+exp(BetaPres)),
             Vol=exp(BetaVol+0.5*Cov),
             VolBillonM3=Pres*Vol) %>%
      mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE) %>%
      select(Annee,Residuel,ArbreID,NoArbre,Placette,Nombre,GrEspece,Espece,
             Etat,DHPcm,Iter,MSCR,hauteur_pred,vol_dm3,Produit,VolBillonM3,Stm2ha,Sup_PE) %>%
      pivot_wider(names_from = Produit, values_from = VolBillonM3)



  }else if(type == "DHP"){      ######## DHP ##########


    CovParmPetro_DHP <- CovParmPetro_DHP %>% filter(Cov>0)

    Vol_Billon<-ParaPetroFinal_dhp %>%
      filter(Module=="Vol") %>%
      mutate(betaVol=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaVol, names_prefix = "Vol")

    Pres_Billon <- ParaPetroFinal_dhp %>%
      filter(Module=="Pres") %>%
      mutate(betaPres=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaPres, names_prefix = "Pres") %>%
      full_join(Vol_Billon, by=c("Essence_billon","Produit", "eco"),relationship = "many-to-many") %>%
      mutate(Presdhpcm2__classepetro=ifelse(is.na(Presdhpcm2__classepetro)==TRUE,0,Presdhpcm2__classepetro),
             Voldhpcm2_classepetro=ifelse(is.na(Voldhpcm2_classepetro)==TRUE,0,Voldhpcm2_classepetro))
    #names(Pres_Billon)

    par_eco <- Pres_Billon %>%
      filter(!is.na(eco)) %>%
      select(c(Essence_billon, Produit, eco, Presclassepetro_eco, Volclassepetro_eco))

    par_num <- Pres_Billon %>%
      filter(is.na(eco)) %>%
      select(c(Essence_billon, Produit, Presdhpcm__classepetro, Presdhpcm2__classepetro, Voldhpcm_classepetro, Voldhpcm2_classepetro))



    sim_ABCD_DHP <- data %>%
      mutate(Essence_billon=Espece) %>% #ajout
      filter(Essence_billon %in% c("ERS", "BOJ")) %>%
      left_join(par_eco, by=c("Essence_billon", "eco"), relationship = "many-to-many") %>%
      #left_join(par_qual, by=c("Essence_billon", "CLASSE_DE_"="QualiteABCD", "Produit")) %>%
      left_join(par_num, by=c("Essence_billon", "Produit")) %>%
      inner_join(CovParmPetro_DHP, by=c("Essence_billon", "Produit")) %>%
      mutate(Cov=ifelse(is.na(Cov)==TRUE,0,Cov)) %>%
      mutate(BetaPres= Presclassepetro_eco+
               (DHPcm/10)*Presdhpcm__classepetro+ #mise a l'echelle du dhp
               (DHPcm/10)^2*Presdhpcm2__classepetro, #mise a l'echelle du dhp
             BetaVol=  Volclassepetro_eco+
               DHPcm*Voldhpcm_classepetro+
               DHPcm^2*Voldhpcm2_classepetro,
             Pres=exp(BetaPres)/(1+exp(BetaPres)),
             Vol=exp(BetaVol+0.5*Cov),
             VolBillonM3=Pres*Vol) %>%
      mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE) %>%
      select(Annee,Residuel,ArbreID,NoArbre,Placette,Nombre,GrEspece,Espece,
             Etat,DHPcm,Iter,MSCR,hauteur_pred,vol_dm3,Produit,VolBillonM3,Stm2ha,Sup_PE) %>%
      # select(Espece, DHPcm, eco, QualiteABCD, Produit, Essence_billon,VolBillonM3 ) %>%
      pivot_wider(names_from = Produit, values_from = VolBillonM3)



  }

  return (sim_ABCD_DHP )

}
