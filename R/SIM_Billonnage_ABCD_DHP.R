#' Fonction prévoit la répartition par produits des arbres feuillus à l'aide des nouvelles
#' équation de Petro régionalisés issu des travaux du CFFM de Filip Havreljuk.
#'
#' @param Data UUn dataframe qui contient en ligne les arbres dont on veut prévoir
#'             les rendements en produit à l'aide du module de billonnage Petro
#'             régionalisé.
#' @param ligne une valeur binaire égale à 1 lorsque l'on désir avoir la sortie
#'              qui contient une ligne par produit ou 0 lorsque l'on désir avoir la sortie
#'              qui contient une colone par produit
#' @return Retourne un dataframe avec l'estimation du volume par classe de produit
#'          pour chacun des arbres feuillus.
#' @examples
#'

SIMBillonnageABCD_DHP<- function (data, ligne){

                          ##### ABCD#####

  if(!"eco" %in% colnames(data)){
    data <-ConvertisseurEco(data)
  }


  if ("QualiteABCD" %in% colnames(data) && !all(is.na(data$QualiteABCD))){

    Para<-read_delim("Parametres/ParaPetroFinal_New.csv", delim = ";")
    CovParms<-read_delim("Parametres/CovParmPetroABCD.csv", delim = ";")

    CovParms <- CovParms %>% filter(Cov>0)


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
      full_join(Vol_Billon, by=c("Essence_billon","Produit", "QualiteABCD", "eco")) %>%
      mutate(Presdhpcm2_classepetro=ifelse(is.na(Presdhpcm2_classepetro)==TRUE,0,Presdhpcm2_classepetro),
             Voldhpcm2_classepetro=ifelse(is.na(Voldhpcm2_classepetro)==TRUE,0,Voldhpcm2_classepetro))
    names(Pres_Billon)

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
      inner_join(CovParms, by=c("Essence_billon", "Produit")) %>%
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
             VolBillonM3=Pres*Vol)

    sim_ABCD_DHP <- sim_ABCD_DHP %>%
      mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE) %>%
      select(-c("Essence_billon", "eco", "Presclassepetro_eco", "Volclassepetro_eco", "Presclassepetro_qual", "Volclassepetro_qual",
                "Presdhpcm_classepetro", "Presdhpcm2_classepetro", "Voldhpcm_classepetro", "Voldhpcm2_classepetro", "Cov",
                "BetaPres", "BetaVol", "Pres" ,"Vol", "vigu0" , "prod0", "Nb_Gaules_Ha", "Nb_Gaules_ERS",
                "Nb_Gaules_BOJ","Nb_Gaules_SAB" , "Nb_Gaules_68_ERS", "Nb_Gaules_68_HEG","Nb_Gaules_68_BOJ","Nb_Gaules_68_SAB",
                "Nb_Gaules_68_Ha" , "Nb_Gaules_HEG", "Sup_PE" , "reg_eco", "Type_Eco", "Altitude", "Ptot", "Tmoy",
                "veg_pot", "milieu", "nb_tige", "eco" )) %>%
      relocate(Annee, PlacetteID,Residuel,ArbreID,NoArbre)


    if(ligne==FALSE){
    sim_ABCD_DHP <- sim_ABCD_DHP %>%
                    pivot_wider(names_from = Produit, values_from = VolBillonM3, names_prefix = "vm3_")

    }

  }else{      ######## DHP ##########


    Para<-read_delim("Parametres/ParaPetroFinal_dhp.csv", delim = ";")
    CovParms<-read_delim("Parametres/CovParmPetro_DHP.csv", delim = ";")

    CovParms <- CovParms %>% filter(Cov>0)

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
    full_join(Vol_Billon, by=c("Essence_billon","Produit", "eco"),relationship = "many-to-many") %>%
    mutate(Presdhpcm2__classepetro=ifelse(is.na(Presdhpcm2__classepetro)==TRUE,0,Presdhpcm2__classepetro),
           Voldhpcm2_classepetro=ifelse(is.na(Voldhpcm2_classepetro)==TRUE,0,Voldhpcm2_classepetro))
  names(Pres_Billon)

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
    inner_join(CovParms, by=c("Essence_billon", "Produit")) %>%
    mutate(Cov=ifelse(is.na(Cov)==TRUE,0,Cov)) %>%
    mutate(BetaPres= Presclassepetro_eco+
             (DHPcm/10)*Presdhpcm__classepetro+ #mise a l'echelle du dhp
             (DHPcm/10)^2*Presdhpcm2__classepetro, #mise a l'echelle du dhp
           BetaVol=  Volclassepetro_eco+
             DHPcm*Voldhpcm_classepetro+
             DHPcm^2*Voldhpcm2_classepetro,
           Pres=exp(BetaPres)/(1+exp(BetaPres)),
           Vol=exp(BetaVol+0.5*Cov),
           VolBillonM3=Pres*Vol)


  sim_ABCD_DHP <- sim_ABCD_DHP %>%
    mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE) %>%
    select(-c("Essence_billon", "Presclassepetro_eco", "Volclassepetro_eco", "Presdhpcm__classepetro", "Presdhpcm2__classepetro",
              "Voldhpcm_classepetro", "Voldhpcm2_classepetro", "Cov", "BetaPres", "BetaVol", "Pres" ,"Vol","vigu0" , "prod0", "Nb_Gaules_Ha", "Nb_Gaules_ERS",
              "Nb_Gaules_BOJ","Nb_Gaules_SAB" , "Nb_Gaules_68_ERS", "Nb_Gaules_68_HEG","Nb_Gaules_68_BOJ","Nb_Gaules_68_SAB",
              "Nb_Gaules_68_Ha" , "Nb_Gaules_HEG", "Sup_PE" , "reg_eco", "Type_Eco", "Altitude", "Ptot", "Tmoy",
              "veg_pot", "milieu", "nb_tige", "eco")) %>%
    relocate(Annee, PlacetteID,Residuel,ArbreID,NoArbre)
    #rename( "F1" = "vm3_F1")


  if(ligne==FALSE){
  sim_ABCD_DHP <- sim_ABCD_DHP %>%
  pivot_wider(names_from = Produit, values_from = VolBillonM3, names_prefix = "vm3_")

  }

  }


  return (sim_ABCD_DHP )

}
