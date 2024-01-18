
#' calcule du billonnage
#'
#' @param arbres  data frame liste des arbes a billoner
#' @return
#' @examples Billonnage(arbres)


Billonnage<-function (arbres){

####################Préparation des paramètres###########################
########################################################################

suppressMessages(
  Para<-read_delim("Parametres/ParaPetroFinal.csv", delim = ";"))

suppressMessages(
  CovParms<-read_delim("Parametres/CovParmPetro.csv", delim = ";") %>%
    filter(Cov>0)
)


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

arbres2<-arbres %>%
         mutate(Essence_billon=ifelse(is.na(Espece)==TRUE, GrEspece, Espece)) %>%
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
         select(PlacetteID,ArbreID,Annee,Produit,VolBillonM3) %>%
        left_join(arbres,., relationship="many-to-many")



return(arbres2)

}
