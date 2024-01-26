
#' Fonction de billonnage basé sur le module Petro 2014. La fonction calcul la
#' répartition des volumes entre des bille de différents grade de qualité soit
#' Deroulage, Sciage (F1, F2, F3 et F4) et Pate. La fonction effectue le
#' billonnage pour l'érable à sucre, le bouleau jaune, le bouleau à papier,
#' le chêne rouge, l'érable rouge et le hêtre à grandes feuilles.
#'
#' @param arbres  Un data frame où chaque ligne représente un arbre ou un groupe
#'                d'arbres partageant les mêmes caractéristiques à billonner.
#'                Le dataframe est généralement une sortie de SaMARE
#'               où d'Artémis dans lequel le diamètre, la hauteur et le volume
#'               marchand de l'arbre sont spécifiés.
#' @return   Retourne un dataframe avec les mêmes informations présente dans le
#'           dataframe arbre en plus d'une colone produit  et le volume en m3
#'           de chacun des produits.
#' @examples Billonnage(arbres)


Billonnage<-function (arbres){
  select=dplyr::select
####################Préparation des paramètres###########################
########################################################################


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
