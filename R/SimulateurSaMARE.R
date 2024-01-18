
#'fonction pour faire une simulation
#'
#' @param NbIter le nombre d'iteration
#' @param AnneeDep année de départ
#' @param Horizon le nombre de periode de 5 ans
#' @param RecruesGaules nombre de recrue
#' @param Data donnée de départ
#' @param Gaules donnée de départ pour les gaules
#' @return
#' @examples


SimulSaMARE<-function(NbIter,AnneeDep,Horizon,RecruesGaules,Data,Gaules){

################################ Lecture des fichiers de placette et de parametres ###################

Parametres_SimulSaMARE <-ParametresSimulSaMARE()

# Fichier des effets aleatoires
CovParms<-Parametres_SimulSaMARE[[1]]
EfCovParms<-Parametres_SimulSaMARE[[2]]

CovParmsGaules<-Parametres_SimulSaMARE[[3]]

####### Fichier des parametres
Para<-Parametres_SimulSaMARE[[4]]

Para <- Para %>%
  mutate(Effect = str_to_lower(Effect)) %>%
  rename(GrEspece=Ess_groupe) %>%
   select(-VegPotID,-Veg_Pot)

ParaGaules<-Parametres_SimulSaMARE[[5]] %>%
  rename(GrEspece=Ess_groupe)

# Fichier des especes
Sp<-Parametres_SimulSaMARE[[6]]
SpGroups<-Parametres_SimulSaMARE[[7]]
# Fichier des especes dans chacun des groupes d'especes
MatchSpGroups<-Parametres_SimulSaMARE[[8]]

#Omega
Omega<-Parametres_SimulSaMARE[[9]]
OmegaGaules<-Parametres_SimulSaMARE[[10]]


############################# Construction de vecteurs pour simulation ##############################

# Merge des fichers des especes et des groupes d'especes, et ensuite merge avec les Essence,
# pour obtenir la liste des essences dans chaque groupe d'essences
ListeSp<- merge(MatchSpGroups,SpGroups, by="SpeciesGroupID") %>%
  merge(Sp, by="SpeciesID") %>%
  rename(Espece=SpeciesName,GrEspece=SpeciesGroupName) %>%
  select(GrEspece,Espece)

####################Importation et préparation des donnes des arbre a simuler################
# Fichier des arbres
ColOrdre<-c("Placette","NoArbre","Espece","GrEspece","Etat","DHPcm","Vigueur","Nombre",
            "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Pente","Ptot","Tmoy",
            "GrwDays","Reg_Eco","Type_Eco", "MSCR","ntrt")

Data<-Data %>%
  left_join(ListeSp)

Data<-Data[ColOrdre]

################Gaules######################

ColOrdre<-c("Placette","Espece","GrEspece","DHPcm","Nombre","Sup_PE")

Gaules<-Gaules %>%
  inner_join(ListeSp) %>%
  filter(!Espece %in% c("ERE","ERP","PRP","SAL","SOA","SOD","AME","AUR","ERE"))
Gaules<-Gaules[ColOrdre]

#############Sélection des placetes avec Gaules

IndexGaules<-Gaules %>%
  group_by(Placette) %>%
  summarise()
Data<-Data %>%
  inner_join(IndexGaules)

#########Selection nombre d'iteration et de l'horizon de simulation#############

#################################################################################
#####################Génération des effets aléatoires###########################
###############################################################################
RandPlacStep<-RandomPlacStep(CovParms=CovParms,Data=Data,
                             NbIter=NbIter,NbPeriodes=Horizon)


######################Gaules###########
RandPlacStepGaules<-RandomPlacStepGaules(CovParms=CovParmsGaules,Data=Gaules,
                                         NbIter=NbIter)

################################################################################
###########################Copie des données initiale * Nb Iter################
##############################################################################
ListeIter<-rep(1:NbIter)
ListeIter<-Data %>%
  group_by(Placette) %>%
  summarise() %>%
  merge(ListeIter) %>%
  rename(Iter=y) %>%
  mutate(PlacetteID=paste(Placette,"_",Iter,sep="")) %>%
  relocate(PlacetteID,.before=Placette) %>%
  arrange(PlacetteID)


# La fonction DoFuture permet de parraléliser les simulations
ListeIterBck<-ListeIter

registerDoFuture()
list_plot <- unique(ListeIter$PlacetteID)
plan(multisession)
Simul<- bind_rows(
  foreach(x = iterators::iter(list_plot)) %dorng%   ######utilisation de doRNG permet de controler la seed
    {SaMARE(Random=RandPlacStep,RandomGaules=RandPlacStepGaules,Data=Data,
            Gaules=Gaules, ListeIter=ListeIter[ListeIter$PlacetteID==x,],
            AnneeDep=AnneeDep,Horizon=Horizon,RecruesGaules=RecruesGaules,
            CovParms=CovParms,CovParmsGaules=CovParmsGaules,
            Para=Para,ParaGaules=ParaGaules,Omega=Omega, OmegaGaules=OmegaGaules)}
)




VarEco<-Data %>%
  group_by(Placette) %>%
  summarise(Sup_PE=first(Sup_PE),reg_eco=first(Reg_Eco),Type_Eco=first(Type_Eco),
            Altitude=first(Altitude),Ptot=first(Ptot),Tmoy=first(Tmoy)) %>%
  mutate(veg_pot=substr(Type_Eco,1,3),milieu=substr(Type_Eco,4,4))

Simul<-Simul %>%
  inner_join(VarEco, relationship="many-to-many") %>%
  mutate(nb_tige=Nombre*Sup_PE/25) %>%   #Conversion pour relation HD
  rename(id_pe=Placette, dhpcm=DHPcm, essence=GrEspece,no_arbre=ArbreID,
         altitude=Altitude,p_tot=Ptot,t_ma=Tmoy, iter=Iter)


nb_iter <- length(unique(Simul$iter))
nb_periodes <- Horizon+1
listeAnnee<-unique(Simul$Annee)
parametre_ht <- param_ht(fic_arbres=Simul, mode_simul='STO', nb_iter=nb_iter, nb_step=(nb_periodes)) ###Nombre plus 1 pour tenir compte de l'année initiale
para_volume<-param_vol(Simul,mode_simul="STO", nb_iter=nb_iter)
# ouverture de session en parralèle
registerDoFuture()
plan(multisession)
SimulHtVol1 <- bind_rows(
  foreach (i = 1:(nb_iter)) %:%
    foreach (k = 1:(nb_periodes)) %dopar%{
      ht <- relation_h_d(fic_arbres=Simul[Simul$iter==i & Simul$Annee==listeAnnee[k] & Simul$Etat!="mort",], mode_simul='STO',
                         iteration=i, step=k, parametre_ht=parametre_ht, reg_eco=TRUE)
      ht<-  cubage(ht, mode_simul="STO", iteration=i, parametre_vol=para_volume)
    }
)


SimulHtVol1<-SimulHtVol1[,c("id_pe","Annee","iter","no_arbre","hauteur_pred","vol_dm3")] ###Garde juste les variables de hauteur et volume pour
###joindre avec Simul pour garder les morts

SimulHtVol<-Simul %>%
  left_join(SimulHtVol1, by=c("id_pe","Annee","no_arbre","iter")) %>%
  rename(Placette=id_pe, DHPcm=dhpcm, GrEspece=essence,ArbreID=no_arbre,
         Altitude=altitude,Ptot=p_tot,Tmoy=t_ma, Iter=iter) %>%
  mutate(PlacetteID=paste(Placette,"_",Iter, sep=""))


return(SimulHtVol)

}


# #write_delim(SimulHtVol,"Resultats/SimulHtVol.csv",delim=";")
# VolumeMoyen<-SimulHtVol %>%
#   filter(Etat=="vivant" ) %>%
#   group_by(Placette,Iter,Annee) %>%
#   summarise(VolIterha=sum(vol_dm3)/2500*10000/1000) %>%
#   arrange(Placette,Annee,VolIterha) %>%
#   group_by(Placette,Annee) %>%
#   summarise(Pct10=quantile(VolIterha,0.1), moy=mean(VolIterha), Pct90=quantile(VolIterha,0.9))
#
#
#
#
#
#
#
# #write_delim(SimulHtVol,"Resultats/SimulHtVol.csv",delim=";")
#
# # VolumeMoyen<-SimulHtVol %>%
# #              filter(Etat=="vivant") %>%
# #              group_by(Placette,Iter,Annee) %>%
# #              summarise(VolIterha=sum(vol_dm3)/2500*10000/1000) %>%
# #              group_by(Placette, Annee) %>%
# #              summarise(Volha=mean(VolIterha))
# # write_csv(VolumeMoyen,"VolumeMoyenTEM23APC500_150Iter.csv")
#
# #################################################################################
# #############################Billonnage#########################################
# ################################################################################
# #Prévoit le volumme par classe pétro (variable VolBillonM3) en m3 pour un arbre entier
# #(ne tient pas compte du nombre d'arbres')
# #basé sur Petro 2014
#
# SimulHtVolBillon<-Billonnage(arbres=SimulHtVol)
#
#
#
#
# # VolumeMoyenBillon<-SimulHtVolBillon %>%
# #   filter(Etat=="vivant") %>%
# #   group_by(Placette,Iter,Annee,Produit) %>%
# #   summarise(VolIterha=sum(VolBillonM3)/2500*10000) %>%
# #   group_by(Placette, Annee,Produit) %>%
# #   summarise(Volha=mean(VolIterha))
# # write_csv(VolumeMoyenBillon,"VolumeMoyenBillonTEM23APC500_150Iter.csv")
#
# #################################################################################
# ####################Distribution DHP moyenne######################
# ###############################################################################
#
# SommaireClassesDHPSp<-SimulHtVol %>%
#   filter(Etat!="mort") %>%
#   mutate(Stm2ha=pi*(DHPcm/200)^2/Sup_PE,      #Calcul surface terrière par ha
#          DHPcm2=DHPcm^2,
#          Nb=Nombre/Sup_PE,                   #Nb tige ha
#          vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE),#volume par ha en dm3 a mettre en m3
#          DHP_cl=round(DHPcm/2)*2) %>%
#   group_by(Placette,GrEspece,DHP_cl,Annee,Iter) %>%
#   summarise(StM2Ha=sum(Stm2ha), NbHa=sum(Nb), DQM=(mean(DHPcm2,na.rm=TRUE))^0.5,
#             VolM3Ha=sum(vol_dm3)/1000, .groups="drop") %>%
#   group_by(Placette,Annee,GrEspece,DHP_cl) %>%
#   summarise(NbHa=sum(NbHa)/NbIter, StM2Ha=sum(StM2Ha)/NbIter,
#             VolM3Ha=sum(VolM3Ha)/NbIter, .groups="drop") %>%
#   arrange(Placette,Annee,GrEspece,DHP_cl)
#
# SommaireClassesDHP<-SommaireClassesDHPSp %>%
#   group_by(Placette,Annee,DHP_cl) %>%
#   summarise(NbHa=sum(NbHa), StM2Ha=sum(StM2Ha),VolM3Ha=sum(VolM3Ha)) %>%
#   mutate(GrEspece="TOT") %>%
#   rbind(SommaireClassesDHPSp) %>%
#   arrange(Placette,Annee,GrEspece,DHP_cl)
#
#
#
# #################################################################################
# ################################Enregistrement du fichier final#################
# ###############################################################################
#
# #write.csv(SommaireClassesDHP,"Resultats/SommaireClassesdhp.csv")
#
#
#
#
