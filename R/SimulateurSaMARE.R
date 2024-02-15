#'Fonction qui sert à appeler le simulateur SaMARE et qui fournit les données
#'initiales ainsi qu'un choix de paramètres pour la simulation.
#'
#' @param NbIter Valuere numérique du nombre d'iterations à effectuer (ex: 300).
#' @param AnneeDep Année de départ de la simulation, cette valeure sera ajustée de
#'                 5 ans pour à chaque pas de simulation (ex: 2023).
#' @param Horizon Valeure numérique du nombre de période de 5 ans sur lesquelles
#'                le simulateur effectuera ses simulations (ex: 6 pour 30 ans de simulations).
#' @param RecruesGaules Variable prenant la valeur de "1" pour utiliser les
#'                       paramètres de recrutement basé sur l'inventaire des gaules
#'                       de la placette et de "0" pour utiliser le module de
#'                       recrutement basé sur les arbres de dimension marchande.
#' @param Data Un dataframe contenant les valeurs de départ pour une liste
#'             d'arbres à simuler. Les champs: "Placette","NoArbre","Espece",
#'             "Etat","DHPcm","Vigueur","Nombre","Sup_PE","Annee_Coupe",
#'             "Latitude","Longitude","Altitude","Pente","Ptot","Tmoy","GrwDays",
#'             "Reg_Eco","Type_Eco", "MSCR","ntrt" doivent être présents. Si l'information
#'             sur certains champs n'est pas disponible, on peut le laisser vide.
#' @param Gaules Un dataframe contenant les valeurs de départ du nombre de gaules
#'                par espèce et par classe de diamètre. Cette information doit être
#'                fournie si le paramètre "RecruesGaules=1.
#'                Les champs: "Placette","Espece","DHPcm",#' "Nombre","Sup_PE"
#'                doivent être présents.
#' @return Retourne un dataframe contenant la liste des arbres, leur état, leur DHP,
#'         leur hauteur et leur volume pour chaque placette, chaque pas de simulation
#'         et chaque iteration.
#'
#' @examples


SimulSaMARE<-function(NbIter,AnneeDep,Horizon,RecruesGaules,Data,Gaules =NA){
  select=dplyr::select
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
            "GrwDays","Reg_Eco","Type_Eco", "MSCR","ntrt","ABCD")

Data<-Data %>%
  left_join(ListeSp)

Data<-Data[ColOrdre]

################Gaules######################

if (RecruesGaules==1){

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
}

#########Selection nombre d'iteration et de l'horizon de simulation#############

#################################################################################
#####################Génération des effets aléatoires###########################
###############################################################################
RandPlacStep<-RandomPlacStep(CovParms=CovParms,Data=Data,
                             NbIter=NbIter,NbPeriodes=Horizon)


######################Gaules###########
if (RecruesGaules==1){
RandPlacStepGaules<-RandomPlacStepGaules(CovParms=CovParmsGaules,Data=Gaules,
                                         NbIter=NbIter)
}
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




#
# plan(multisession) # Vous pouvez spécifier le nombre de workers si nécessaire, par exemple, plan(multisession, workers = 4)
#
# # Unique list of PlacetteID
# list_plot <- unique(ListeIter$PlacetteID)
#
# # Définition de la fonction pour être utilisée dans lapply pour traiter chaque PlacetteID
# processPlacetteID <- function(x, RandPlacStep, RandPlacStepGaules, Data, Gaules, ListeIter, AnneeDep, Horizon, RecruesGaules, CovParms, CovParmsGaules, Para, ParaGaules, Omega, OmegaGaules) {
#   filteredListeIter <- ListeIter[ListeIter$PlacetteID == x, ]
#   SaMARE(Random = RandPlacStep, RandomGaules = RandPlacStepGaules, Data = Data,
#          Gaules = Gaules, ListeIter = filteredListeIter,
#          AnneeDep = AnneeDep, Horizon = Horizon, RecruesGaules = RecruesGaules,
#          CovParms = CovParms, CovParmsGaules = CovParmsGaules,
#          Para = Para, ParaGaules = ParaGaules, Omega = Omega, OmegaGaules = OmegaGaules)
# }
#
# # Utilisation de future_lapply pour exécuter la fonction en parallèle
# Simul <- future_lapply(list_plot, processPlacetteID, RandPlacStep, RandPlacStepGaules, Data, Gaules, ListeIter, AnneeDep, Horizon, RecruesGaules, CovParms, CovParmsGaules, Para, ParaGaules, Omega, OmegaGaules, future.seed = TRUE)
#
# # Combinaison des résultats en un seul dataframe
# Simul <- bind_rows(Simul)
#
#
#




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


