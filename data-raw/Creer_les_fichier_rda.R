# Tous les fichiers internes excel/csv/sas7bdat doivent être convertis en en seul fichier rda nommé sysdata.rda sous /R
# Tous les fichiers d'exemples doivent être convertis individuellement en rda et mis sous /data
# le fichier avec le code pour créer le fichier sysdata.rda doit être sauvegardé sous R/data-raw

# exemple:
# param_tarif = read.sas7bdat("c:/Mes docs/ data/ beta_volume.sas7bdat")
# param_ht = read.sas7bdat("c:/Mes docs/ data/ beta_ht.sas7bdat")
# Puis utiliser la ligne de code suivant (toujours dans le projet du package)
# usethis::use_data(param_tarif, param_ht, internal=TRUE): ça fonctionne seulement si le projet est un package

#library(readxl)
#library(sas7bdat)


#write_delim(MatchModuleCovparms, "data_raw\\Parametre_Samare\\MatchModuleCovparms.csv", delim = ';')
MatchModuleCovparms <- read_delim("data-raw\\Parametre_Samare\\MatchModuleCovparms.csv", delim = ';')

#write_delim(AttribQual, "data_raw\\Parametre_Samare\\AttribQual.csv", delim = ';')
AttribQual <- read_delim("data-raw\\Parametre_Samare\\AttribQual.csv", delim = ';')

#write_delim(EffetCovParms, "data_raw\\Parametre_Samare\\EffetCovParms.csv", delim = ';')
EffetCovParms <- read_delim("data-raw\\Parametre_Samare\\EffetCovParms.csv", delim = ';')

#write_delim(MatchModuleOmega, "data_raw\\Parametre_Samare\\MatchModuleOmega.csv", delim = ';')
MatchModuleOmega <- read_delim("data-raw\\Parametre_Samare\\MatchModuleOmega.csv", delim = ';')

#write_delim(MatchModuleParameters, "data_raw\\Parametre_Samare\\MatchModuleParameters.csv", delim = ';')
MatchModuleParameters <- read_delim("data-raw\\Parametre_Samare\\MatchModuleParameters.csv", delim = ';')

#write_delim(OmegaEvolQual, "data_raw\\Parametre_Samare\\OmegaEvolQual.csv", delim = ';')
OmegaEvolQual <- read_delim("data-raw\\Parametre_Samare\\OmegaEvolQual.csv", delim = ';')

#write_delim(OmegaGaulesFormat, "data_raw\\Parametre_Samare\\OmegaGaulesFormat.csv", delim = ';')
OmegaGaulesFormat <- read_delim("data-raw\\Parametre_Samare\\OmegaGaulesFormat.csv", delim = ';')

#write_delim(ParametresEvolQual, "data_raw\\Parametre_Samare\\ParametresEvolQual.csv", delim = ';')
ParametresEvolQual <- read_delim("data-raw\\Parametre_Samare\\ParametresEvolQual.csv", delim = ';')

#write_delim(ParametresGaules, "data_raw\\Parametre_Samare\\ParametresGaules.csv", delim = ';')
ParametresGaules <- read_delim("data-raw\\Parametre_Samare\\ParametresGaules.csv", delim = ';')

#write_delim(CovparmGaules, "data_raw\\Parametre_Samare\\CovparmGaules.csv", delim = ';')
CovparmGaules <- read_delim("data-raw\\Parametre_Samare\\CovparmGaules.csv", delim = ';')

#write_delim(Species, "data_raw\\Parametre_Samare\\Species.csv", delim = ';')
Species <- read_delim("data-raw\\Parametre_Samare\\Species.csv", delim = ';')

#write_delim(SpeciesGroups, "data_raw\\Parametre_Samare\\SpeciesGroups.csv", delim = ';')
SpeciesGroups <- read_delim("data-raw\\Parametre_Samare\\SpeciesGroups.csv", delim = ';')

#write_delim(MatchSpeciesGroups, "data_raw\\Parametre_Samare\\MatchSpeciesGroups.csv", delim = ';')
MatchSpeciesGroups <- read_delim("data-raw\\Parametre_Samare\\MatchSpeciesGroups.csv", delim = ';')

#write_delim(SubModuleID, "data_raw\\Parametre_Samare\\SubModuleID.csv", delim = ';')
SubModuleID <- read_delim("data-raw\\Parametre_Samare\\SubModuleID.csv", delim = ';')




# créer les fichiers d'association d'essences pour les équations ht et volume
# en utilisant les mêmes associations que dans Samare2018-Capsis
# pour le volume du BOJ, on utilise l'ERS dans le fichier MatchSpeciesGroups (VolumeSpeciesID=10) au lieu du BOJ (VolumeSpeciesID=2)
# vérifier avec Filip si c'est volontaire (dans Artemis-Capsis, on utilise le volume du BOJ pour le BOJ)

VolMatchSpeciesGroups <- read_delim("data-raw/0_VolMatchSpeciesGroups.csv", delim=';') %>% dplyr::select(-HarvestSpeciesID, -VegPotID)
EssenceID_Ht <- read_delim("data-raw/0_SpeciesHD.csv", delim=';')
EssenceID_Vol <- read_delim("data-raw/0_SpeciesVol.csv", delim=';')

VolMatchSpeciesGroups <- left_join(VolMatchSpeciesGroups, Species) %>% dplyr::select(-SpeciesID)
VolMatchSpeciesGroups <- left_join(VolMatchSpeciesGroups, SpeciesGroups) %>% dplyr::select(-SpeciesGroupID, -OldID)
VolMatchSpeciesGroups <- left_join(VolMatchSpeciesGroups, EssenceID_Ht) %>% dplyr::select(-RelationHDSpeciesID)

ass_ess_ht_vol <- left_join(VolMatchSpeciesGroups, EssenceID_Vol) %>% dplyr::select(-VolumeSpeciesID) %>% rename(Espece=SpeciesName, GrEspece=SpeciesGroupName)


# tous les fichiers à mettre dans le fichier sysdata.rda
usethis::use_data(MatchModuleCovparms, AttribQual, EffetCovParms, MatchModuleOmega, MatchModuleParameters, OmegaEvolQual, ParametresEvolQual,
                  OmegaGaulesFormat, ParametresGaules, CovparmGaules,
                  Species, SpeciesGroups, MatchSpeciesGroups, ass_ess_ht_vol,
                  SubModuleID,
                  internal=TRUE, overwrite = TRUE)


# Ajout de t et Especes dans sysdata
# Pas de temps
t <- 5
# Liste d'Especes
Especes <- c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB")

# Créer un environnement temporaire
temp_env <- new.env()
# Charger le fichier sysdata.rda dans cet environnement
load("R/sysdata.rda", envir = temp_env)
# Vérifier les objets actuellement dans sysdata.rda
ls(envir = temp_env)
# Ajouter les nouveaux objets à l'environnement temporaire

temp_env$ass_ess_ht_vol <- ass_ess_ht_vol

#view(temp_env$t)

# Sauvegarder tous les objets présents dans l'environnement temporaire
save(list = ls(envir = temp_env), file = "R/sysdata.rda", envir = temp_env)

rm(temp_env)



# ajouter 2 fichiers d'exemples
load("data-raw/Fichiers_exemple/Test400m2.rda")
load("data-raw/Fichiers_exemple/Test400m2Coupe.rda")

# Créer un environnement temporaire
temp_env <- new.env()
# Charger le fichier sysdata.rda dans cet environnement
load("R/sysdata.rda", envir = temp_env)
# Vérifier les objets actuellement dans sysdata.rda
ls(envir = temp_env)
# Ajouter les nouveaux objets à l'environnement temporaire

temp_env$Test400m2 <- Test400m2
temp_env$Test400m2Coupe <- Test400m2Coupe

view(temp_env$Test400m2)
view(temp_env$Test400m2Coupe)

# Sauvegarder tous les objets présents dans l'environnement temporaire
save(list = ls(envir = temp_env), file = "R/sysdata.rda", envir = temp_env)

rm(temp_env)


