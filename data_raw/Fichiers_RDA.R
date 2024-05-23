# créer les fichiers d'association d'essences pour les équations ht et le volume
# en utilisant les mêmes associations que dans Samare2018-Capsis

# pour le volumne du BOJ, on utilise l'ERS dans le fichier MatchSpeciesGroups (VolumeSpeciesID=10) au lieu du BOJ (VolumeSpeciesID=2)
# vérifier avec Filip si c'est volontaire (dans Artemis-Capsis, on utilise le volume du BOJ pour le BOJ)

VolMatchSpeciesGroups <- read_delim("data_raw/0_VolMatchSpeciesGroups.csv", delim=';') %>% dplyr::select(-HarvestSpeciesID, -VegPotID)
SpeciesID <- read_delim("data_raw/0_Species.csv", delim=';') %>% rename(Espece=SpeciesName)
SpeciesGroupsID <- read_delim("data_raw/0_SpeciesGroups.csv", delim=';') %>% dplyr::select(-OldID) %>% rename(GrEspece=SpeciesGroupName)

VolMatchSpeciesGroups <- left_join(VolMatchSpeciesGroups, SpeciesID) %>% dplyr::select(-SpeciesID)
VolMatchSpeciesGroups <- left_join(VolMatchSpeciesGroups, SpeciesGroupsID) %>% dplyr::select(-SpeciesGroupID)

EssenceID_Ht <- read_delim("data_raw/0_SpeciesHD.csv", delim=';')
EssenceID_Vol <- read_delim("data_raw/0_SpeciesVol.csv", delim=';')

VolMatchSpeciesGroups <- left_join(VolMatchSpeciesGroups, EssenceID_Ht) %>% dplyr::select(-RelationHDSpeciesID)
ass_ess_ht_vol <- left_join(VolMatchSpeciesGroups, EssenceID_Vol) %>% dplyr::select(-VolumeSpeciesID)

usethis::use_data(ass_ess_ht_vol,
                  internal=FALSE, overwrite = TRUE)

Test400m2 <- read_delim("data_raw/Test400m2.csv", delim=';')

usethis::use_data(Test400m2,
                  internal=FALSE, overwrite = TRUE)
