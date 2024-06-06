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

#Fichiers pour tests sorties Arbre SaMARE

expect_for_sortie_arbre_samare_test<-SortieArbreSamare(expect_for_Simulateur_Samare_sans_gaules_et_coupe_test)

usethis::use_data(expect_for_sortie_arbre_samare_test,
                  internal=FALSE, overwrite = TRUE)

#Fichiers pour tests Sommaire classe DHP SaMARE

expect_for_arbre_sommaire_classes_DHP<- Sommaire_Classes_DHP(expect_for_Simulateur_Samare_sans_gaules_et_coupe_test)

usethis::use_data(expect_for_arbre_sommaire_classes_DHP,
                  internal=FALSE, overwrite = TRUE)

#Fichiers pour tests Sommaire DendroIter  SaMARE

expect_for_sortie_dendroIter_samare_test<- SortieDendroIterSamare(expect_for_Simulateur_Samare_sans_gaules_et_coupe_test)

usethis::use_data(expect_for_sortie_dendroIter_samare_test,
                  internal=FALSE, overwrite = TRUE)

#Fichiers pour tests Sommaire Dendro  SaMARE

expect_for_sortie_dendro_samare_test<- SortieDendroSamare(expect_for_Simulateur_Samare_sans_gaules_et_coupe_test)

usethis::use_data(expect_for_sortie_dendro_samare_test,
                  internal=FALSE, overwrite = TRUE)

