# test_that("SortieDendroIterSamare return the expected data frame avec simplifier=F", {
#
#   simul <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds")) %>%
#     rename(Hautm=hauteur_pred, Vol_dm3=vol_dm3) %>%
#     mutate(Temps = Annee-2025)
#
#   obtenu <- SortieDendroIterSamare(simul)
#
#   # je suis pas mal sûre que le fichier attendu n'a pas été fait avec le fichier simul, les recrues ne sont pas les mêmes
#   attendu <- readRDS(test_path("fixtures", "expect_for_sortie_dendroIter_samare_test.rds")) %>%
#     mutate(Annee = Annee+1)
#
#   expect_equal(obtenu, attendu)
# })
test_that("SortieDendroIterSamare return the expected data frame avec simplifier=F", {

  simul <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds")) %>%
    rename(Hautm=hauteur_pred, Vol_dm3=vol_dm3) %>%
    mutate(Temps = Annee-2025)

  obtenu <- SortieDendroIterSamare(simul)

  # on retrouve les essences en entrée + TOT
  ess_attendu <- c(unique(simul$GrEspece), 'TOT')
  ess_obtenu <- unique(obtenu$GrEspece)
  expect_equal(setequal(ess_attendu, ess_obtenu), TRUE)

  nom_obtenu <- names(obtenu)
  expect_equal(sum(c("Placette", "Iter", "Annee", "Temps", "GrEspece", "Etat", "Residuel") %in% nom_obtenu), 7)

  mort_obtenu <- obtenu %>% filter(Etat=='mort')
  mort_obtenu1 <- mort_obtenu %>% filter(!GrEspece=='TOT')  # mort seulement pour TOT
  expect_equal(nrow(mort_obtenu1),0)
  mort_obtenu2 <- mort_obtenu %>% filter(is.na(Ti_ha))  # mort compté seulement pour ti_ha, les autres à na
  mort_obtenu3 <- mort_obtenu %>% filter(!is.na(ST_m2ha))
  mort_obtenu4 <- mort_obtenu %>% filter(!is.na(Vol_m3ha))
  mort_obtenu5 <- mort_obtenu %>% filter(!is.na(DQM_cm))
  mort_obtenu6 <- mort_obtenu %>% filter(!is.na(HDom_m))
  expect_equal(nrow(mort_obtenu2)+nrow(mort_obtenu3)+nrow(mort_obtenu4)+nrow(mort_obtenu5)+nrow(mort_obtenu6),0)

  # somme des vivant+recrue
  attendu_recrue <- simul %>% filter(Etat != 'mort', Temps==5, Iter==1, GrEspece=='HEG') %>% summarise(nb = sum(Nombre/Sup_PE))
  obtenu_recrue <- obtenu %>% filter(Etat=='vivant', Temps==5, Iter==1, GrEspece=='HEG')
  expect_equal(obtenu_recrue$Ti_ha, attendu_recrue$nb)

})



test_that("SortieDendroIterSamare return the expected data frame avec une simul cubage=F", {

  simul <- SimulHtVol <- SimulSaMARE(NbIter = 2, Horizon = 4, Data = Test2500m2, cubage = F)

  obtenu <- SortieDendroIterSamare(simul)

  # hdom et vol sont tous à na
  obtenu_vol <- obtenu %>% filter(!is.na(Vol_m3ha))
  obtenu_hdom <- obtenu %>% filter(!is.na(HDom_m))
  expect_equal(nrow(obtenu_vol)+nrow(obtenu_hdom),0)

})


test_that("SortieDendroIterSamare return the expected data frame avec simplifier=T", {

  # data à la fin de la simulation de samare
  simul <- SimulSaMARE(NbIter = 2, Horizon = 4, Data = Test2500m2)

  # fonctionne sans erreur
  expect_no_error(SortieDendroIterSamare(simul, simplifier = T))

  obtenu <- Sommaire_Classes_DHP(simul, simplifier = T)

  list_an <- unique(obtenu$Temps)

  expect_equal(list_an, c(0,20))


})


