# test_that("Sommaire_Classes_DHP return the expected data frame", {
#
#   expect_for_Samare_sans_gaules_et_coupe_test <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds"))
#   som <- Sommaire_Classes_DHP(expect_for_Samare_sans_gaules_et_coupe_test)
#   som <- som %>%
#     mutate(across(where(is.numeric), ~ round(., 6)))%>% ungroup()
#
#   expect_for_arbre_sommaire_classes_DHP <- readRDS(test_path("fixtures", "expect_sommaire_classes_DHP.rds"))
#   sommaire_ <- expect_for_arbre_sommaire_classes_DHP %>%
#     mutate(across(where(is.numeric), ~ round(., 6)))%>% ungroup()
#
#   expect_equal(som, sommaire_)
#
# })


test_that("Sommaire_Classes_DHP fonctionne avec volume", {

  # bon calcul
  attendu <- readRDS(test_path("fixtures/sortie_sommaire_cl_dhp", "attendu_sommaire_DHP.rds")) %>% rename(Ti_ha=NbHa, ST_m2ha=StM2Ha, Vol_m3ha=VolM3Ha)
  simul <- readRDS(test_path("fixtures/sortie_sommaire_cl_dhp", "sortie_simul.rds"))
  obtenu <- Sommaire_Classes_DHP(simul)
  expect_equal(obtenu, attendu)

  # fonctionne sans erreur
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)
  expect_no_error(Sommaire_Classes_DHP(simul))

})

test_that("Sommaire_Classes_DHP fonctionne sans volume", {

  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2, cubage = F)

  obtenu <- Sommaire_Classes_DHP(simul)

  obtenuv <- obtenu %>% filter(!is.na(Vol_m3ha)) # tous les volume sont à NA

  expect_equal(nrow(obtenuv), 0)

})


test_that("Sommaire_Classes_DHP fonctionne avec simul avec gaules ", {

  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2, RecruesGaules = 1, Gaules = GaulesTest2500m2)

  expect_no_error(Sommaire_Classes_DHP(simul))


})


test_that("Sommaire_Classes_DHP fonctionne avec le parametre simplifier", {

  # data à la fin de la simulation de samare
  simul <- SimulSaMARE(NbIter = 2, Horizon = 4, Data = Test2500m2)

  # fonctionne sans erreur
  expect_no_error(Sommaire_Classes_DHP(simul, simplifier = T))

  obtenu <- Sommaire_Classes_DHP(simul, simplifier = T)

  list_an <- unique(obtenu$Temps)

  expect_equal(list_an, c(0,20))

})
