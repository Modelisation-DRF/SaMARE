# test_that("SortieArbreSamare return the expected data frame ", {
#
#   expect_for_Samare_sans_gaules_et_coupe_test <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds"))
#   expect_for_sortie_arbre_samare_test <- readRDS(test_path("fixtures", "expect_for_sortie_arbre_samare_test.rds"))
#   expect_equal(SortieArbreSamare(expect_for_Samare_sans_gaules_et_coupe_test), expect_for_sortie_arbre_samare_test )
# })


test_that("SortieArbreSamare fonctionne avec simplifier=F", {

  # data à la fin de la simulation de samare
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)

  # fonctionne sans erreur
  expect_no_error(SortieArbreSamare(simul))

  obtenu <- SortieArbreSamare(simul)

  expect_equal(obtenu,simul)

})
#
# test_that("SortieArbreSamare fonctionne sans haut-vol", {
#
#   # data à la fin de la simulation de samare
#   simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2, cubage = F)
#
#   # fonctionne sans erreur
#   expect_no_error(SortieArbreSamare(simul))
#
#   obtenu <- SortieArbreSamare(simul)
#
#   ht <- obtenu %>% filter(is.na(Hautm))
#   vo <- obtenu %>% filter(is.na(Vol_dm3))
#
#   expect_equal(nrow(ht), nrow(simul))
#   expect_equal(nrow(vo), nrow(simul))
#
#
# })


test_that("SortieArbreSamare fonctionne avec le parametre simplifier", {

  # data à la fin de la simulation de samare
  simul <- SimulSaMARE(NbIter = 2, Horizon = 4, Data = Test2500m2)

  # fonctionne sans erreur
  expect_no_error(SortieArbreSamare(simul, simplifier = T))

  obtenu <- SortieArbreSamare(simul, simplifier = T)

  list_an <- unique(obtenu$Temps)

  expect_equal(list_an, c(0,20))

})

test_that("SortieArbreSamare fonctionne avec une simulation avec gaules", {

  # data à la fin de la simulation de samare
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2, RecruesGaules = 1, Gaules = GaulesTest2500m2)

  # fonctionne sans erreur
  expect_no_error(SortieArbreSamare(simul, simplifier = T))

  obtenu <- SortieArbreSamare(simul)

  expect_equal(obtenu,simul)
})
