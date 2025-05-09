test_that("nb_Gaules() fonctionne avec 2 placettes", {


  attendu <- readRDS(test_path("fixtures/gaules_evolution", "attendu_nb_gaules_total.rds"))
  Para.nb_gaules <- readRDS(test_path("fixtures/gaules_evolution", "Para.nb_gaules.rds"))
  RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))
  RecGaules <- readRDS(test_path("fixtures/gaules_evolution", "RecGaules_nb_gaules_total.rds"))
  Rec <- readRDS(test_path("fixtures/gaules_evolution", "Rec_nb_gaules_total.rds"))

  obtenu <- nb_Gaules(Rec=Rec, RecGaules=RecGaules, t=5, RandomPlacGaules=RandomPlacGaules, Para.nb_gaules=Para.nb_gaules)

  expect_equal(obtenu, attendu)

})


test_that("ratio_pi_Gaules() fonctionne avec 2 placettes", {

  attendu <- readRDS(test_path("fixtures/gaules_evolution", "attendu_pi_gaules.rds"))
  Para.ratio_gaules <- readRDS(test_path("fixtures/gaules_evolution", "Para.ratio_gaules.rds"))
  RecGaules <- readRDS(test_path("fixtures/gaules_evolution", "RecGaules_pi_gaules.rds"))
  Rec <- readRDS(test_path("fixtures/gaules_evolution", "Rec_pi_gaules.rds"))
  RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))
  Ratio <- readRDS(test_path("fixtures/gaules_evolution", "Ratio_pi_gaules.rds"))

  obtenu <- ratio_pi_Gaules(Ratio=Ratio, Rec=Rec, RecGaules=RecGaules, t=5, RandomPlacGaules=RandomPlacGaules, Para.ratio_gaules=Para.ratio_gaules)

  expect_equal(obtenu, attendu)

})


test_that("ratio_count_Gaules() fonctionne avec 2 placettes", {

  attendu <- readRDS(test_path("fixtures/gaules_evolution", "attendu_count_gaules.rds"))
  Para.ratio_gaules <- readRDS(test_path("fixtures/gaules_evolution", "Para.ratio_gaules.rds"))
  RecGaules <- readRDS(test_path("fixtures/gaules_evolution", "RecGaules_count_gaules.rds"))
  Rec <- readRDS(test_path("fixtures/gaules_evolution", "Rec_count_gaules.rds"))
  RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))
  Ratio <- readRDS(test_path("fixtures/gaules_evolution", "Ratio_count_gaules.rds"))

  obtenu <- ratio_count_Gaules(Ratio=Ratio, Rec=Rec, RecGaules=RecGaules, t=5, RandomPlacGaules=RandomPlacGaules, Para.ratio_gaules=Para.ratio_gaules)

  expect_equal(obtenu, attendu)

})


test_that("ratio_Gaules() fonctionne avec 2 placettes", {


  Para.ratio_gaules <- readRDS(test_path("fixtures/gaules_evolution", "Para.ratio_gaules.rds"))
  RecGaules <- readRDS(test_path("fixtures/gaules_evolution", "RecGaules_count_gaules.rds"))
  Rec <- readRDS(test_path("fixtures/gaules_evolution", "Rec_count_gaules.rds"))
  RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))
  Ratio <- readRDS(test_path("fixtures/gaules_evolution", "Ratio_count_gaules.rds"))
  Para.nb_gaules <- readRDS(test_path("fixtures/gaules_evolution", "Para.nb_gaules.rds"))
  attendu <- readRDS(test_path("fixtures/gaules_evolution", "attendu_ratio_gaules.rds"))

  obtenu <- ratio_Gaules(Ratio=Ratio, Rec=Rec, RecGaules=RecGaules, t=5, RandomPlacGaules=RandomPlacGaules, Para.nb_gaules=Para.nb_gaules, Para.ratio_gaules=Para.ratio_gaules)

  expect_equal(obtenu, attendu)

})



test_that("pi68XXX() et count68XXX fonctionnent avec 2 placettes", {

  RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))

  attendu_pi68BOJ <- readRDS(test_path("fixtures/gaules_evolution", "attendu_pi68BOJ.rds"))
  attendu_pi68ERS <- readRDS(test_path("fixtures/gaules_evolution", "attendu_pi68ERS.rds"))
  attendu_pi68HEG <- readRDS(test_path("fixtures/gaules_evolution", "attendu_pi68HEG.rds"))
  attendu_pi68SAB <- readRDS(test_path("fixtures/gaules_evolution", "attendu_pi68SAB.rds"))

  attendu_count68BOJ <- readRDS(test_path("fixtures/gaules_evolution", "attendu_count68BOJ.rds"))
  attendu_count68ERS <- readRDS(test_path("fixtures/gaules_evolution", "attendu_count68ERS.rds"))
  attendu_count68HEG <- readRDS(test_path("fixtures/gaules_evolution", "attendu_count68HEG.rds"))
  attendu_count68SAB <- readRDS(test_path("fixtures/gaules_evolution", "attendu_count68SAB.rds"))

  RecGaules <- readRDS(test_path("fixtures/gaules_evolution", "RecGaules_68_gaules.rds"))
  Rec <- readRDS(test_path("fixtures/gaules_evolution", "Rec_68_gaules.rds"))
  Ratio <- readRDS(test_path("fixtures/gaules_evolution", "Ratio_68_gaules.rds"))

  Para.68_BOJ <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_BOJ.rds"))
  Para.68_ERS <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_ERS.rds"))
  Para.68_HEG <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_HEG.rds"))
  Para.68_SAB <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_SAB.rds"))


  obtenu_pi68BOJ <- pi68BOJ(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_BOJ=Para.68_BOJ)
  obtenu_pi68ERS <- pi68ERS(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_ERS=Para.68_ERS)
  obtenu_pi68HEG <- pi68HEG(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_HEG=Para.68_HEG)
  obtenu_pi68SAB <- pi68SAB(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_SAB=Para.68_SAB)

  obtenu_count68BOJ <- count68BOJ(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, t=5, RandomPlacGaules=RandomPlacGaules, Para.68_BOJ=Para.68_BOJ)
  obtenu_count68ERS <- count68ERS(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_ERS=Para.68_ERS)
  obtenu_count68HEG <- count68HEG(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_HEG=Para.68_HEG)
  obtenu_count68SAB <- count68SAB(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_SAB=Para.68_SAB)


  expect_equal(obtenu_pi68BOJ, attendu_pi68BOJ)
  expect_equal(obtenu_pi68ERS, attendu_pi68ERS)
  expect_equal(obtenu_pi68HEG, attendu_pi68HEG)
  expect_equal(obtenu_pi68SAB, attendu_pi68SAB)

  expect_equal(obtenu_count68BOJ, attendu_count68BOJ)
  expect_equal(obtenu_count68ERS, attendu_count68ERS)
  expect_equal(obtenu_count68HEG, attendu_count68HEG)
  expect_equal(obtenu_count68SAB, attendu_count68SAB)

})




test_that("gaules_68() fonctionne avec 2 placettes", {

RecGaules <- readRDS(test_path("fixtures/gaules_evolution", "RecGaules_68_gaules.rds"))
Rec <- readRDS(test_path("fixtures/gaules_evolution", "Rec_68_gaules.rds"))
Ratio <- readRDS(test_path("fixtures/gaules_evolution", "Ratio_68_gaules.rds"))
RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))

Para.68_BOJ <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_BOJ.rds"))
Para.68_ERS <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_ERS.rds"))
Para.68_HEG <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_HEG.rds"))
Para.68_SAB <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_SAB.rds"))

attendu <- readRDS(test_path("fixtures/gaules_evolution", "attendu_gaules_68.rds"))


obtenu <- gaules_68(Ratio=Ratio, Rec=Rec, RecGaules=RecGaules, t=5, RandomPlacGaules=RandomPlacGaules,
                     Para.68_ERS=Para.68_ERS, Para.68_HEG=Para.68_HEG, Para.68_BOJ=Para.68_BOJ, Para.68_SAB=Para.68_SAB)

expect_equal(obtenu, attendu)

})
