test_that("La fonction accrois() fonctionne correctement", {

  data <- readRDS(test_path("fixtures/accroissement", "data_test_module_acc.rds"))

  attendu <- readRDS(test_path("fixtures/accroissement", "acc_attendu.rds"))
  param <- readRDS(test_path("fixtures/accroissement", "Para.acc.rds"))
  random <- readRDS(test_path("fixtures/accroissement", "RandomAcc.rds"))
  resid <- readRDS(test_path("fixtures/accroissement", "Res.rds"))

  #t=5 #fichier interne

  obtenu <- accrois(data, t, param, random, resid)

  expect_equal(obtenu, attendu)

})

test_that("La fonction accrois() fonctionne correctement pour les morts", {

  data <- readRDS(test_path("fixtures/accroissement", "data_test_module_acc.rds"))

  param <- readRDS(test_path("fixtures/accroissement", "Para.acc.rds"))
  random <- readRDS(test_path("fixtures/accroissement", "RandomAcc.rds"))
  resid <- readRDS(test_path("fixtures/accroissement", "Res.rds"))

  #t=5 #fichier interne


  obtenu <- accrois(data, t, param, random, resid)

  obt_mort <- obtenu %>% filter(Etat1=='mort', !is.na(DHPcm1))

  expect_equal(nrow(obt_mort), 0)

})
