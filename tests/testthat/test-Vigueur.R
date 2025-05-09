test_that("La fonction vig() fonctionne correctement", {

  data <- readRDS(test_path("fixtures/vigueur", "data_test_module_vig.rds"))
  attendu <- readRDS(test_path("fixtures/vigueur", "vig_attendu.rds"))
  param <- readRDS(test_path("fixtures/vigueur", "Para.vig.rds"))
  random <- readRDS(test_path("fixtures/vigueur", "RandomVig.rds"))

  #t=5

  obtenu <- vig(data, param, random, seed_value = 10)

  expect_equal(obtenu, attendu)

})


test_that("La fonction vig() fonctionne correctement pour les morts", {

  data <- readRDS(test_path("fixtures/vigueur", "data_test_module_vig.rds"))
  param <- readRDS(test_path("fixtures/vigueur", "Para.vig.rds"))
  random <- readRDS(test_path("fixtures/vigueur", "RandomVig.rds"))

  #t=5


  obtenu <- vig(data, param, random, seed_value = 10)

  obt_mort <- obtenu %>% filter(Etat1=='mort', !is.na(vigu1))

  expect_equal(nrow(obt_mort), 0)

})
