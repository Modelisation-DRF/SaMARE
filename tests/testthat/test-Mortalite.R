test_that("La fonction mort() fonctionne correctement avec MCH=0", {

  data <- readRDS(test_path("fixtures/mortalite", "data_test_module.rds"))

  attendu <- readRDS(test_path("fixtures/mortalite", "mort_attendu_mch0.rds"))
  parm <- readRDS(test_path("fixtures/mortalite", "Para.mort.rds"))
  random <- readRDS(test_path("fixtures/mortalite", "RandomMort.rds"))

  # t=5 #fichier interne

  obtenu <- mort(data, t, MCH=0 , parm, random, seed_value=10)

  expect_equal(obtenu, attendu)

})

test_that("La fonction mort() fonctionne correctement avec MCH=1", {

  data <- readRDS(test_path("fixtures/mortalite", "data_test_module.rds"))

  attendu <- readRDS(test_path("fixtures/mortalite", "mort_attendu_mch1.rds"))
  parm <- readRDS(test_path("fixtures/mortalite", "Para.mort.rds"))
  random <- readRDS(test_path("fixtures/mortalite", "RandomMort.rds"))

  # t=5 #fichier interne

  obtenu <- mort(data, t, MCH=1 , parm, random, seed_value=10)

  expect_equal(obtenu, attendu)

})

test_that("La fonction mort() fonctionne correctement avec MCH=1 comparaison", {

  data <- readRDS(test_path("fixtures/mortalite", "data_test_module.rds"))

  parm <- readRDS(test_path("fixtures/mortalite", "Para.mort.rds"))
  random <- readRDS(test_path("fixtures/mortalite", "RandomMort.rds"))

  # t=5 #fichier interne

  obtenu0 <- mort(data, t, MCH=0 , parm, random, seed_value=10)
  obtenu1 <- mort(data, t, MCH=1 , parm, random, seed_value=10)

  # les lignes qui ne sont pas HEG devraient être pareilles
  obtenu0_sans_heg <- obtenu0 %>% filter(GrEspece != 'HEG')
  obtenu1_sans_heg <- obtenu0 %>% filter(GrEspece != 'HEG')

  expect_equal(obtenu0_sans_heg, obtenu1_sans_heg)

})


