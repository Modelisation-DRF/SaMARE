# test de junior
test_that("rec_pi_Gaules return the expected data test junior", {

  # fichier des info placette et sur les arbres marchands
  Rec_test_gaules <- readRDS(test_path("fixtures/recrues_avec_gaules", "Rec_test_gaules.rds")) %>% mutate(st_tot0=21.5900825754271, Placette='TEM23APC5000')

  # fichier des info sur le nombre de gaules
  RecGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RecGaules_test.rds")) %>% mutate(Placette='TEM23APC5000')

  # fichier des effets aléatoires
  RandomPlacGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RandomPlacGaules_test.rds"))

  # fichier des paramètres
  Para.rec_gaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "Para.rec_gaules_test.rds"))


  Result <- rec_pi_Gaules(Rec=Rec_test_gaules, RecGaules=RecGaules_test, t=5, RandomPlacGaules=RandomPlacGaules_test, Para.rec_gaules=Para.rec_gaules_test)
  Result<- Result %>% mutate(V1= round(predPi_nbrec,7))

  expect_Result_test_Recrutement_Nb_pi_Gaules <- readRDS(test_path("fixtures/recrues_avec_gaules", "expect_Result_Nb_pi_Gaules.rds"))
  expected <-data.frame(V1=expect_Result_test_Recrutement_Nb_pi_Gaules)
  expected <-expected%>% mutate(V1= round(V1,7))
  expect_equal(Result$V1, expected$V1)

})

# tester  rec_pi_Gaules avec 2 placettes
test_that("rec_pi_Gaules fonctionne avec 2 placettes", {

  # fichier des info placette et sur les arbres marchands
  Rec_test_gaules <- readRDS(test_path("fixtures/recrues_avec_gaules", "Rec_test_gaules.rds")) %>% mutate(st_tot0=21.5900825754271, Placette='TEM23APC5000')
  Rec_test_gaules2 <- Rec_test_gaules %>% mutate(Placette='TEM23APC5001')
  Rec_test_gaules <- bind_rows(Rec_test_gaules, Rec_test_gaules2)

  # fichier des info sur le nombre de gaules
  RecGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RecGaules_test.rds")) %>% mutate(Placette='TEM23APC5000')
  RecGaules_test2 <- RecGaules_test %>% mutate(Placette='TEM23APC5001')
  RecGaules_test <- bind_rows(RecGaules_test, RecGaules_test2)

  # fichier des effets aléatoires de placettes
  RandomPlacGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RandomPlacGaules_test.rds"))
  RandomPlacGaules_test2 <- RandomPlacGaules_test %>% mutate(Placette='TEM23APC5001')
  RandomPlacGaules_test <- bind_rows(RandomPlacGaules_test, RandomPlacGaules_test2)

  # fichier des paramètres
  Para.rec_gaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "Para.rec_gaules_test.rds"))

  Result <- rec_pi_Gaules(Rec=Rec_test_gaules, RecGaules=RecGaules_test, t=5, RandomPlacGaules=RandomPlacGaules_test, Para.rec_gaules=Para.rec_gaules_test)
  Result<- Result %>% mutate(V1= round(predPi_nbrec,7))

  expect_Result_test_Recrutement_Nb_pi_Gaules <- readRDS(test_path("fixtures/recrues_avec_gaules", "expect_Result_Nb_pi_Gaules.rds"))
  expected <-data.frame(V1=expect_Result_test_Recrutement_Nb_pi_Gaules)
  expected <- bind_rows(expected, expected) # doubler les résultats, car j'ai copié 2 fois la même placette
  expected <-expected %>% mutate(V1= round(V1,7))
  expect_equal(Result$V1, expected$V1)

})

# test de junior
test_that("rec_count_Gaules return the expected data", {

  # fichier des info placette et sur les arbres marchands
  Rec_test_gaules <- readRDS(test_path("fixtures/recrues_avec_gaules", "Rec_test_gaules.rds")) %>% mutate(st_tot0=21.5900825754271, Placette='TEM23APC5000')

  # fichier des info sur le nombre de gaules
  RecGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RecGaules_test.rds")) %>% mutate(Placette='TEM23APC5000')

  # fichier des effets aléatoires de placettes
  RandomPlacGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RandomPlacGaules_test.rds"))

  # fichier des paramètres
  Para.rec_gaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "Para.rec_gaules_test.rds"))

  Result <- rec_count_Gaules(Rec=Rec_test_gaules, RecGaules=RecGaules_test,t=5, RandomPlacGaules=RandomPlacGaules_test, Para.rec_gaules=Para.rec_gaules_test)
  Result<- Result %>% mutate(V1= round(predCount_nbrec,7))

  expect_Result_test_Recrutement_Nb_count_Gaules <- readRDS(test_path("fixtures/recrues_avec_gaules", "expect_Result_Nb_count_Gaules.rds"))
  expected <-data.frame(V1=expect_Result_test_Recrutement_Nb_count_Gaules )
  expected <-expected%>% mutate(V1= round(V1,7))

  expect_equal(Result$V1, expected$V1)
})

# tester  rec_count_Gaules avec 2 placettes
test_that("rec_count_Gaules fonctionne avec 2 placettes", {

  # fichier des info placette et sur les arbres marchands
  Rec_test_gaules <- readRDS(test_path("fixtures/recrues_avec_gaules", "Rec_test_gaules.rds")) %>% mutate(st_tot0=21.5900825754271, Placette='TEM23APC5000')
  Rec_test_gaules2 <- Rec_test_gaules %>% mutate(Placette='TEM23APC5001')
  Rec_test_gaules <- bind_rows(Rec_test_gaules, Rec_test_gaules2)

  # fichier des info sur le nombre de gaules
  RecGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RecGaules_test.rds")) %>% mutate(Placette='TEM23APC5000')
  RecGaules_test2 <- RecGaules_test %>% mutate(Placette='TEM23APC5001')
  RecGaules_test <- bind_rows(RecGaules_test, RecGaules_test2)

  # fichier des effets aléatoires de placettes
  RandomPlacGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RandomPlacGaules_test.rds"))
  RandomPlacGaules_test2 <- RandomPlacGaules_test %>% mutate(Placette='TEM23APC5001')
  RandomPlacGaules_test <- bind_rows(RandomPlacGaules_test, RandomPlacGaules_test2)

  # fichier des paramètres
  Para.rec_gaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "Para.rec_gaules_test.rds"))

  Result <- rec_count_Gaules(Rec=Rec_test_gaules, RecGaules=RecGaules_test,t=5, RandomPlacGaules=RandomPlacGaules_test, Para.rec_gaules=Para.rec_gaules_test)
  Result<- Result %>% mutate(V1= round(predCount_nbrec,7))

  expect_Result_test_Recrutement_Nb_count_Gaules <- readRDS(test_path("fixtures/recrues_avec_gaules", "expect_Result_Nb_count_Gaules.rds"))
  expected <-data.frame(V1=expect_Result_test_Recrutement_Nb_count_Gaules )
  expected <- bind_rows(expected, expected) # doubler les résultats, car j'ai copié 2 fois la même placette
  expected <-expected %>% mutate(V1= round(V1,7))

  expect_equal(Result$V1, expected$V1)

})


# test de rec_n_gaules avec 2 placettes
test_that("rec_n_gaules fonctionne avec 2 placettes", {

# fichier des info placette et sur les arbres marchands
Rec_test_gaules <- readRDS(test_path("fixtures/recrues_avec_gaules", "Rec_test_gaules.rds")) %>% mutate(st_tot0=21.5900825754271, Placette='TEM23APC5000')
Rec_test_gaules2 <- Rec_test_gaules %>% mutate(Placette='TEM23APC5001')
Rec_test_gaules <- bind_rows(Rec_test_gaules, Rec_test_gaules2)

# fichier des info sur le nombre de gaules
RecGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RecGaules_test.rds")) %>% mutate(Placette='TEM23APC5000')
RecGaules_test2 <- RecGaules_test %>% mutate(Placette='TEM23APC5001')
RecGaules_test <- bind_rows(RecGaules_test, RecGaules_test2)

# fichier des effets aléatoires de placettes
RandomPlacGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RandomPlacGaules_test.rds"))
RandomPlacGaules_test2 <- RandomPlacGaules_test %>% mutate(Placette='TEM23APC5001')
RandomPlacGaules_test <- bind_rows(RandomPlacGaules_test, RandomPlacGaules_test2)

# fichier des paramètres
Para.rec_gaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "Para.rec_gaules_test.rds"))


set.seed(NULL)
Result <- rec_n_Gaules(Rec=Rec_test_gaules, RecGaules=RecGaules_test, t=5, CovParmsGaules=CovparmGaules, RandomPlacGaules=RandomPlacGaules_test, Para.rec_gaules=Para.rec_gaules_test, seed_value=3)
set.seed(NULL)

attendu <- readRDS(test_path("fixtures/recrues_avec_gaules", "resultat_attendu_nb.rds"))

expect_equal(Result, attendu)

})
