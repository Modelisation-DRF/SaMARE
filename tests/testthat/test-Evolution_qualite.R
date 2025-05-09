test_that("La fonction EvolQual() fonctionne correctement", {

  data <- readRDS(test_path("fixtures/qualite_evol", "data_test_module_qual.rds"))
  attendu <- readRDS(test_path("fixtures/qualite_evol", "qual_attendu.rds")) %>% select(-ABCD_orig)
  param <- readRDS(test_path("fixtures/qualite_evol", "Para.qual.rds"))


  obtenu <- EvolQual(data, param, seed_value = 10)
  expect_equal(obtenu, attendu)

  # tous les ABCD manquants au temps 0 sont aussi manquant au temps 1
  # sans_abcd <- obtenu %>% filter(is.na(ABCD_orig)) # tous les ABCD manquants au temps 0 sont aussi manquant au temps 1
  # sans_abcd <- sans_abcd %>% filter(!is.na(ABCD))
  # expect_equal(nrow(sans_abcd), 0)

  # les arbres qui ont dépasser les seuils de 33 et 39 (le seuil de 23 est traiter dans une autre fonction)
  #seuil33 <- obtenu %>% filter(DHPcm==33 & DHPcm1==33.2 & !is.na(ABCD_orig) & Etat1=='vivant')
  #seuil39 <- obtenu %>% filter(DHPcm==39 & DHPcm1==39.2 & !is.na(ABCD_orig) & Etat1=='vivant')


  # pas de qualité pour essences pas feuilus
  ess_qual <- obtenu %>% filter(!GrEspece %in% c("BOJ","ERR","ERS","FEN","FIN","HEG"))
  ess_qual <- ess_qual %>% filter(!is.na(ABCD))
  expect_equal(nrow(ess_qual), 0)

})




test_that("La fonction EvolQual() fonctionne correctement pour les morts", {

  data <- readRDS(test_path("fixtures/qualite_evol", "data_test_module_qual.rds"))
  param <- readRDS(test_path("fixtures/qualite_evol", "Para.qual.rds"))


  obtenu <- EvolQual(data, param, seed_value = 10)

  obt_mort <- obtenu %>% filter(Etat1=='mort', !is.na(ABCD))

  expect_equal(nrow(obt_mort), 0)

})

# test d'un fichier où il n'y a pas d'evolution a faire
test_that("La fonction EvolQual() fonctionne correctement si pas d'evolution de qualite a faire", {

  data <- readRDS(test_path("fixtures/qualite_evol", "data_test_module_qual.rds"))
  data <- data %>% mutate(ABCD=NA)
  param <- readRDS(test_path("fixtures/qualite_evol", "Para.qual.rds"))

  obtenu <- EvolQual(data, param, seed_value = 10)

  expect_equal(obtenu, data)

})


