test_that("La fonction AttribQualFct() fonctionne correctement", {

  data <- readRDS(test_path("fixtures/qualite_attrib", "data_test_module_qual.rds"))
  attendu <- readRDS(test_path("fixtures/qualite_attrib", "qual_attendu.rds"))


  obtenu <- AttribQualFct(data, seed_value = 10)
  expect_equal(obtenu, attendu)


  # les arbres feuillus qui ont dépassé le seuil de 23 ont tous une qualité
  seuil23 <- obtenu %>% filter(DHPcm==23 & DHPcm1>=23.1 & is.na(ABCD_orig) & Etat1=='vivant' & GrEspece %in% c("BOJ","ERR","ERS","FEN","FIN","HEG"))
  seuil23 <- seuil23 %>% filter(is.na(ABCD))
  expect_equal(nrow(seuil23), 0)

  # pas de qualité pour essences pas feuilus
  ess_qual <- obtenu %>% filter(!GrEspece %in% c("BOJ","ERR","ERS","FEN","FIN","HEG"))
  ess_qual <- ess_qual %>% filter(!is.na(ABCD))
  expect_equal(nrow(ess_qual), 0)

})



test_that("La fonction AttribQualFct() fonctionne correctement pour les morts", {

  data <- readRDS(test_path("fixtures/qualite_attrib", "data_test_module_qual.rds"))
  attendu <- readRDS(test_path("fixtures/qualite_attrib", "qual_attendu.rds"))

  obtenu <- AttribQualFct(data, seed_value = 10)

  obt_mort <- obtenu %>% filter(Etat1=='mort', !is.na(ABCD))

  expect_equal(nrow(obt_mort), 0)

})

# test d'un fichier où il n'y a pas d'attribution a faire
test_that("La fonction AttribQualFct() fonctionne correctement si pas d'attribution de qualite a faire", {

  data <- readRDS(test_path("fixtures/qualite_attrib", "data_test_module_qual.rds"))
  # aucun arbre n'a transitionné au seuil de 23.1
  data <- data %>% mutate(ABCD=NA,
                          DHPcm1 = ifelse(Etat1=='vivant', 22, DHPcm1),
                          DHPcm = 19)


  obtenu <- AttribQualFct(data, seed_value = 10)

  expect_equal(obtenu, data)

})


