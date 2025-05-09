# vérification de mscr avec la version de junior
test_that("AttribMSCR() fonctionne correctement avec les 10 groupes d'essences", {
  # le fichier des paramètres est directement dans la fonctions, sans stochastique, il lit le fichier interne
  # il faut un data généré à la fin de la boucle des pas de simulation à l'intérieur de la fct SaMARE
  data <- readRDS(test_path("fixtures/mscr_attrib", "data_test.rds"))
  Para <- readRDS(test_path("fixtures/mscr_attrib", "param.rds"))
  obtenu = AttribMSCR(Data=data, Para.ConvVigMSCR=Para, seed_value=3)
  obtenu <- obtenu %>% select(-contains("pred"), -contains("prob"), -Alea)

  attendu <- readRDS(test_path("fixtures/mscr_attrib", "resultat_attendu_version_junior.rds"))

  expect_equal(obtenu$MSCR, attendu$MSCR)

})

# vérification des prob
test_that("AttribMSCR() fonctionne correctement avec les 10 groupes d'essences", {
  # le fichier des paramètres est directement dans la fonctions, sans stochastique, il lit le fichier interne
  # il faut un data généré à la fin de la boucle des pas de simulation à l'intérieur de la fct SaMARE
  data <- readRDS(test_path("fixtures/mscr_attrib", "data_test.rds"))
  Para <- readRDS(test_path("fixtures/mscr_attrib", "param.rds"))
  obtenu = AttribMSCR(Data=data, Para.ConvVigMSCR=Para, seed_value=3)

  attendu <- readRDS(test_path("fixtures/mscr_attrib", "resultat_attendu_prob.rds"))

  expect_equal(obtenu, attendu)

})
