test_that("cubage_arbres fonctionne comme attendu sans gaules", {

  Simul <- readRDS(test_path("fixtures/cubage_arbres", "Simul.rds"))
  Data <- readRDS(test_path("fixtures/cubage_arbres", "Data_test.rds"))
  attendu <- readRDS(test_path("fixtures/cubage_arbres", "attendu_sans_gaules.rds"))

  obtenu <- cubage_arbres(data=Data, simul_data=Simul, NbIter=3, Horizon=3)

  # meme nombre de ligne que l'intrant
  expect_equal(nrow(obtenu), nrow(Simul))

  # 2 colonnes de plus ht et vol
  noms_attendus <-c(names(Simul), c('hauteur_pred','vol_dm3'))
  diff <- setdiff(noms_attendus, names(obtenu))
  expect_equal(length(diff),0)


  # essence à manquant
  ess_na <- obtenu %>% filter(Espece %in% c("AME","AUR","ERE","ERG","ERP","MAS","PRP","SAL","SOA","SOD"))
  ess_na <- ess_na%>% filter(!is.na(vol_dm3))
  expect_equal(nrow(ess_na), 0)

})

test_that("cubage_arbres fonctionne comme attendu avec gaules", {

  Simul <- readRDS(test_path("fixtures/cubage_arbres", "Simul_avec_gaules.rds"))
  Data <- readRDS(test_path("fixtures/cubage_arbres", "Data_test_avec_gaules.rds"))
  attendu <- readRDS(test_path("fixtures/cubage_arbres", "attendu_avec_gaules.rds"))

  obtenu <- cubage_arbres(data=Data, simul_data=Simul, NbIter=3, Horizon=3)

  # meme nombre de ligne que l'intrant
  expect_equal(nrow(obtenu), nrow(Simul))

  # 2 colonnes de plus ht et vol
  noms_attendus <-c(names(Simul), c('hauteur_pred','vol_dm3'))
  diff <- setdiff(noms_attendus, names(obtenu))
  expect_equal(length(diff),0)


})
