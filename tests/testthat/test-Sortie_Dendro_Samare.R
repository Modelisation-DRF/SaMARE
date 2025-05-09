test_that("SortieDendroSamare return the expected data frame ", {

  # n'est pas utilisé car n'est pas pas pareil à celui obtenu
  #attendu <- readRDS(test_path("fixtures", "expect_for_sortie_dendro_samare_test.rds"))

  # simulation avec la première version de la fonction
  simul_v1 <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds"))
  obtenu_dendro_v1 = SortieDendroSamare_ancien(simul_v1)
  # simul_v1$Sup_PE 0.04

  # simulation avec la nouvelle version
  simul_v2 <- simul_v1 %>% mutate(Temps = Annee - 2025) %>% rename(Vol_dm3 = vol_dm3, Hautm=hauteur_pred)
  obtenu_dendro_v2 = SortieDendroSamare(simul_v2)

  #expect_equal(SortieDendroSamare_ancien(simul_v1), attendu)

  # vérifier que la nouvelles version donne les mêmes résultats que l'ancienne
  expect_equal(obtenu_dendro_v2$Ti_ha, obtenu_dendro_v1$nbTi_HA)
  expect_equal(obtenu_dendro_v2$ST_m2ha, obtenu_dendro_v1$ST_HA)
  expect_equal(obtenu_dendro_v2$DQM_cm, obtenu_dendro_v1$DQM)
  expect_equal(obtenu_dendro_v2$Vol_m3ha, obtenu_dendro_v1$Vol_HA)
  #expect_equal(obtenu_dendro_v2$HDom_m, obtenu_dendro_v1$HDomM) # HDomM ne sera pas pareil car n'est plus calculé de la même façon
  expect_equal(obtenu_dendro_v2$EcartType_Ti_HA, obtenu_dendro_v1$EcartType_nbTi_HA)
  expect_equal(obtenu_dendro_v2$EcartType_ST_HA, obtenu_dendro_v1$EcartType_ST_HA)
  expect_equal(obtenu_dendro_v2$EcartType_DQM, obtenu_dendro_v1$EcartType_DQM)
  expect_equal(obtenu_dendro_v2$EcartType_Vol_HA, obtenu_dendro_v1$EcartType_Vol_HA)
  #expect_equal(obtenu_dendro_v2$EcartType_HDom, obtenu_dendro_v1$EcartType_HDom)
  expect_equal(obtenu_dendro_v2$AAC_Survie_m2haan, obtenu_dendro_v1$AACAccrM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Mort_m2haan, obtenu_dendro_v1$AACMortM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Recrue_m2haan, obtenu_dendro_v1$AACRecrM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Brut_m2haan, obtenu_dendro_v1$AACBrut)
  expect_equal(obtenu_dendro_v2$AAC_Net_m2haan, obtenu_dendro_v1$AACNet)

})


test_that("SortieDendroSamare fonctionne avec le parametre simplifier", {

  # simulation avec la première version de la fonction
  simul_v1 <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds"))
  obtenu_dendro_v1 = SortieDendroSamare_ancien(simul_v1, simplifier = T)

  # simulation avec la nouvelle version
  simul_v2 <- simul_v1 %>% mutate(Temps = Annee - 2025) %>% rename(Vol_dm3 = vol_dm3, Hautm=hauteur_pred)
  obtenu_dendro_v2 = SortieDendroSamare(simul_v2, simplifier = T)

  expect_equal(obtenu_dendro_v2$Ti_ha, obtenu_dendro_v1$nbTi_HA)
  expect_equal(obtenu_dendro_v2$ST_m2ha, obtenu_dendro_v1$ST_HA)
  expect_equal(obtenu_dendro_v2$DQM_cm, obtenu_dendro_v1$DQM)
  expect_equal(obtenu_dendro_v2$Vol_m3ha, obtenu_dendro_v1$Vol_HA)
  #expect_equal(obtenu_dendro_v2$HDom_m, obtenu_dendro_v1$HDomM) # HDomM ne sera pas pareil car n'est plus calculé de la même façon
  expect_equal(obtenu_dendro_v2$EcartType_Ti_HA, obtenu_dendro_v1$EcartType_nbTi_HA)
  expect_equal(obtenu_dendro_v2$EcartType_ST_HA, obtenu_dendro_v1$EcartType_ST_HA)
  expect_equal(obtenu_dendro_v2$EcartType_DQM, obtenu_dendro_v1$EcartType_DQM)
  expect_equal(obtenu_dendro_v2$EcartType_Vol_HA, obtenu_dendro_v1$EcartType_Vol_HA)
  #expect_equal(obtenu_dendro_v2$EcartType_HDom, obtenu_dendro_v1$EcartType_HDom)
  expect_equal(obtenu_dendro_v2$AAC_Survie_m2haan, obtenu_dendro_v1$AACAccrM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Mort_m2haan, obtenu_dendro_v1$AACMortM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Recrue_m2haan, obtenu_dendro_v1$AACRecrM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Brut_m2haan, obtenu_dendro_v1$AACBrut)
  expect_equal(obtenu_dendro_v2$AAC_Net_m2haan, obtenu_dendro_v1$AACNet)

})



test_that("SortieDendroSamare fonctionne avec le parametre simplifier et une sortie directe", {

  # data à la fin de la simulation de samare
  simul <- SimulSaMARE(NbIter = 2, Horizon = 4, Data = Test2500m2)
  # simul$Sup_PE 0.25
  obtenu_dendro_v2 <- SortieDendroSamare(simul, simplifier = T)

  list_an <- unique(obtenu_dendro_v2$Temps)
  expect_equal(list_an, c(0,20))

  simul_v1 <- simul %>% rename(vol_dm3=Vol_dm3, hauteur_pred=Hautm)
  obtenu_dendro_v1 = SortieDendroSamare_ancien(simul_v1, simplifier = T)

  expect_equal(obtenu_dendro_v2$Ti_ha, obtenu_dendro_v1$nbTi_HA)
  expect_equal(obtenu_dendro_v2$ST_m2ha, obtenu_dendro_v1$ST_HA)
  expect_equal(obtenu_dendro_v2$DQM_cm, obtenu_dendro_v1$DQM)
  expect_equal(obtenu_dendro_v2$Vol_m3ha, obtenu_dendro_v1$Vol_HA)
  #expect_equal(obtenu_dendro_v2$HDom_m, obtenu_dendro_v1$HDomM) # HDomM ne sera pas pareil car n'est plus calculé de la même façon
  expect_equal(obtenu_dendro_v2$EcartType_Ti_HA, obtenu_dendro_v1$EcartType_nbTi_HA)
  expect_equal(obtenu_dendro_v2$EcartType_ST_HA, obtenu_dendro_v1$EcartType_ST_HA)
  expect_equal(obtenu_dendro_v2$EcartType_DQM, obtenu_dendro_v1$EcartType_DQM)
  expect_equal(obtenu_dendro_v2$EcartType_Vol_HA, obtenu_dendro_v1$EcartType_Vol_HA)
  #expect_equal(obtenu_dendro_v2$EcartType_HDom, obtenu_dendro_v1$EcartType_HDom)

  expect_equal(obtenu_dendro_v2$AAC_Survie_m2haan, obtenu_dendro_v1$AACAccrM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Mort_m2haan, obtenu_dendro_v1$AACMortM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Recrue_m2haan, obtenu_dendro_v1$AACRecrM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Brut_m2haan, obtenu_dendro_v1$AACBrut)
  expect_equal(obtenu_dendro_v2$AAC_Net_m2haan, obtenu_dendro_v1$AACNet)

})

test_that("SortieDendroSamare fonctionne avec une sortie directe", {

  # data à la fin de la simulation de samare
  simul <- SimulSaMARE(NbIter = 2, Horizon = 4, Data = Test2500m2)
  # simul$Sup_PE 0.25
  obtenu_dendro_v2 <- SortieDendroSamare(simul)

  simul_v1 <- simul %>% rename(vol_dm3=Vol_dm3, hauteur_pred=Hautm)
  obtenu_dendro_v1 = SortieDendroSamare_ancien(simul_v1)

  expect_equal(obtenu_dendro_v2$Ti_ha, obtenu_dendro_v1$nbTi_HA)
  expect_equal(obtenu_dendro_v2$ST_m2ha, obtenu_dendro_v1$ST_HA)
  expect_equal(obtenu_dendro_v2$DQM_cm, obtenu_dendro_v1$DQM)
  expect_equal(obtenu_dendro_v2$Vol_m3ha, obtenu_dendro_v1$Vol_HA)
  #expect_equal(obtenu_dendro_v2$HDom_m, obtenu_dendro_v1$HDomM) # HDomM ne sera pas pareil car n'est plus calculé de la même façon
  expect_equal(obtenu_dendro_v2$EcartType_Ti_HA, obtenu_dendro_v1$EcartType_nbTi_HA)
  expect_equal(obtenu_dendro_v2$EcartType_ST_HA, obtenu_dendro_v1$EcartType_ST_HA)
  expect_equal(obtenu_dendro_v2$EcartType_DQM, obtenu_dendro_v1$EcartType_DQM)
  expect_equal(obtenu_dendro_v2$EcartType_Vol_HA, obtenu_dendro_v1$EcartType_Vol_HA)
  #expect_equal(obtenu_dendro_v2$EcartType_HDom, obtenu_dendro_v1$EcartType_HDom)
  expect_equal(obtenu_dendro_v2$AAC_Survie_m2haan, obtenu_dendro_v1$AACAccrM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Mort_m2haan, obtenu_dendro_v1$AACMortM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Recrue_m2haan, obtenu_dendro_v1$AACRecrM2HaAn)
  expect_equal(obtenu_dendro_v2$AAC_Brut_m2haan, obtenu_dendro_v1$AACBrut)
  expect_equal(obtenu_dendro_v2$AAC_Net_m2haan, obtenu_dendro_v1$AACNet)

})



# avec une simul sans cubage
test_that("SortieDendroSamare fonctionne sans cubage", {

  # data à la fin de la simulation de samare
  simul <- SimulSaMARE(NbIter = 2, Horizon = 4, Data = Test2500m2, cubage = F)

  # fonctionne sans erreur
  expect_no_error(SortieDendroSamare(simul))

  obtenu <- SortieDendroSamare(simul)
  verif_vol <- obtenu %>% filter(!is.na(Vol_m3ha))
  verif_et <- obtenu %>% filter(!is.na(EcartType_Vol_HA))
  expect_equal(nrow(verif_vol), 0)
  expect_equal(nrow(verif_et), 0)

})



