test_that("La fonction AttribProd0() fonctionne correctement sans vigueur", {

  set.seed(NULL)
  set.seed(3)

  Para.1024 <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRProd1024.rds"))
  Para.24 <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRProd24.rds"))
  Plac_test <- readRDS(test_path("fixtures/Vigu0_Prod0", "Fic_test_complet.rds"))
  attendu <- readRDS(test_path("fixtures/Vigu0_Prod0", "resultat_attendu_prod.rds"))
  #attendu2 <- attendu %>% dplyr::select(-prod0)

  obtenu_prod <- AttribProd0(Plac_test,Para.1024,Para.24)
  #obtenu2 <- obtenu_prod %>% dplyr::select(-prod0)

  # vérifier les résultats de la valeur prédite non transformée en sciage/pate
  expect_equal(obtenu_prod, attendu)


  # les resineux ont tous prod0=resineux
  res <- obtenu_prod %>% filter(GrEspece %in% c('RES','EPX','SAB')) %>% dplyr::select(prod0) %>% unique()
  expect_equal(nrow(res), 1)
  expect_equal(res$prod0, 'resineux')

  # les feuillus ont tous sciage ou pate
  feu <- obtenu_prod %>% filter(!GrEspece %in% c('RES','EPX','SAB'))
  feu_unique <- feu %>%  dplyr::select(prod0) %>% unique()
  expect_equal(nrow(feu_unique), 2)

  set.seed(NULL)

})

test_that("La fonction AttribProd0() fonctionne correctement avec vigueur", {

  Para.1024 <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRProd1024.rds"))
  Para.24 <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRProd24.rds"))
  Plac_test <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds"))
  # ajouter des resineux avec vigueur 5 et 6
  res1 <- Plac_test %>% filter(GrEspece=='HEG') %>% mutate(GrEspece='SAB', Vigueur=5)
  res2 <- Plac_test %>% filter(GrEspece=='ERS') %>% mutate(GrEspece='EPX', Vigueur=6)
  res3 <- Plac_test %>% filter(GrEspece=='ERS') %>% mutate(GrEspece='RES', Vigueur=6)
  Plac_test <- bind_rows(Plac_test, res1, res2, res3)

  obtenu <- AttribProd0(Plac_test,Para.1024,Para.24)

  obtenu1 <- obtenu %>% mutate(verif = ifelse(Vigueur %in% c(1,3) & prod0=="sciage", 0,
                                             ifelse(Vigueur %in% c(2,4) & prod0=="pate", 0,
                                                    ifelse(GrEspece %in% c('EPX','RES','SAB') & prod0=="resineux", 0,
                                                           1)))) %>%
    select(GrEspece, DHPcm, Vigueur, prod0, verif) %>%
    summarise(nb=sum(verif))

  expect_equal(obtenu1$nb, 0)


})


test_that("La fonction AttribProd0() fonctionne correctement sans vigueur sans mscr", {

  Para.1024 <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRProd1024.rds"))
  Para.24 <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRProd24.rds"))
  Plac_test <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
    mutate(Vigueur=NA, MSCR=NA)

  obtenu <- AttribProd0(Plac_test,Para.1024,Para.24)

  expect_equal(obtenu$prod0, rep('pate',18))
  expect_equal(ncol(obtenu), ncol(Plac_test)+3)


})



