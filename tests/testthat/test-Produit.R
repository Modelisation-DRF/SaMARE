test_that("La fonction produit() fonctionne correctement", {

  data <- readRDS(test_path("fixtures/produit", "data_test_module_prod.rds"))
  attendu <- readRDS(test_path("fixtures/produit", "prod_attendu.rds"))
  param <- readRDS(test_path("fixtures/produit", "Para.prod.rds"))
  random <- readRDS(test_path("fixtures/produit", "RandomProd.rds"))

  # t=5 #fichier interne

  obtenu <- produit(data, param, random, seed_value = 10)

  expect_equal(obtenu, attendu)

})


test_that("La fonction produit() fonctionne correctement pour les morts", {

  data <- readRDS(test_path("fixtures/produit", "data_test_module_prod.rds"))
  param <- readRDS(test_path("fixtures/produit", "Para.prod.rds"))
  random <- readRDS(test_path("fixtures/produit", "RandomProd.rds"))

  # t=5 #fichier interne

  obtenu <- produit(data, param, random, seed_value = 10)

  obt_mort <- obtenu %>% filter(Etat1=='mort', !is.na(prod1))  # tous les morts doivent avoir NA

  expect_equal(nrow(obt_mort), 0)

})

test_that("La fonction produit() fonctionne correctement pour les resineux", {

  data <- readRDS(test_path("fixtures/produit", "data_test_module_prod.rds"))
  param <- readRDS(test_path("fixtures/produit", "Para.prod.rds"))
  random <- readRDS(test_path("fixtures/produit", "RandomProd.rds"))

  # t=5 #fichier interne

  obtenu <- produit(data, param, random, seed_value = 10)

  obt_res <- obtenu %>% filter(Etat1!='mort' & prod0=='resineux')
  obt_res <- obt_res %>% filter(prod1 != 'resineux')

  # tous les morts doivent avoir NA

  expect_equal(nrow(obt_res), 0)

})

test_that("La fonction produit() fonctionne correctement pour les AUT et < 23cm", {

  data <- readRDS(test_path("fixtures/produit", "data_test_module_prod.rds"))
  param <- readRDS(test_path("fixtures/produit", "Para.prod.rds"))
  random <- readRDS(test_path("fixtures/produit", "RandomProd.rds"))

  # t=5 #fichier interne

  obtenu <- produit(data, param, random, seed_value = 10)

  obt_aut <- obtenu %>% filter(Etat1!='mort' & prod0!='resineux' & GrEspece=='AUT') # doivent tous être pate
  obt_aut <- obt_aut %>% filter(prod1 != 'pate')

  obt_23 <- obtenu %>% filter(Etat1!='mort' & prod0!='resineux' & vigu0=="NONVIG" & DHPcm1<23.1) # doivent tous être pate
  obt_23 <- obt_23 %>% filter(prod1 != 'pate')


  expect_equal(nrow(obt_aut), 0)
  expect_equal(nrow(obt_23), 0)

})
