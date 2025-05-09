# test_that("test-Recrutement_Produit work", {
#
#   RecSelect_test <- readRDS(test_path("fixtures", "RecSelect_test.rds"))
#   Para.rec_prod_test <- readRDS(test_path("fixtures", "Para.rec_prod_test.rds"))
#
#   Result <-rec_prod(RecSelect =RecSelect_test,type_pe_Plac="type0",rid1="3b",Iterj =  1L,Para.rec_prod =  Para.rec_prod_test)
#   Result<-data.frame(V1=Result)
#   Result<- Result %>% mutate(V1= round(V1,7))
#
#   expectResult_Produit_test <- readRDS(test_path("fixtures", "expectResult_Produit_test.rds"))
#   expectResult <-data.frame(V1=expectResult_Produit_test)
#   expectResult<-expectResult%>% mutate(V1= round(V1,7))
#
#   expect_equal(Result, expectResult)
# })

test_that("Test rec_prod() - test Junior", {

  attendu <- readRDS(test_path("fixtures/recrue_prod", "expectResult_produit_test.rds")) # fichier préparé par Junior
  data <- readRDS(test_path("fixtures/recrue_prod", "Rec_test_prod.rds")) # fichier préparé par Junior
  parms <- readRDS(test_path("fixtures/recrue_prod", "Para.rec_prod_test.rds")) # fichier préparé par Junior
  rand <- readRDS(test_path("fixtures/recrue_prod", "RandomRec.rds"))


  obtenu <- rec_prod(data, parms, rand, seed_value = 10)

  compare <- bind_cols(obtenu, attendu)

  expect_equal(compare$xb_rec_prod, compare$V1)


})




test_that("Test rec_prod()", {

  attendu <- readRDS(test_path("fixtures/recrue_prod", "rec_prod_attendu.rds")) # fichier préparé par Junior
  data <- readRDS(test_path("fixtures/recrue_prod", "Rec_test_prod.rds")) # fichier préparé par Junior
  parms <- readRDS(test_path("fixtures/recrue_prod", "Para.rec_prod_test.rds")) # fichier préparé par Junior
  rand <- readRDS(test_path("fixtures/recrue_prod", "RandomRec.rds"))

  obtenu <- rec_prod(data, parms, rand, seed_value = 10)

  expect_equal(obtenu, attendu)


})
