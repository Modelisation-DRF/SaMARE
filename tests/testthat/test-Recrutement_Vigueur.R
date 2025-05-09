# test_that(" test-Recrutement_Vigueur", {
#
#  RecSelect_test <- readRDS(test_path("fixtures", "RecSelect_test.rds"))
#  Para.rec_vig_test <- readRDS(test_path("fixtures", "Para.rec_vig_test.rds"))
#  Result <-rec_vig(RecSelect =RecSelect_test,latitude=46.7,Iterj =  1L,Para.rec_vig =  Para.rec_vig_test)
#
#  Result<-data.frame(V1=Result)
#  Result<- Result %>% mutate(V1= round(V1,7))
#
#  expectResult_vigeur_test <- readRDS(test_path("fixtures", "expectResult_vigeur_test.rds"))
#  expectResult <-data.frame(V1=expectResult_vigeur_test)
#  expectResult<-expectResult%>% mutate(V1= round(V1,7))
#
#   expect_equal(Result, expectResult)
# })


test_that("Test rec_vig() - test Junior", {

  attendu <- readRDS(test_path("fixtures/recrue_vig", "expectResult_vigeur_test.rds")) # fichier préparé par Junior
  data <- readRDS(test_path("fixtures/recrue_vig", "Rec_test_vig.rds")) # fichier préparé par Junior
  parms <- readRDS(test_path("fixtures/recrue_vig", "Para.rec_vig_test.rds")) # fichier préparé par Junior
  rand <- readRDS(test_path("fixtures/recrue_vig", "RandomRec.rds"))


  obtenu <- rec_vig(data, parms, rand, seed_value = 10)

  compare <- bind_cols(obtenu, attendu)

  expect_equal(compare$xb_rec_vig, compare$V1)


})


test_that("Test rec_vig()", {

  attendu <- readRDS(test_path("fixtures/recrue_vig", "rec_vig_attendu.rds")) # fichier préparé par Junior
  data <- readRDS(test_path("fixtures/recrue_vig", "Rec_test_vig.rds")) # fichier préparé par Junior
  parms <- readRDS(test_path("fixtures/recrue_vig", "Para.rec_vig_test.rds")) # fichier préparé par Junior
  rand <- readRDS(test_path("fixtures/recrue_vig", "RandomRec.rds"))


  obtenu <- rec_vig(data, parms, rand, seed_value = 10)

  expect_equal(obtenu, attendu)


})
