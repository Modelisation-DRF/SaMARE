# test_that("Test Recrutement_Dhp", {
#
#   RecSelect_test <- readRDS(test_path("fixtures", "RecSelect_test.rds"))
#   Para.rec_dhp_for_test <- readRDS(test_path("fixtures", "Para.rec_dhp_for_test.rds"))
#
#   Result=rec_dhp(RecSelect=RecSelect_test ,st_tot0 = 27.6458293053024,dens_tot0 =453.325,t=5,ntrt =1,Iterj=1L,
#                  Para.rec_dhp=Para.rec_dhp_for_test)
#
#   Result<-data.frame(V1=Result)
#   Result<- Result %>% mutate(V1= round(V1,7))
#
#
#   Result_Recrutement_Dhp_test <- readRDS(test_path("fixtures", "Result_Recrutement_Dhp_test.rds"))
#   expectResult <-data.frame(V1=Result_Recrutement_Dhp_test)
#   expectResult<-expectResult%>% mutate(V1= round(V1,7))
#
#   expect_equal(Result, expectResult)
# })


test_that("Test Recrutement_Dhp - test Junior", {

  attendu <- readRDS(test_path("fixtures/recrue_dhp", "Result_Recrutement_Dhp_test.rds")) # fichier préparé par Junior
  data <- readRDS(test_path("fixtures/recrue_dhp", "Rec_test_dhp.rds")) # fichier préparé par Junior
  parms <- readRDS(test_path("fixtures/recrue_dhp", "Para.rec_dhp_for_test.rds")) # fichier préparé par Junior
  rand <- readRDS(test_path("fixtures/recrue_dhp", "RandomRec.rds"))
  theta <- readRDS(test_path("fixtures/recrue_dhp", "theta.rds"))
  varRecDhp <- readRDS(test_path("fixtures/recrue_dhp", "varRecDhp.rds"))

  # t=5 #fichier interne

  obtenu <- rec_dhp(RecSelect=data, t, parms, rand, varRecDhp, theta)

  attendu <- as.data.frame(attendu)

  compare <- bind_cols(obtenu, attendu)

  expect_equal(compare$pred_dhp, compare$V1)


})

test_that("Test Recrutement_Dhp", {

  attendu <- readRDS(test_path("fixtures/recrue_dhp", "rec_dhp_attendu.rds")) # fichier préparé par Junior
  data <- readRDS(test_path("fixtures/recrue_dhp", "Rec_test_dhp.rds")) # fichier préparé par Junior
  parms <- readRDS(test_path("fixtures/recrue_dhp", "Para.rec_dhp_for_test.rds")) # fichier préparé par Junior
  rand <- readRDS(test_path("fixtures/recrue_dhp", "RandomRec.rds"))
  theta <- readRDS(test_path("fixtures/recrue_dhp", "theta.rds"))
  varRecDhp <- readRDS(test_path("fixtures/recrue_dhp", "varRecDhp.rds"))

  # t=5 #fichier interne
  obtenu <- rec_dhp(RecSelect=data, t, parms, rand, varRecDhp, theta, seed_value = 10)

  expect_equal(obtenu, attendu)

  # les dhp des recrues sont >=9.1
  dhp9 <- obtenu %>% filter(DHPcm1<9.1)
  expect_equal(nrow(dhp9), 0)

})
