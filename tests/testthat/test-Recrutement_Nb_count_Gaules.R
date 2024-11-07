test_that("Recrutement_Nb_count_Gaules return the expected data", {

  Rec_test_gaules <- readRDS(test_path("fixtures", "Rec_test_gaules.rds"))
  RecGaules_test <- readRDS(test_path("fixtures", "RecGaules_test.rds"))
  RandomPlacGaules_test <- readRDS(test_path("fixtures", "RandomPlacGaules_test.rds"))
  Para.rec_gaules_test <- readRDS(test_path("fixtures", "Para.rec_gaules_test.rds"))

  Result<-rec_count_Gaules(Rec=Rec_test_gaules, RecGaules=RecGaules_test,t=5,st_tot0=21.5900825754271,Iterj=1,
                           RandomPlacGaules=RandomPlacGaules_test, Para.rec_gaules =Para.rec_gaules_test)
  Result<-data.frame(V1=Result)
  Result<- Result %>% mutate(V1= round(V1,7))

  expect_Result_test_Recrutement_Nb_count_Gaules <- readRDS(test_path("fixtures", "expect_Result_test_Recrutement_Nb_count_Gaules.rds"))
  expected <-data.frame(V1=expect_Result_test_Recrutement_Nb_count_Gaules )
  expected <-expected%>% mutate(V1= round(V1,7))

  expect_equal(Result, expected )
})
