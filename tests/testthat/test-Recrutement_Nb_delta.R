test_that("Recrutement_Nb_delta return the expected data", {

  rec_lam_test <- readRDS(test_path("fixtures", "rec_lam_test.rds"))
  Para.rec_n_for_test <- readRDS(test_path("fixtures", "Para.rec_n_for_test.rds"))

  Result<-rec_delta(Rec=rec_lam_test,type_pe_Plac="type0",st_tot0 =21.5900825754271,ntrt=1,t0_aj_=2.1,Iterj=1L,
                    Para.rec_n =Para.rec_n_for_test)

  Result<-data.frame(V1=Result)
  Result<- Result %>% mutate(V1= round(V1,7))

  Result_rec_delta_for_test <- readRDS(test_path("fixtures", "Result_rec_delta_for_test.rds"))
  expected <-data.frame(V1=Result_rec_delta_for_test )
  expected <-expected%>% mutate(V1= round(V1,7))

  expect_equal(Result, expected)
})
