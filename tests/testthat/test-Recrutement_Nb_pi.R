test_that("multiplication works", {

  Result<-rec_pi(Rec=Rec_test,5,36.75691,1,27.1,"type0",1L,Para.rec_n_for_test)
  Result<-data.frame(V1=Result)
  Result<- Result %>% mutate(V1= round(V1,7))

  expect_equal(Result, expectResult_test_Recrutement_Nb_pi)
})


