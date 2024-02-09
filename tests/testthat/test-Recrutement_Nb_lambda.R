test_that(" Recrutement_Nb_lambda return the expected data ", {

  Result<-rec_lambda(Rec=rec_lam_test,type_pe_Plac="type0",st_tot0 =21.5900825754271,t =5,Iterj=1L,Para.rec_n =Para.rec_n_for_test)
  Result<-data.frame(V1=Result)
  Result<- Result %>% mutate(V1= round(V1,7))

  expected <-data.frame(V1=expectResult_Recrutement_Nb_lambda_test )
  expected <-expected%>% mutate(V1= round(V1,7))

  expect_equal(Result, expected )
})
