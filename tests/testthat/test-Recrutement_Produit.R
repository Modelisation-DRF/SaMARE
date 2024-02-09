test_that("test-Recrutement_Produit work", {

  Result <-rec_prod(RecSelect =RecSelect_test,type_pe_Plac="type0",rid1="3b",Iterj =  1L,Para.rec_prod =  Para.rec_prod_test)
  Result<-data.frame(V1=Result)
  Result<- Result %>% mutate(V1= round(V1,7))


  expectResult <-data.frame(V1=expectResult_Produit_test)
  expectResult<-expectResult%>% mutate(V1= round(V1,7))

  expect_equal(Result, expectResult)
})
