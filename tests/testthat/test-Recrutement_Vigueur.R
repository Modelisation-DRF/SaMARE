test_that(" test-Recrutement_Vigueur", {

 Result <-rec_vig(RecSelect =RecSelect_test,latitude=46.7,Iterj =  1L,Para.rec_vig =  Para.rec_vig_test)

 Result<-data.frame(V1=Result)
 Result<- Result %>% mutate(V1= round(V1,7))
 expectResult <-data.frame(V1=expectResult_vigeur_test)
 expectResult<-expectResult%>% mutate(V1= round(V1,7))

  expect_equal(Result, expectResult)
})
