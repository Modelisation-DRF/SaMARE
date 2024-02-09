test_that("", {

  Result=rec_dhp(RecSelect=RecSelect_test ,st_tot0 = 27.6458293053024,dens_tot0 =453.325,t=5,ntrt =1,Iterj=1L,Para.rec_dhp=Para.rec_dhp_for_test)

  Result<-data.frame(V1=Result)
  Result<- Result %>% mutate(V1= round(V1,7))


  expectResult <-data.frame(V1=Result_Recrutement_Dhp_test)
  expectResult<-expectResult%>% mutate(V1= round(V1,7))

  expect_equal(Result, expectResult)
})
