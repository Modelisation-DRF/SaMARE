test_that("SaMARE returns the expected data frame without Gaules", {
  set.seed(NULL)
  set.seed(3)
  ListeItertest <- data.frame(PlacetteID ="TEM23APC5000_1",
                          Placette = "TEM23APC5000",
                          Iter = 1)


  result_simul2<-SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA, ListeIter=ListeItertest, AnneeDep=2023, Horizon = 6 ,
                    RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                    Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat)

set.seed(NULL)
  expect_equal(result_simul2, result_samare_sans_gaules_test)
})


test_that("SaMARE returns the expected data frame with Gaules", {
  set.seed(NULL)
  set.seed(3)
  ListeItertest <- data.frame(PlacetteID ="TEM23APC5000_1",
                              Placette = "TEM23APC5000",
                              Iter = 1)


  result_simul<-SaMARE(Random =RandomTest,RandomGaules=RandPlacStepGaules_test, Data = Data_test_for_simul_samare, Gaules =Gaules_test, ListeIter=ListeItertest, AnneeDep=2023, Horizon = 6 ,
                        RecruesGaules =1,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                        Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat)

  set.seed(NULL)
  expect_equal(result_simul, result_samare_avec_gaules_test)
})
