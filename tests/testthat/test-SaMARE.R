test_that("SaMARE returns the expected data frame without Gaules and MCH = 0", {
  set.seed(NULL)
  set.seed(3)
  ListeItertest <- data.frame(PlacetteID ="TEM23APC5000_1",
                          Placette = "TEM23APC5000",
                          Iter = 1)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures", "Data_test_for_simul_samare.rds"))
  RandomTest <- readRDS(test_path("fixtures", "RandomTest.rds"))
  result_simul2<-SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA, ListeIter=ListeItertest, AnneeDep=2023,
                    Horizon = 6 ,
                    RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                    Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)

  result_samare_sans_gaules_test <- readRDS(test_path("fixtures", "result_samare_sans_gaules_test.rds"))
  set.seed(NULL)
  expect_equal(result_simul2, result_samare_sans_gaules_test)
})


test_that("SaMARE returns the expected data frame without Gaules and MCH = 1", {
  set.seed(NULL)
  set.seed(3)
  ListeItertest <- data.frame(PlacetteID ="TEM23APC5000_1",
                              Placette = "TEM23APC5000",
                              Iter = 1)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures", "Data_test_for_simul_samare.rds"))
  RandomTest <- readRDS(test_path("fixtures", "RandomTest.rds"))
  result_simul2<-SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA, ListeIter=ListeItertest,
                        AnneeDep=2023, Horizon = 6 ,
                        RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                        Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 1)

  result_samare_sans_gaules_test_MCH <- readRDS(test_path("fixtures", "result_samare_sans_gaules_test_MCH.rds"))
  set.seed(NULL)
  expect_equal(result_simul2, result_samare_sans_gaules_test_MCH)
})


test_that("SaMARE returns the expected data frame with Gaules MCH=0", {
  set.seed(NULL)
  set.seed(3)
  ListeItertest <- data.frame(PlacetteID ="TEM23APC5000_1",
                              Placette = "TEM23APC5000",
                              Iter = 1)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures", "Data_test_for_simul_samare.rds"))
  RandomTest <- readRDS(test_path("fixtures", "RandomTest.rds"))
  RandPlacStepGaules_test <- readRDS(test_path("fixtures", "RandPlacStepGaules_test.rds"))
  Gaules_test <- readRDS(test_path("fixtures", "Gaules_test.rds"))
  result_simul<-SaMARE(Random =RandomTest,RandomGaules=RandPlacStepGaules_test, Data = Data_test_for_simul_samare,
                       Gaules =Gaules_test, ListeIter=ListeItertest, AnneeDep=2023, Horizon = 6 ,
                        RecruesGaules =1,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                        Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)

  set.seed(NULL)
  result_samare_avec_gaules_test <- readRDS(test_path("fixtures", "result_samare_avec_gaules_test.rds"))
  expect_equal(result_simul, result_samare_avec_gaules_test)
})


test_that("SaMARE returns the expected data frame with Gaules MCH=1", {
  set.seed(NULL)
  set.seed(3)
  ListeItertest <- data.frame(PlacetteID ="TEM23APC5000_1",
                              Placette = "TEM23APC5000",
                              Iter = 1)

  RandomTest <- readRDS(test_path("fixtures", "RandomTest.rds"))
  RandPlacStepGaules_test <- readRDS(test_path("fixtures", "RandPlacStepGaules_test.rds"))
  Data_test_for_simul_samare <- readRDS(test_path("fixtures", "Data_test_for_simul_samare.rds"))
  Gaules_test <- readRDS(test_path("fixtures", "Gaules_test.rds"))
  result_simul<-SaMARE(Random =RandomTest,RandomGaules=RandPlacStepGaules_test, Data = Data_test_for_simul_samare,
                       Gaules =Gaules_test, ListeIter=ListeItertest, AnneeDep=2023, Horizon = 6 ,
                       RecruesGaules =1,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                       Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 1)

  set.seed(NULL)
  result_samare_avec_gaules_test_MCH <- readRDS(test_path("fixtures", "result_samare_avec_gaules_test_MCH.rds"))
  expect_equal(result_simul, result_samare_avec_gaules_test_MCH)
})
