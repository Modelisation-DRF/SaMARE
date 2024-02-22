
test_that("Parametres_Simul_SaMARE retrun a list of parametter", {

  List_Para_result <- list(MatchModuleCovparms,EffetCovParms, CovparmGaules, MatchModuleParameters, ParametresGaules, Species, SpeciesGroups, MatchSpeciesGroups, MatchModuleOmega, OmegaGaulesFormat )

  expect_equal(ParametresSimulSaMARE(),List_Para_result)

})
