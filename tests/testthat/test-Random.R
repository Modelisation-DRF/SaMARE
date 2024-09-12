test_that("RandomPlacStepGaules return the expected data frame ", {
  set.seed(NULL)
  set.seed(3)

  RandPlacStepGaules <-RandomPlacStepGaules(CovparmGaules,GaulesTest2500m2,2)
  set.seed(NULL)
  expected_for_RandomPlacStepGaules <- readRDS(test_path("fixtures", "expected_for_RandomPlacStepGaules.rds"))
  expect_equal(RandPlacStepGaules , expected_for_RandomPlacStepGaules)
})
