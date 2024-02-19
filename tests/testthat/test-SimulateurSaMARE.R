test_that("simulateur SaMARE return the expected data with Gaules", {
  set.seed(NULL)
  set.seed(3)

  Result<-SimulSaMARE(NbIter=2,AnneeDep=2023, Horizon=6,RecruesGaules=1, Data = Test400m2Coupe , Gaules = GaulesTest2500m2)

  set.seed(NULL)

  expect_equal(Result, expect_for_Simulateur_Samare_test )
})


test_that("simulateur SaMARE return the expected data without Gaules", {
  set.seed(NULL)
  set.seed(3)

  Result<-SimulSaMARE(NbIter=2,AnneeDep=2023, Horizon=6,RecruesGaules=0, Data = Test400m2Coupe )

  set.seed(NULL)

  expect_equal(Result, expect_for_Simulateur_Samare_sans_gaules_test  )
})
