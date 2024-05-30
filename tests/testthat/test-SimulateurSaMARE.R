#
# test_that("simulateur SaMARE return the expected data without Gaules and coupe MCH=0", {
#   set.seed(NULL)
#   set.seed(3)
#
#   Result<-SimulSaMARE(NbIter=2,Horizon=6,RecruesGaules=0, Data = Test2500m2 ,MCH = 0)
#
#   set.seed(NULL)
#
#   expect_equal(Result, expect_for_Simulateur_Samare_avec_coupe_sans_gaules_test  )
# })






test_that("simulateur SaMARE return the expected data with Gaules and coupe MCH=0", {
  set.seed(NULL)
  set.seed(3)

  Result<-SimulSaMARE(NbIter=2, Horizon=6,RecruesGaules=1, Data = Test400m2Coupe , Gaules = GaulesTest2500m2,MCH = 0)

  set.seed(NULL)

  expect_equal(Result, expect_test_for_Simulateur_Samare )
})

test_that("simulateur SaMARE return the expected data with Gaules and coupe MCH=1", {
  set.seed(NULL)
  set.seed(3)

  Result<-SimulSaMARE(NbIter=2, Horizon=6,RecruesGaules=1, Data = Test400m2Coupe , Gaules = GaulesTest2500m2,MCH = 1)

  set.seed(NULL)

  expect_equal(Result, expect_test_for_Simulateur_Samare_MCH )
})

test_that("simulateur SaMARE return the expected data with gaules and without Gaules MCH=0", {
  set.seed(NULL)
  set.seed(3)

  Result<-SimulSaMARE(NbIter=2, Horizon=6,RecruesGaules=0, Data = Test400m2Coupe ,MCH = 0)

  set.seed(NULL)

  expect_equal(Result, expect_for_Simulateur_Samare_sans_gaules_et_coupe_test  )
})

test_that("simulateur SaMARE return the expected data with gaules and without Gaules MCH=1", {
  set.seed(NULL)
  set.seed(3)

  Result<-SimulSaMARE(NbIter=2, Horizon=6,RecruesGaules=0, Data = Test400m2Coupe ,MCH = 1)

  set.seed(NULL)

  expect_equal(Result, expect_for_Simulateur_Samare_sans_gaules_et_coupe_test_MCH  )
})

test_that("simulateur SaMARE return the expected data with Gaules and without coupe MCH=0", {
  set.seed(NULL)
  set.seed(3)

  Result<-SimulSaMARE(NbIter=2, Horizon=6,RecruesGaules=1, Data = Test2500m2 , Gaules = GaulesTest2500m2,MCH = 0)

  set.seed(NULL)

  expect_equal(Result, expect_for_Simulateur_Samare_avec_gaules_et_coupe_test )
})

test_that("simulateur SaMARE return the expected data with Gaules and without coupe MCH=1", {
  set.seed(NULL)
  set.seed(3)

  Result<-SimulSaMARE(NbIter=2, Horizon=6,RecruesGaules=1, Data = Test2500m2 , Gaules = GaulesTest2500m2,MCH = 1)

  set.seed(NULL)

  expect_equal(Result, expect_for_Simulateur_Samare_avec_gaules_et_coupe_test_MCH )
})

test_that("simulateur SaMARE return the expected data without Gaules and coupe MCH=1", {
  set.seed(NULL)
  set.seed(3)

  Result<-SimulSaMARE(NbIter=2,Horizon=6,RecruesGaules=0, Data = Test2500m2 ,MCH = 1)

  set.seed(NULL)

  expect_equal(Result, expect_for_Simulateur_Samare_avec_coupe_sans_gaules_test_MCH  )
})
