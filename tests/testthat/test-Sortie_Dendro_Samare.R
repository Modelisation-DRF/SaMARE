test_that("SortieDendroSamare return the expected data frame ", {

  expect_for_Samare_sans_gaules_et_coupe_test <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds"))
  expect_for_sortie_dendro_samare_test <- readRDS(test_path("fixtures", "expect_for_sortie_dendro_samare_test.rds"))

  expect_equal(SortieDendroSamare(SimulHtVol = expect_for_Samare_sans_gaules_et_coupe_test), expect_for_sortie_dendro_samare_test )
})


