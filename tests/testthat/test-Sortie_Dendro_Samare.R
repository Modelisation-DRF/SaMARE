test_that("SortieDendroSamare return the expected data frame ", {
  expect_equal(SortieDendroSamare(SimulHtVol = expect_for_Simulateur_Samare_sans_gaules_et_coupe_test), expect_for_sortie_dendro_samare_test )
})
