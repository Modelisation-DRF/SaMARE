test_that("SortieDendroIterSamare return the expected data frame ", {
  expect_equal(SortieDendroIterSamare(SimulHtVol = expect_for_Simulateur_Samare_sans_gaules_et_coupe_test), expect_for_sortie_dendroIter_samare_test )
})
