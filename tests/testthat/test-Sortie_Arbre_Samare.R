test_that("SortieArbreSamare return the expected data frame ", {

  expect_equal(SortieArbreSamare(expect_for_Simulateur_Samare_sans_gaules_et_coupe_test), expect_for_dortie_arbre_samare_test )
})
