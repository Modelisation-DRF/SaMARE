
test_that("SortieArbreSamare return the expected data frame", {
  expect_equal(SortieArbreSamare(Test_Simul), expect_for_arbre_samare)
})
