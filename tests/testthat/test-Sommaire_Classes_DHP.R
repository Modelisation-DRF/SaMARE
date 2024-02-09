test_that("Sommaire_Classes_DHP return the expected data frame", {

  som <- Sommaire_Classes_DHP(Test_Simul, 2)
  som <- som %>%
    mutate(across(where(is.numeric), ~ round(., 6)))%>% ungroup()

  sommaire_ <- expect_for_arbre_sommaire_classes_DHP %>%
    mutate(across(where(is.numeric), ~ round(., 6)))%>% ungroup()

  expect_equal(som, sommaire_)

})
