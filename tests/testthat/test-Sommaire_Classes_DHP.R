test_that("Sommaire_Classes_DHP return the expected data frame", {

  expect_for_Samare_sans_gaules_et_coupe_test <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds"))
  som <- Sommaire_Classes_DHP(expect_for_Samare_sans_gaules_et_coupe_test)
  som <- som %>%
    mutate(across(where(is.numeric), ~ round(., 6)))%>% ungroup()

  expect_for_arbre_sommaire_classes_DHP <- readRDS(test_path("fixtures", "expect_for_arbre_sommaire_classes_DHP.rds"))
  sommaire_ <- expect_for_arbre_sommaire_classes_DHP %>%
    mutate(across(where(is.numeric), ~ round(., 6)))%>% ungroup()

  expect_equal(som, sommaire_)

})
