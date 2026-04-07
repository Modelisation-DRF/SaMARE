test_that("La fonction AttribVigu0() fonctionne correctement", {

  Plac_test <- readRDS(test_path("fixtures/Vigu0_Prod0", "Plac_test.rds"))
  Para.ConvMSCRVig <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRVig.rds"))

  obtenu <- AttribVigu0(Plac_test, Para.ConvMSCRVig)

  expect_equal(nrow(obtenu), nrow(Plac_test))
  expect_equal(ncol(obtenu), ncol(Plac_test)+1)

})

