test_that("La fonction AttribProd0() fonctionne correctement", {

  Plac_test <- readRDS(test_path("fixtures/Vigu0_Prod0", "Plac_test.rds"))

  Para.ConvMSCRProd1024 <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRProd1024.rds"))
  Para.ConvMSCRProd24 <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRProd24.rds"))

  obtenu <- AttribProd0(Plac_test,Para.ConvMSCRProd1024,Para.ConvMSCRProd24)

  expect_equal(nrow(obtenu), nrow(Plac_test))
  expect_equal(ncol(obtenu), ncol(Plac_test)+1)

})

