
test_that("Billonage ABCD", {

  expect_equal(SIMBillonnageABCD_DHP(data=expect_for_Simulateur_Samare_test, type = "ABCD"), Billonage_ABCD_test)
})

test_that("Billonage DHP", {

  expect_equal(SIMBillonnageABCD_DHP(data=expect_for_Simulateur_Samare_test, type = "DHP"), Billonage_ABCD_test)
})

test_that("Billonage 1234", {

  expect_equal(c<-SIMBillonnageABCD_DHP(data=expect_for_Simulateur_Samare_test, type = "1234"), Billonage_1234_test)
})

test_that("Billonage MSCR", {

  expect_equal(SIMBillonnageABCD_DHP(data=expect_for_Simulateur_Samare_test, type = "MSCR"), Billonage_MSCR_test)
})
