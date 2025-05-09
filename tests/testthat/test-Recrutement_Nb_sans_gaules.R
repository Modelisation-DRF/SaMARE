
test_that("La fonction rec_pi() fonctionne correctement - test Junior", {

  param <- readRDS(test_path("fixtures/recrue_nb_pi", "Para.rec_n_for_test.rds")) # fichier préparé par Junior
  attendu <- readRDS(test_path("fixtures/recrue_nb_pi", "expectResult_test_Recrutement_Nb_pi.rds")) # fichier préparé par Junior
  data <- readRDS(test_path("fixtures/recrue_nb_pi", "data_test_rec_pi.rds"))
  RandomRec <- readRDS(test_path("fixtures/recrue_nb_pi", "RandomRec.rds"))

  # t=5 #fichier interne
  obtenu <- rec_pi(Rec=data,t,param, RandomRec)

  compare <- bind_cols(obtenu, attendu)

  expect_equal(round(compare$xb_prob_rec, 5), round(compare$V1,5))

})


test_that("La fonction rec_pi() fonctionne correctement", {

  param <- readRDS(test_path("fixtures/recrue_nb_pi", "Para.rec_n_for_test.rds")) # fichier préparé par Junior
  attendu <- readRDS(test_path("fixtures/recrue_nb_pi", "rec_pi_attendu.rds"))
  data <- readRDS(test_path("fixtures/recrue_nb_pi", "data_test_rec_pi.rds"))
  RandomRec <- readRDS(test_path("fixtures/recrue_nb_pi", "RandomRec.rds"))

  # t=5 #fichier interne
  obtenu <- rec_pi(Rec=data,t,param, RandomRec)

  expect_equal(obtenu, attendu)

})


test_that("La fonction rec_delta() fonctionne correctement test Junior", {

  param <- readRDS(test_path("fixtures/recrue_nb_delta", "Para.rec_n_for_test.rds")) # fichier préparé par Junior
  attendu <- readRDS(test_path("fixtures/recrue_nb_delta", "Result_rec_delta_for_test.rds")) # fichier préparé par Junior
  data <- readRDS(test_path("fixtures/recrue_nb_delta", "Rec_test_delta.rds"))


  obtenu <- rec_delta(Rec=data, param)

  attendu <- as.data.frame(attendu)
  compare <- bind_cols(obtenu, attendu)

  expect_equal(compare$xb_delta_rec, compare$V1)

})


test_that("La fonction rec_delta() fonctionne correctement", {

  param <- readRDS(test_path("fixtures/recrue_nb_delta", "Para.rec_n_for_test.rds"))
  attendu <- readRDS(test_path("fixtures/recrue_nb_delta", "rec_delta_attendu.rds")) # mon fichier
  data <- readRDS(test_path("fixtures/recrue_nb_delta", "Rec_test_delta.rds"))


  obtenu <- rec_delta(Rec=data, param)

  expect_equal(obtenu, attendu)

})




test_that("La fonction rec_lambda() fonctionne correctement - test Junior", {

  param <- readRDS(test_path("fixtures/recrue_nb_lambda", "Para.rec_n_for_test.rds")) # fichier préparé par Junior
  attendu <- readRDS(test_path("fixtures/recrue_nb_lambda", "expectResult.rds")) # fichier préparé par Junior
  data <- readRDS(test_path("fixtures/recrue_nb_lambda", "Rec_test_lambda.rds"))

  # t=5 #fichier interne

  obtenu <- rec_lambda(Rec=data,t,param)

  attendu <- as.data.frame(attendu)
  compare <- bind_cols(obtenu, attendu)

  expect_equal(compare$xb_lambda_rec, compare$V1)

})


test_that("La fonction rec_lambda() fonctionne correctement", {

  param <- readRDS(test_path("fixtures/recrue_nb_lambda", "Para.rec_n_for_test.rds"))
  attendu <- readRDS(test_path("fixtures/recrue_nb_lambda", "rec_lambda_attendu.rds"))
  data <- readRDS(test_path("fixtures/recrue_nb_lambda", "Rec_test_lambda.rds"))

  # t=5 #fichier interne

  obtenu <- rec_lambda(Rec=data,t,param)

  expect_equal(obtenu, attendu)

})



test_that("Test rec_nb()", {

  attendu <- readRDS(test_path("fixtures/recrue_nb", "rec_nb_attendu.rds")) # fichier préparé par Junior
  data <- readRDS(test_path("fixtures/recrue_nb", "Rec.rds")) # fichier préparé par Junior
  parms <- readRDS(test_path("fixtures/recrue_nb", "Para.rec_n.rds")) # fichier préparé par Junior
  rand <- readRDS(test_path("fixtures/recrue_nb", "RandomRec.rds"))

  # t=5 #fichier interne
  obtenu <- rec_n(data, t, parms, rand, seed_value = 10)

  expect_equal(obtenu, attendu)


})

