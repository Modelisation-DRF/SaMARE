test_that("La fonction SortieBillesFusion() fonctionne comme attendu", {

  fic <- SimulSaMARE(NbIter = 10, Horizon = 2, Data = Test2500m2)
  fic <- fic[1:16,] %>% dplyr::select(-Espece, -GrEspece) %>% mutate(DHPcm=34, Hautm=20) # une seule iter/placette/annee
  Espece   <- c("EPN", "BOJ", "ERR", "BOP", "HEG", "CHR", "CHG", "CHB", "CHE", "SAB",    NA,    NA,    NA,    NA,    NA,  NA)
  GrEspece <- c("RES", "BOJ", "ERR", "FIN", "HEG", "FEN", "FEN", "FEN", "FEN", "SAB", "ERS", "BOJ", "ERR", "HEG", "FEN", "SAB")

  fic$Espece <- Espece
  fic$GrEspece <- GrEspece
  result <-  SortieBillesFusion(Data=fic, Type='DHP', nom_grade1 = 'pate', long_grade1 = 8, diam_grade1 = 9,
                                nom_grade2 = NA, long_grade2 = NA, diam_grade2 = NA,
                                nom_grade3 = NA, long_grade3 = NA, diam_grade3 = NA, Simplifier = FALSE)


  # nombre de colonnes est le nb du fichier en entrée + 2 = 32
  expect_equal(ncol(result),ncol(fic)+2)

  # il y a des valeurs de petro et de sybille
  obtenu <- result %>% select(grade_bille) %>% unique()
  expect_equal(nrow(obtenu),8)

})

test_that("La fonction SortieBillesFusion() gère le cas sans essence sybille", {

  fic <- SimulSaMARE(NbIter = 10, Horizon = 2, Data = Test2500m2)
  fic <- fic[1:2,] %>% dplyr::select(-Espece, -GrEspece) %>% mutate(DHPcm=34, Hautm=20) # une seule iter/placette/annee
  Espece   <- c("ERR", "ERR")
  GrEspece <- c("ERR", "ERR")
  fic$Espece <- Espece
  fic$GrEspece <- GrEspece
  expect_no_error(SortieBillesFusion(Data=fic, Type='DHP', nom_grade1 = 'pate', long_grade1 = 8, diam_grade1 = 9))


})

test_that("La fonction SortieBillesFusion() gère le cas sans essence petro", {

  fic <- SimulSaMARE(NbIter = 10, Horizon = 2, Data = Test2500m2)
  fic <- fic[1:2,] %>% dplyr::select(-Espece, -GrEspece) %>% mutate(DHPcm=34, Hautm=20) # une seule iter/placette/annee
  Espece   <- c("SAB", "SAB")
  GrEspece <- c("SAB", "SAB")
  fic$Espece <- Espece
  fic$GrEspece <- GrEspece
  expect_no_error(SortieBillesFusion(Data=fic, Type='DHP', nom_grade1 = 'pate', long_grade1 = 8, diam_grade1 = 9))


})

test_that("La fonction SortieBillesFusion() gère le cas sans essence petro ni sybille", {

  fic <- SimulSaMARE(NbIter = 10, Horizon = 2, Data = Test2500m2)
  fic <- fic[1:2,] %>% dplyr::select(-Espece, -GrEspece) %>% mutate(DHPcm=34, Hautm=20) # une seule iter/placette/annee
  Espece   <- c("PIR", "PIR")
  GrEspece <- c("RES", "RES")
  fic$Espece <- Espece
  fic$GrEspece <- GrEspece
  expect_no_error(SortieBillesFusion(Data=fic, Type='DHP', nom_grade1 = 'pate', long_grade1 = 8, diam_grade1 = 9))


})
