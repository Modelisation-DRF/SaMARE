test_that("La fonction SortieSybille() fonctionne comme attendu", {

  fic <- SimulSaMARE(NbIter = 10, Horizon = 2, Data = Test2500m2)
  fic <- fic[1:16,] %>% dplyr::select(-Espece, -GrEspece) %>% mutate(DHPcm=34, Hautm=20) # une seule iter/placette/annee
  Espece   <- c("EPN", "BOJ", "ERR", "BOP", "HEG", "CHR", "CHG", "CHB", "CHE", "SAB",    NA,    NA,    NA,    NA,    NA,  NA)
  GrEspece <- c("RES", "BOJ", "ERR", "FIN", "HEG", "FEN", "FEN", "FEN", "FEN", "SAB", "ERS", "BOJ", "ERR", "HEG", "FEN", "SAB")

  fic$Espece <- Espece
  fic$GrEspece <- GrEspece

  # sybille s'applliquera sur EPN, SAB, BOP, arbres 1-4-10
  result <- SortieSybille(Data=fic, nom_grade1 = 'pate', long_grade1 = 8, diam_grade1 = 9)
  result2 <- result %>% filter(!is.na(vol_bille_dm3)) %>% group_by(ArbreID) %>% slice(1)

  expect_equal(nrow(result2),3) # valeur pour 13 arbres

})

test_that("La fonction SortieSybille() gère le cas sans aucun arbre valide", {

  fic <- SimulSaMARE(NbIter = 10, Horizon = 2, Data = Test2500m2)
  fic <- fic[1:3,] %>% dplyr::select(-Espece, -GrEspece, -DHPcm)
  Espece   <- c("ERS", "BOJ", "HEG")
  GrEspece <- c("ERS", "BOJ", "HEG")
  DHPcm <- c(12,12,25)

  fic$Espece <- Espece
  fic$GrEspece <- GrEspece
  fic$DHPcm <- DHPcm

  result <- SortieSybille(Data=fic, nom_grade1 = 'pate', long_grade1 = 8, diam_grade1 = 9)

  expect_equal(nrow(result),nrow(fic))
  result2 <- result %>% filter(is.na(vol_bille_dm3))
  expect_equal(nrow(result2),nrow(fic))


})


test_that("La fonction SortieSybille() fonctionne sans erreur à partir d'une simulation de samare avec 1 grade", {
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)
  simul <- simul[1:16,] %>% dplyr::select(-Espece, -GrEspece) %>% mutate(DHPcm=34, Hautm=20) # une seule iter/placette/annee
  Espece   <- c("EPN", "BOJ", "ERR", "BOP", "HEG", "CHR", "CHG", "CHB", "CHE", "SAB",    NA,    NA,    NA,    NA,    NA,  NA)
  GrEspece <- c("RES", "BOJ", "ERR", "FIN", "HEG", "FEN", "FEN", "FEN", "FEN", "SAB", "ERS", "BOJ", "ERR", "HEG", "FEN", "SAB")

  simul$Espece <- Espece
  simul$GrEspece <- GrEspece
  expect_no_error(SortieSybille(Data=simul, nom_grade1 = 'pate', long_grade1 = 8, diam_grade1 = 9))
})

test_that("La fonction SortieSybille() fonctionne sans erreur à partir d'une simulation de samare avec 2 grades", {
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)
  simul <- simul[1:16,] %>% dplyr::select(-Espece, -GrEspece) %>% mutate(DHPcm=34, Hautm=20) # une seule iter/placette/annee
  Espece   <- c("EPN", "BOJ", "ERR", "BOP", "HEG", "CHR", "CHG", "CHB", "CHE", "SAB",    NA,    NA,    NA,    NA,    NA,  NA)
  GrEspece <- c("RES", "BOJ", "ERR", "FIN", "HEG", "FEN", "FEN", "FEN", "FEN", "SAB", "ERS", "BOJ", "ERR", "HEG", "FEN", "SAB")

  simul$Espece <- Espece
  simul$GrEspece <- GrEspece
  expect_no_error(SortieSybille(Data=simul, nom_grade1 = 'sciage', long_grade1 = 8, diam_grade1 = 20,
                                            nom_grade2 = 'pate', long_grade2 = 8, diam_grade2 = 9))
})

test_that("La fonction SortieSybille() fonctionne sans erreur à partir d'une simulation de samare avec 3 grades", {
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)
  simul <- simul[1:16,] %>% dplyr::select(-Espece, -GrEspece) %>% mutate(DHPcm=34, Hautm=20) # une seule iter/placette/annee
  Espece   <- c("EPN", "BOJ", "ERR", "BOP", "HEG", "CHR", "CHG", "CHB", "CHE", "SAB",    NA,    NA,    NA,    NA,    NA,  NA)
  GrEspece <- c("RES", "BOJ", "ERR", "FIN", "HEG", "FEN", "FEN", "FEN", "FEN", "SAB", "ERS", "BOJ", "ERR", "HEG", "FEN", "SAB")

  simul$Espece <- Espece
  simul$GrEspece <- GrEspece
  expect_no_error(SortieSybille(Data=simul, nom_grade1 = 'sciage', long_grade1 = 8, diam_grade1 = 20,
                                nom_grade2 = 'sciage2', long_grade2 = 8, diam_grade2 = 10,
                                nom_grade3 = 'pate', long_grade3 = 8, diam_grade3 = 9))
})
