test_that("La fonction SortieBillonage() gère les essences comme attendu", {

  fic <- readRDS(test_path("fixtures", "expect_test_for_Simulateur_Samare.rds"))
  fic <- fic[1:16,] %>% dplyr::select(-Espece, -GrEspece) %>% mutate(DHPcm=24)
  Espece   <- c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR", "CHG", "CHB", "CHE", "SAB",    NA,    NA,    NA,    NA,    NA,  NA)
  GrEspece <- c("ERS", "BOJ", "ERR", "FIN", "HEG", "FEN", "FEN", "FEN", "FEN", "SAB", "ERS", "BOJ", "ERR", "HEG", "FEN", "SAB")

  fic$Espece <- Espece
  fic$GrEspece <- GrEspece

  # il devrait y avoir des estimations pour 13 lignes

  # essences samare:      "AUT" "BOJ" "EPX" "ERR" "ERS" "FEN" "FIN" "HEG" "RES" "SAB"
  # essences billonnage:         BOJ         ERR   ERS               HEG               CHR  BOP

  result <- SortieBillonage(Data=fic, Type="DHP2015")

  expect_equal(nrow(result),13)

})

test_that("La fonction SortieBillonage() message erreur essence", {

  fic <- readRDS(test_path("fixtures", "expect_test_for_Simulateur_Samare.rds"))
  fic <- fic[1:3,] %>% dplyr::select(-Espece, -GrEspece, -DHPcm)
  Espece   <- c("ERS", "BOJ", "SAB")
  GrEspece <- c("ERS", "BOJ", "SAB")
  DHPcm <- c(12,12,25)

  fic$Espece <- Espece
  fic$GrEspece <- GrEspece
  fic$DHPcm <- DHPcm

  result <- SortieBillonage(Data=fic, Type="DHP2015")

  expect_equal(result$erreur,"Code d'essence a l'exterieur de la plage de valeurs possibles pour billonage")

})
