test_that("La fonction SortieBillonage() gère les essences comme attendu", {

  fic <- readRDS(test_path("fixtures", "expect_test_for_Simulateur_Samare.rds"))
  fic <- fic[1:16,] %>% dplyr::select(-Espece, -GrEspece) %>% mutate(DHPcm=24) # une seule iter/placette/annee
  Espece   <- c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR", "CHG", "CHB", "CHE", "SAB",    NA,    NA,    NA,    NA,    NA,  NA)
  GrEspece <- c("ERS", "BOJ", "ERR", "FIN", "HEG", "FEN", "FEN", "FEN", "FEN", "SAB", "ERS", "BOJ", "ERR", "HEG", "FEN", "SAB")

  fic$Espece <- Espece
  fic$GrEspece <- GrEspece

  fic <- fic %>% rename(Vig=vigu0, Prod=prod0)

  # il devrait y avoir des estimations pour 13 lignes

  # essences samare:      "AUT" "BOJ" "EPX" "ERR" "ERS" "FEN" "FIN" "HEG" "RES" "SAB"
  # essences billonnage:         BOJ         ERR   ERS               HEG               CHR  BOP

  result <- SortieBillonnage(Data=fic, Type="DHP2015")
  result2 <- result %>% filter(!is.na(vol_bille_dm3)) %>% group_by(NoArbre) %>% slice(1)

  expect_equal(nrow(result2),13) # valeur pour 13 arbres

})

test_that("La fonction SortieBillonage() gère le cas sans aucun arbre valide", {

  # filtrer sur dhp>23 et essence in c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR")

  fic <- readRDS(test_path("fixtures", "expect_test_for_Simulateur_Samare.rds"))
  fic <- fic[1:3,] %>% dplyr::select(-Espece, -GrEspece, -DHPcm)
  Espece   <- c("ERS", "BOJ", "SAB")
  GrEspece <- c("ERS", "BOJ", "SAB")
  DHPcm <- c(12,12,25)

  fic <- fic %>% rename(Vig=vigu0, Prod=prod0, origTreeID=NoArbre)
  fic$Espece <- Espece
  fic$GrEspece <- GrEspece
  fic$DHPcm <- DHPcm

  result <- SortieBillonnage(Data=fic, Type="DHP2015")

  #expect_equal(result$message,"Aucun arbre valide pour le billonnage")
  expect_equal(nrow(result),0)
  expect_equal(names(result),c("Iter","Placette", "Annee", "ArbreID", "grade_bille", "vol_bille_dm3"))


})


test_that("La fonction SortieBillonage() fonctionne sans erreur à partir d'une simulation de samare avec type=DHP2015", {
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)
  expect_no_error(SortieBillonnage(Data=simul, Type="DHP2015"))
  #obtenu <- SortieBillonnage(Data=simul, Type="DHP2015")
})


test_that("La fonction SortieBillonage() fonctionne sans erreur avec type=1234", {
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)
  expect_no_error(SortieBillonnage(Data=simul, Type="1234"))
  #obtenu <- SortieBillonnage(Data=simul, Type="1234")
})


test_that("La fonction SortieBillonage() fonctionne sans erreur avec type=DHP", {
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)
  expect_no_error(SortieBillonnage(Data=simul, Type="DHP"))
  #obtenu <- SortieBillonnage(Data=simul, Type="DHP")
})

test_that("La fonction SortieBillonage() fonctionne sans erreur avec type=ABCD", {
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)
  expect_no_error(SortieBillonnage(Data=simul, Type="ABCD"))
  #obtenu <- SortieBillonnage(Data=simul, Type="ABCD")
})

test_that("La fonction SortieBillonage() fonctionne sans erreur avec type=MSCR", {
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)
  expect_no_error(SortieBillonnage(Data=simul, Type="MSCR"))
  #obtenu <- SortieBillonnage(Data=simul, Type="MSCR")
})

test_that("La fonction SortieBillonage() fonctionne sans erreur avec type=ABCD2015", {
  simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2)
  expect_no_error(SortieBillonnage(Data=simul, Type="ABCD2015"))
  #obtenu <- SortieBillonnage(Data=simul, Type="ABCD2015")
})

