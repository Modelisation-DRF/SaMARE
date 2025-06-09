# valide_Annee_depart: Dans le fichier Validation_colonne.R

test_that("La fonction valide_Annee_depart() fonctionne tel qu'attendu quand la colonne n'est pas dans le fichier", {

  data_test <- Test2500m2 # ne contient pas Annee_Inventaire
  obtenu <- valide_Annee_depart(data_test)
  obtenu <- unique((obtenu$Annee_Inventaire))
  expect_equal(as.character(obtenu), format(Sys.Date(), "%Y"))
})

test_that("La fonction valide_Annee_depart() fonctionne tel qu'attendu quand la colonne est dans le fichier", {

  data_test <- Test2500m2 %>% mutate(Annee_Inventaire=2015)
  obtenu <- valide_Annee_depart(data_test)
  expect_equal(obtenu, data_test)
})


test_that("La fonction valide_Annee_depart() fonctionne tel qu'attendu quand la colonne contient des incohérences", {

  data_test <- Test400m2 %>% mutate(Annee_Inventaire = ifelse(Placette=='TEM23APC5000', NA,
                                                              ifelse(Placette=='TEM23APC5001' & Espece=='BOJ', 2015,
                                                                     Annee_Inventaire)))
  obtenu <- valide_Annee_depart(data_test)
  obtenu1 <- obtenu %>% filter(Placette %in% c('TEM23APC5000','TEM23APC5001'))
  obtenu1 <- unique(obtenu1$Annee_Inventaire)
  expect_equal(as.character(obtenu1), format(Sys.Date(), "%Y"))

  obtenu2 <- obtenu %>% filter(!Placette %in% c('TEM23APC5000','TEM23APC5001'))
  obtenu2 <- unique(obtenu2$Annee_Inventaire)
  expect_equal(obtenu2, 2024)
})
