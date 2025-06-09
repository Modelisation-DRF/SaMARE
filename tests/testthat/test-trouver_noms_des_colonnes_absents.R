test_that("La fonction trouver_noms_absents() fonctionne tel qu'attendu quand toutes les variables sont presentes", {

  data_test <- Test2500m2
  obtenu <- trouver_noms_absents(data_test)
  expect_equal(length(obtenu), 0)
})

test_that("La fonction trouver_noms_absents() fonctionne tel qu'attendu quand des variables sont manquantes", {

  data_test <- Test2500m2 %>% select(-Placette, -NoArbre)
  obtenu <- trouver_noms_absents(data_test)
  attendu <- list("placette", "noarbre")
  expect_equal(obtenu, attendu)
})


test_that("La fonction trouver_noms_absents_gaules() fonctionne tel qu'attendu quand toutes les variables sont presentes", {

  data_test <- GaulesTest2500m2
  obtenu <- trouver_noms_absents_gaules(data_test)
  expect_equal(length(obtenu), 0)
})

test_that("La fonction trouver_noms_absents_gaules() fonctionne tel qu'attendu quand des variables sont manquantes", {

  data_test <- Test2500m2 %>% select(-Placette, -Espece)
  obtenu <- trouver_noms_absents_gaules(data_test)
  attendu <- list("placette", "espece")
  expect_equal(obtenu, attendu)
})



test_that("La fonction renommer_les_colonnes() fonctionne tel qu'attendu", {

  data_test <- Test2500m2
  # changer leur ordre
  data_test <- data_test %>% select(NoArbre, Etat, Pente, MSCR, Vigueur, everything())
  # mettre en minuscule
  names(data_test) <- tolower(names(data_test))
  # ajouter une variable non nécessaires
  data_test$test <- 0

  obtenu <- renommer_les_colonnes(data_test)
  obtenu <- names(obtenu)
  attendu <- c("Placette","NoArbre","Espece","Etat","DHPcm","Vigueur","Nombre",
               "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Pente","Ptot","Tmoy",
               "GrwDays","Reg_Eco","Type_Eco", "MSCR","ntrt","ABCD")

  # est-ce que toutes les variables attendu sont présentes
   expect_equal(length( setdiff(attendu, obtenu)), 0)
})

test_that("La fonction renommer_les_colonnes_gaules() fonctionne tel qu'attendu", {

  data_test <- GaulesTest2500m2
  # changer leur ordre
  data_test <- data_test %>% select(Espece, DHPcm, everything())
  # mettre en minuscule
  names(data_test) <- tolower(names(data_test))
  # ajouter une variable non nécessaires
  data_test$test <- 0

  obtenu <- renommer_les_colonnes_gaules(data_test)
  obtenu <- names(obtenu)
  attendu <- c("Placette","Espece","DHPcm","Nombre","Sup_PE")

  # est-ce que toutes les variables attendu sont présentes
  expect_equal(length( setdiff(attendu, obtenu)), 0)
})


test_that("La fonction verifier_variable_meteo() fonctionne tel qu'attendu quand les variables meteo sont présentes", {

  data_test <- Test2500m2
  # si les variables sont présentes, elles sont nommées correctement car dans simulSamare,
  # on appelle nommer_les_colonnes avant d'appeler verifier_variable_meteo
  obtenu <- verifier_variable_meteo(data_test)

  # si les variables etaient présentes, ça retourne le fichier en entrée sans modification
  expect_equal(obtenu, data_test)
})

test_that("La fonction verifier_variable_meteo() fonctionne tel qu'attendu quand les variables meteo sont absentes", {

  data_test <- Test2500m2 %>% select(-Ptot, -Tmoy)
  obtenu <- verifier_variable_meteo(data_test)

  # Ptot et Tmoy ont été estimées et GrwDays est comme dans le fichier en entrée
  obtenu_GrwDays <- obtenu %>% select(-Ptot, -Tmoy)
  expect_equal(obtenu_GrwDays, data_test)
  obtenu_ptot <- obtenu %>% select(Ptot, Tmoy) %>% filter(!is.na(Ptot))
  expect_true(ncol(obtenu_ptot)==2 && nrow(obtenu_ptot)==nrow(data_test))

})


test_that("La fonction verifier_variable_station() fonctionne tel qu'attendu quand les variables de station sont présentes", {

  data_test <- Test2500m2
  # si les variables sont présentes, elles sont nommées correctement car dans simulSamare,
  # on appelle nommer_les_colonnes avant d'appeler verifier_variable_station
  obtenu <- verifier_variable_station(data_test)

  # si les variables etaient présentes, ça retourne le fichier en entrée sans modification
  expect_equal(obtenu, data_test)
})


test_that("La fonction verifier_variable_station() fonctionne tel qu'attendu quand les variables de station sont absentes", {

  data_test <- Test2500m2 %>% select(-Pente)
  obtenu <- verifier_variable_station(data_test)

  # Ptot et Tmoy ont été estimées et GrwDays est comme dans le fichier en entrée
  obtenu_sans <- obtenu %>% select(-Pente)
  expect_equal(obtenu_sans, data_test)
  obtenu_pente <- obtenu %>% select(Pente) %>% filter(!is.na(Pente))
  expect_true(ncol(obtenu_pente)==1 && nrow(obtenu_pente)==nrow(data_test))

})
