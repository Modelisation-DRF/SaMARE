# La fonction SimulSaMARE ne sert qu'à préparer les fichiers d'intrants, de paramètres et d'appeler la fonction SaMARE en boucle,
# pour terminer avec le cubage desd arbres.
# La fonction SaMARE est déjà testée et on sait qu'elle fournit les bons résultats
# La fonction cubage est déjà testée et on sait qu'elle fournit les bons résultats
# Pour la fct SimulSaMARE, puisqu'on sait déjà que les sous-parties donnent les bons résultats,
# il suffit de vérifier que le tout fonctionne sans erreur, pas besoin de vérifier les résultats eux mêmes
# Il faut donc essayer de tester plusieurs situation possibles qui pourraient générer des erreurs
# et tester toutes les combinaisons possibles pour la valeur des paramètres de la fct

# avec gaules  ok
# sans gaules  ok
# avec coupe   ok
# sans coupe   ok
# avec mch     ok
# sans mch     ok
# avec annee inventaire dans le fichier  ok
# sans annee inventaire dans le fichier  ok
# cubage TRUE      ok
# cubage FALSE     ok
# PlacetteID, temps, temps_coupe  ok
# avec martele + CP      ok
# avec martele sans CP   ok
# sans martele           ok

# il y a des fct qui n'ont pas de tests
# renommer_les_colonnes
# valide_Annee_depart


test_that("La fonction SimulSaMARE avec Gaules=1, coupe=1, MCH=0, sans annee inventaire s'exécute sans erreur", {

  set.seed(NULL)
  set.seed(3)

  Test400m2Coupe_test <- Test400m2CP %>% select(-Annee_Inventaire,-NombrE)

  # s'exécute sans erreur
  expect_no_error(SimulSaMARE(NbIter=2, Horizon=6, RecruesGaules=1, Data = Test400m2Coupe_test, Gaules = GaulesTest2500m2, MCH = 0))

  set.seed(NULL)

  result <- SimulSaMARE(NbIter=2, Horizon=6, RecruesGaules=1, Data = Test400m2Coupe_test, Gaules = GaulesTest2500m2, MCH = 0)

  # vérifier le nombre de steps
  nb <- length(unique(result$Annee))
  expect_equal(nb, 7)

  # nombre d'itérations
  nb <- length(unique(result$Iter))
  expect_equal(nb, 2)

  # vérifier la présence des hauteur et volume
  nom <- names(result)
  expect_equal(sum(c('Hautm', 'Vol_dm3') %in% nom),2)

  # vérifier la présence des colonnes de gaules
  nom <- names(result)
  nom_attendu <- sum(c("Nb_Gaules_Ha", "Nb_Gaules_BOJ", "Nb_Gaules_ERS","Nb_Gaules_HEG", "Nb_Gaules_SAB",
                       "Nb_Gaules_68_BOJ", "Nb_Gaules_68_ERS", "Nb_Gaules_68_HEG", "Nb_Gaules_68_SAB") %in% nom)
  expect_equal(nom_attendu, 9)

  # l'année de départ est l'année système
  an_inv <- result %>% group_by(Placette) %>% slice(1)
  expect_equal(an_inv$Annee, as.numeric(format(Sys.Date(), "%Y")))

  # presence de PlacetteID et autres
  expect_equal(sum(c('PlacetteID', 'ntrt','Annee_Coupe', 'Annee_Inventaire', 'MCH', 'reg_eco') %in% nom),6)

  # vérifier Temps
  temps <- unique(result$Temps)
  expect_equal(temps, seq(0,30,5))

  # vérifier Temps depuis coupe
  cp <- unique(result$Temps_depuis_coupe)
  annee_cp <- unique(result$Annee_Coupe)
  annee <- unique(result$Annee)
  expect_equal(cp, annee-annee_cp) # seq(1,31,5) ceci ne marche pas car ça change avec l'année système

  # vérifier mch
  mch0 <- unique(result$MCH)
  expect_equal(mch0, 0)

  # verifier présence de valeurs de dhp
  dhp <- result %>% filter(Etat != 'mort') %>% filter(is.na(DHPcm))
  expect_equal(nrow(dhp), 0)

  # verifier présence de valeurs de haut et vol
  ht <- result %>% filter(Etat != 'mort') %>% filter(is.na(Hautm))
  expect_equal(nrow(ht), 0)
  vol <- result %>% filter(Etat != 'mort') %>% filter(is.na(Vol_dm3))
  expect_equal(nrow(vol), 0)

  # verifier présence de valeurs de gaules
  gau <- result %>% filter(Etat != 'mort') %>% filter(is.na(Nb_Gaules_Ha))
  expect_equal(nrow(gau), 0)

})


test_that("La fonction SimulSaMARE fonctionne MCH=0 ou 1", {
  set.seed(NULL)
  set.seed(5)

  Test400m2Coupe <- Test400m2CP %>% select(-NombrE)

  Result_MCH0 <- SimulSaMARE(NbIter=2, Horizon=3, RecruesGaules=0, Data = Test400m2Coupe, MCH = 0)

  set.seed(NULL)
  set.seed(5)
  Result_MCH1 <- SimulSaMARE(NbIter=2, Horizon=3, RecruesGaules=0, Data = Test400m2Coupe, MCH = 1)

  set.seed(NULL)

  # comparer les 2 colonnes de mort? différence?
  # il n'y a pas de différence entre ces 2 résultats avec seed=3, car on ne voit pas la probabilité de mourir
  # en changeant de seed de 3 à 5, il y a plus de morts de HEG avec MCH1 que MCH0
  # si le paramètre MCH ne fonctionnait pas, les 2 fichiers seraient toujours identiques
  mortmhc0 <- Result_MCH0 %>% filter(Etat=='mort')
  mortmhc1 <- Result_MCH1 %>% filter(Etat=='mort')
  expect_true(nrow(mortmhc1) > nrow(mortmhc0))


  mch1 <- unique(Result_MCH1$MCH)
  expect_equal(mch1, 1)

})


test_that("La fonction SimulSaMARE avec Gaules=0, coupe=0, MCH=0, avec annee inventaire s'exécute sans erreur", {
  set.seed(NULL)
  set.seed(3)

  Test400m2Coupe2 <- Test400m2CP %>% mutate(Annee_Inventaire=2023, Annee_Coupe=NA, ntrt=0) %>% select(-NombrE)

  expect_no_error(SimulSaMARE(NbIter=2, Horizon=6, RecruesGaules=0, Data = Test400m2Coupe2, MCH = 0))

  result <- SimulSaMARE(NbIter=2, Horizon=6, RecruesGaules=0, Data = Test400m2Coupe2, MCH = 0)

  temps <- unique(result$Annee_Inventaire)
  expect_equal(temps, 2023)

  temps <- unique(result$Annee)
  expect_equal(temps, seq(2023, 2053, 5))

  nom <- names(result)
  nom_pas_attendu <- sum(c("Nb_Gaules_Ha", "Nb_Gaules_BOJ", "Nb_Gaules_ERS","Nb_Gaules_HEG", "Nb_Gaules_SAB",
                       "Nb_Gaules_68_BOJ", "Nb_Gaules_68_ERS", "Nb_Gaules_68_HEG", "Nb_Gaules_68_SAB") %in% nom)
  expect_equal(nom_pas_attendu, 0)

  # presence de PlacetteID et autres
  expect_equal(sum(c('PlacetteID', 'ntrt','Annee_Coupe', 'Annee_Inventaire', 'MCH', 'reg_eco') %in% nom),6)

  # verifier présence de valeurs de dhp
  dhp <- result %>% filter(Etat != 'mort') %>% filter(is.na(DHPcm))
  expect_equal(nrow(dhp), 0)

})


# ajouter test avec cubage=FALSE, par défaut il est à TRUE
test_that("La fonction SimulSaMARE avec cubage=FALSE s'exécute sans erreur", {
  set.seed(NULL)
  set.seed(3)

  Test400m2Coupe <- Test400m2CP %>% select(-NombrE)

  # s'exécute sans erreur
  expect_no_error(SimulSaMARE(NbIter=2, Horizon=6, RecruesGaules=0, Data = Test400m2Coupe, MCH = 0, cubage = FALSE))

  set.seed(NULL)

  result <- SimulSaMARE(NbIter=2, Horizon=6, RecruesGaules=0, Data = Test400m2Coupe, MCH = 0, cubage = FALSE)

  # vérifier l'absence des hauteur et volume
  #nom <- names(result)
  #expect_equal(sum(c('hauteur_pred', 'vol_dm3') %in% nom),0)

  # vérifier haut et vol à NA
  verif <- result %>% filter(!is.na(Hautm))
  expect_equal(nrow(verif),0)

  verif <- result %>% filter(!is.na(Vol_dm3))
  expect_equal(nrow(verif),0)

})



test_that("La fonction SimulSaMARE avec coupe et avec martele et avec cubage s'exécute sans erreur", {
  set.seed(NULL)
  set.seed(3)

  Test400m2Coupe2 <- Test400m2CP %>% mutate(Annee_Inventaire=2025, Etat = ifelse(NoArBre<=3, 11, Etat), Annee_Coupe=2021) %>% select(-NombrE)

  # s'exécute sans erreur
  expect_no_error(SimulSaMARE(NbIter=2, Horizon=3, RecruesGaules=0, Data = Test400m2Coupe2, MCH = 0))

  set.seed(NULL)

  result <- SimulSaMARE(NbIter=2, Horizon=3, RecruesGaules=0, Data = Test400m2Coupe2, MCH = 0, cubage=T)


  # vérifier Temps depuis coupe
  cp <- unique(result$Temps_depuis_coupe)
  expect_equal(cp, c(4, 0, 5, 10, 15))  # pour residuel=0, temps=4 car pas encore martele et coupe en 2021 et inv en 2025, mais à residuel=1, le martelage est fait donc temps_coupe=0

  # le fichier est trié correctement
  temps0 <- result %>% filter(Temps==0) %>% select(Iter,Residuel)
  expect_equal(sum(temps0[1:18,2]),0)
  expect_equal(sum(temps0[34:51,2]),0)
  expect_equal(sum(temps0[19:33,2]),33-19+1)
  expect_equal(sum(temps0[52:66,2]),66-52+1)

})

test_that("La fonction SimulSaMARE avec coupe et avec martele et sans cubage s'exécute sans erreur", {
  set.seed(NULL)
  set.seed(3)

  Test400m2Coupe2 <- Test400m2CP %>% mutate(Annee_Inventaire=2025, Etat = ifelse(NoArBre<=3, 11, Etat), Annee_Coupe=2021) %>% select(-NombrE)

  # s'exécute sans erreur
  expect_no_error(SimulSaMARE(NbIter=2, Horizon=3, RecruesGaules=0, Data = Test400m2Coupe2, MCH = 0, cubage=F))

  set.seed(NULL)

  result <- SimulSaMARE(NbIter=2, Horizon=3, RecruesGaules=0, Data = Test400m2Coupe2, MCH = 0, cubage=F)

  # le fichier est trié correctement
  temps0 <- result %>% filter(Temps==0) %>% select(Iter,Residuel)
  expect_equal(sum(temps0[1:18,2]),0)
  expect_equal(sum(temps0[34:51,2]),0)
  expect_equal(sum(temps0[19:33,2]),33-19+1)
  expect_equal(sum(temps0[52:66,2]),66-52+1)



})


test_that("La fonction SimulSaMARE sans coupe et avec martele et avec cubage s'exécute sans erreur", {
  set.seed(NULL)
  set.seed(3)

  Test400m2Coupe2 <- Test400m2CP %>% mutate(Annee_Inventaire=2025, Etat = ifelse(NoArBre<=3, 11, Etat), Annee_Coupe=NA, ntrt=0) %>% select(-NombrE)

  # s'exécute sans erreur
  expect_no_error(SimulSaMARE(NbIter=2, Horizon=3, RecruesGaules=0, Data = Test400m2Coupe2, MCH = 0))

  set.seed(NULL)

  result <- SimulSaMARE(NbIter=2, Horizon=3, RecruesGaules=0, Data = Test400m2Coupe2, MCH = 0)

  # le fichier est trié correctement
  temps0 <- result %>% filter(Temps==0) %>% select(Iter,Residuel)
  expect_equal(sum(temps0[1:18,2]),0)
  expect_equal(sum(temps0[34:51,2]),0)
  expect_equal(sum(temps0[19:33,2]),33-19+1)
  expect_equal(sum(temps0[52:66,2]),66-52+1)


  # vérifier Temps depuis coupe
  cp <- unique(result$Temps_depuis_coupe)
  expect_equal(cp, c(NA, 0, 5, 10, 15))  # pour residuel=0, temps=0 car pas encore martele et pas de coupe, mais à residuel=1, le martelage est fait donc temps_coupe=0


})


test_that("La fonction SimulSaMARE retourne un message d'erreur quand verifArguments retourne une erreur", {

  expect_error(SimulSaMARE(NbIter=1, Horizon=2, RecruesGaules=0, Data=Test2500m2, Gaules, MCH=0, cubage=TRUE),
               "NbIter doit etre > 1")

})


test_that("La fonction SimulSaMARE retourne un message d'erreur avec un nbiter impair", {

  expect_error(SimulSaMARE(NbIter=3, Horizon=1, RecruesGaules=0, Data = Test400m2Temoin),
               "NbIter doit etre un nombre pair")


})

test_that("La fonction SimulSaMARE fonctionne avec un nombre iter pair", {

  result <- SimulSaMARE(NbIter=4, Horizon=1, RecruesGaules=0, Data = Test400m2Temoin)

  # nombre d'itérations
  liste_iter <- unique(result$Iter)
  expect_equal(liste_iter, seq(1:4))

  # identifiant des placettes
  plot_attendu <- unique(Test400m2Temoin$Placette)
  plot_obtenu <- unique(result$Placette)

  expect_equal(plot_obtenu, plot_attendu)
  })


test_that("La fonction SimulSaMARE retourne un message d'erreur avec une variable obligatoire manquante", {

  data_test <- Test400m2Temoin %>% select(-Placette, -NoArbre)
  expect_error(SimulSaMARE(NbIter=2, Horizon=1, RecruesGaules=0, Data = data_test),
               "Les colonnes suivantes ne sont pas dans le fichier en entrée: placette, noarbre")


})


test_that("La fonction SimulSaMARE retourne un message d'erreur avec une variable obligatoire manquante dans les gaules", {

  data_test <- GaulesTest2500m2 %>% select(-Placette, -Espece)
  expect_error(SimulSaMARE(NbIter=2, Horizon=1, RecruesGaules=1, Data = Test2500m2, Gaule=data_test),
               "Les colonnes suivantes ne sont pas dans le fichier des gaules: placette, espece")


})


test_that("La fonction SimulSaMARE fonctionne avec et sans Pente dans le fichier", {

  data_test <- Test400m2Temoin %>% select(-Pente)
  expect_no_error(SimulSaMARE(NbIter=2, Horizon=1, Data = data_test))
  expect_no_error(SimulSaMARE(NbIter=2, Horizon=1, Data = Test400m2Temoin))


})

test_that("La fonction SimulSaMARE fonctionne avec et sans les variables météo dans le fichier", {

  data_test <- Test400m2Temoin %>% select(-Tmoy)
  expect_no_error(SimulSaMARE(NbIter=2, Horizon=1, Data = data_test))
  expect_no_error(SimulSaMARE(NbIter=2, Horizon=1, Data = Test400m2Temoin))

})




test_that("La fonction SimulSaMARE avec simulateur de coupe fonctionne pour coupe placette temoin sans modificateur, avec cubage, hor=1, sans martelage", {

  data1 <- Test400m2Temoin %>% filter(Placette=='TEM23APC5000') # TEM sans arbres martelés

  result <- SimulSaMARE(NbIter=2, Horizon=1, Data = data1, Coupe_ON = c(1), cubage=TRUE)
  # on s'attend à ce que la coupe se fasse à la premiere periode car il n'y a pas de martelage

  # pour temps=0 et residuel=0: ntrt=0, annee_coupe=NA, temps_depuis_coupe=NA
  obtenu <- as.matrix(result %>% filter(Temps==0, Residuel==0) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(NA, NA, 0), nrow = 1))

  # pour temps=0 et residuel=1: ntrt=1, annee_coupe=2024, temps_depuis_coupe=0
  obtenu <- as.matrix(result %>% filter(Temps==0, Residuel==1) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2024, 0, 1), nrow = 1))

  # pour temps=5: residuel=0, ntrt=1,  annee_coupe=2024, temps_depuis_coupe=5
  obtenu <- as.matrix(result %>% filter(Temps==5) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2024, 5, 1, 0), nrow = 1))



})

test_that("La fonction SimulSaMARE avec simulateur de coupe fonctionne pour coupe placette temoin sans modificateur, avec cubage, hor=1, avec martelage", {

  data2 <- Test400m2Temoin %>% filter(Placette=='TEM23APC5002') # TEM avec arbres martelés

  result <- SimulSaMARE(NbIter=2, Horizon=1, Data = data2, Coupe_ON = c(1))

  # on s'attend à ce que la coupe ne se fasse pas à la premiere periode car il y a des arbres martelés

  # pour temps=0 et residuel=0: ntrt=0, annee_coupe=NA, temps_depuis_coupe=NA
  obtenu <- as.matrix(result %>% filter(Temps==0, Residuel==0) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(NA, NA, 0), nrow = 1))

  # pour temps=0 et residuel=1: ntrt=1, annee_coupe=2024, temps_depuis_coupe=0
  obtenu <- as.matrix(result %>% filter(Temps==0, Residuel==1) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2024, 0, 1), nrow = 1))

  # pour temps=5: residuel=0, ntrt=1,  annee_coupe=2024, temps_depuis_coupe=5
  obtenu <- as.matrix(result %>% filter(Temps==5) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2024, 5, 1, 0), nrow = 1))


})




test_that("La fonction SimulSaMARE avec simulateur de coupe fonctionne pour 2 coupe placette CP avec modificateur, sans cubage, hor=3, sans martelage", {

  data <- Test400m2CP %>% select(-NombrE) # placette CP ntrt=1, annee_coupe=2024, annee_inventaire=2024

  result <- SimulSaMARE(NbIter=2, Horizon=6, Data = data, Coupe_ON = c(NA,NA,1,NA, NA,2), Coupe_modif = list(NA, NA, NA, NA, NA, 50))

  # on s'attend a une coupe supplémentaire à la période 3 et 6:
  # periode 1: 2024 à 2029, 0-5 ans, pas de coupe
  # periode 2: 2029 à 2034, 5-10 ans, pas de coupe
  # periode 3: 2034 à 2039, 10-15, coupe en début de période, donc à 10 ans un residuel et à 15 ans un trt de plus
  # periode 4: 2039 à 2044, 15-20, pas de coupe
  # periode 5: 2044 à 2049, 20-25, pas de coupe
  # periode 6: 2049 à 2054, 25-30, coupe en début de période, donc à 25 ans un residuel et à 30 ans un trt de plus

  # pour temps=0 : residuel=0: ntrt=1, annee_coupe=2024, temps_depuis_coupe=0
  obtenu <- as.matrix(result %>% filter(Temps==0) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2024, 0, 1, 0), nrow = 1))

  # pour temps=5 : residuel=0: ntrt=1, annee_coupe=2024, temps_depuis_coupe=5
  obtenu <- as.matrix(result %>% filter(Temps==5) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2024, 5, 1, 0), nrow = 1))

  # pour temps=10 : residuel=0: ntrt=1, annee_coupe=2024, temps_depuis_coupe=10
  obtenu <- as.matrix(result %>% filter(Temps==10, Residuel==0) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2024, 10, 1), nrow = 1))

  # pour temps=10 : residuel=1: ntrt=2, annee_coupe=2024, temps_depuis_coupe=0
  obtenu <- as.matrix(result %>% filter(Temps==10, Residuel==1) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2034, 0, 2), nrow = 1))

  # pour temps=15 : residuel=0: ntrt=2, annee_coupe=2034, temps_depuis_coupe=5
  obtenu <- as.matrix(result %>% filter(Temps==15) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2034, 5, 2, 0), nrow = 1))

  # pour temps=20 : residuel=0: ntrt=2, annee_coupe=2034, temps_depuis_coupe=10
  obtenu <- as.matrix(result %>% filter(Temps==20) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2034, 10, 2, 0), nrow = 1))

  # pour temps=25 : residuel=0: ntrt=2, annee_coupe=2034, temps_depuis_coupe=15
  obtenu <- as.matrix(result %>% filter(Temps==25, Residuel==0) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2034, 15, 2), nrow = 1))

  # pour temps=25 : residuel=1: ntrt=3, annee_coupe=2049, temps_depuis_coupe=0
  obtenu <- as.matrix(result %>% filter(Temps==25, Residuel==1) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2049, 0, 3), nrow = 1))

  # pour temps=30 : residuel=0: ntrt=3, annee_coupe=2049, temps_depuis_coupe=5
  obtenu <- as.matrix(result %>% filter(Temps==30) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2049, 5, 3, 0), nrow = 1))
})



test_that("La fonction SimulSaMARE avec simulateur de coupe fonctionne avec une table pour modificateur de coupe et plus d'une placette", {

  data <- Test400m2Temoin %>% mutate(Etat=10) # ne pas mettre de martelé

  modif <- data.frame(ess_ind=c('ERS','HEG'),
                      modifier=c(50,80))

  result <- SimulSaMARE(NbIter=2, Horizon=6, Data = data, Coupe_ON = c(NA,NA,1,NA,NA,2), Coupe_modif = list(NA, NA, modif, NA, NA, 50))


  # pour temps=0 : residuel=0: ntrt=1, annee_coupe=NA, temps_depuis_coupe=NA
  obtenu <- as.matrix(result %>% filter(Temps==0) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(NA, NA, 0, 0), nrow = 1))

  # pour temps=5 : residuel=0: ntrt=1, annee_coupe=NA, temps_depuis_coupe=NA
  obtenu <- as.matrix(result %>% filter(Temps==5) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(NA, NA, 0, 0), nrow = 1))

  # pour temps=10 : residuel=0: ntrt=1, annee_coupe=NA, temps_depuis_coupe=NA
  obtenu <- as.matrix(result %>% filter(Temps==10, Residuel==0) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(NA, NA, 0), nrow = 1))

  # pour temps=10 : residuel=1: ntrt=2, annee_coupe=2034, temps_depuis_coupe=0
  obtenu <- as.matrix(result %>% filter(Temps==10, Residuel==1) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2034, 0, 1), nrow = 1))

  # pour temps=15 : residuel=0: ntrt=1, annee_coupe=2034, temps_depuis_coupe=5
  obtenu <- as.matrix(result %>% filter(Temps==15) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2034, 5, 1, 0), nrow = 1))

  # pour temps=20 : residuel=0: ntrt=1, annee_coupe=2034, temps_depuis_coupe=10
  obtenu <- as.matrix(result %>% filter(Temps==20) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2034, 10, 1, 0), nrow = 1))

  # pour temps=25 : residuel=0: ntrt=1, annee_coupe=2034, temps_depuis_coupe=15
  obtenu <- as.matrix(result %>% filter(Temps==25, Residuel==0) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2034, 15, 1), nrow = 1))

  # pour temps=25 : residuel=1: ntrt=2, annee_coupe=2049, temps_depuis_coupe=0
  obtenu <- as.matrix(result %>% filter(Temps==25, Residuel==1) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2049, 0, 2), nrow = 1))

  # pour temps=30 : residuel=0: ntrt=2, annee_coupe=2049, temps_depuis_coupe=5
  obtenu <- as.matrix(result %>% filter(Temps==30) %>% select(Annee_Coupe, Temps_depuis_coupe, ntrt, Residuel) %>% unique())
  expect_equal(unname(obtenu), matrix(c(2049, 5, 2, 0), nrow = 1))


})




# test_that("La fonction SimulSaMARE fonctionne avec un nombre iter >10 et multi_session=FALSE", {
#
#   result <- SimulSaMARE(NbIter=20, Horizon=1, RecruesGaules=0, Data = Test400m2)
#
#   # nombre d'itérations
#   liste_iter <- unique(result$Iter)
#   expect_equal(liste_iter, seq(1:20))
#
#   # identifiant des placettes
#   plot_attendu <- unique(Test400m2$Placette)
#   plot_obtenu <- unique(result$Placette)
#   expect_equal(plot_obtenu, plot_attendu)
# })

# test_that("La fonction SimulSaMARE fonctionne avec un nombre iter >10 et multi_session=TRUE", {
#
#   result <- SimulSaMARE(NbIter=20, Horizon=1, RecruesGaules=0, Data = Test400m2, multi_session=TRUE)
#
#   # nombre d'itérations
#   liste_iter <- unique(result$Iter)
#   expect_equal(liste_iter, seq(1:20))
#
#   # identifiant des placettes
#   plot_attendu <- unique(Test400m2$Placette)
#   plot_obtenu <- unique(result$Placette)
#   expect_equal(plot_obtenu, plot_attendu)
# })

# test_that("La fonction SimulSaMARE fonctionne avec un nombre iter >10 et multi_session=FALSE", {
#
#   result <- SimulSaMARE(NbIter=20, Horizon=1, RecruesGaules=0, Data = Test400m2, multi_session=FALSE)
#
#   # nombre d'itérations
#   liste_iter <- unique(result$Iter)
#   expect_equal(liste_iter, seq(1:20))
#
#   # identifiant des placettes
#   plot_attendu <- unique(Test400m2$Placette)
#   plot_obtenu <- unique(result$Placette)
#   expect_equal(plot_obtenu, plot_attendu)
# })
