# fct 1: CheckArguments(): vérifications des paramètres.

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Data", {
  chk = verifArguments(NbIter=2, Horizon=2, RecruesGaules=0, Data=, Gaules=, MCH=0, cubage=FALSE, Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(chk[[1]], "Data doit etre specifie")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour RecruesGaules", {
  chk = verifArguments(NbIter=2, Horizon=2, RecruesGaules=3, Data=Test, Gaules=, MCH=0, cubage=FALSE, Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(chk[[1]], "RecruesGaules doit etre 0 ou 1")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour MCH", {
  chk = verifArguments(NbIter=2, Horizon=2, RecruesGaules=0, Data=Test, Gaules=, MCH=3, cubage=FALSE, Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(chk[[1]], "MCH doit etre 0 ou 1")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour cubage", {
  chk = verifArguments(NbIter=2, Horizon=2, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage='oui', Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(chk[[1]], "cubage doit etre TRUE ou FALSE")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Horizon", {
  chk = verifArguments(NbIter=2, Horizon=0, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(chk[[1]], "Horizon doit etre de 1 a 20")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Horizon", {
  chk = verifArguments(NbIter=2, Horizon=23, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(chk[[1]], "Horizon doit etre de 1 a 20")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu pour NbIter", {
  chk = verifArguments(NbIter=1, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(chk[[1]], "NbIter doit etre > 1")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu pour NbIter", {
  chk = verifArguments(NbIter=3, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(chk[[1]], "NbIter doit etre un nombre pair")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Gaules", {
  chk = verifArguments(NbIter=2, Horizon=1, RecruesGaules=1, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(chk[[1]], "Avec RecruesGaules==1 Gaules doit etre specifie")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu sans erreur", {
  chk = verifArguments(NbIter=2, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(length(chk), 0)
})
# test_that("La fonction verifArguments() fonctionne tel qu'attendu pour multi_session", {
#   chk = verifArguments(NbIter=2, Horizon=2, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=TRUE, multi_session='oui')
#   expect_equal(chk, "multi_session doit etre TRUE ou FALSE")
# })


test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Coupe_ON", {
  chk = verifArguments(NbIter=2, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = 'off', Coupe_modif = NULL)
  expect_equal(chk[[1]], "L'argument Coupe_ON doit etre un vecteur numerique")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Coupe_ON avec Horizon", {
  chk = verifArguments(NbIter=2, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = c(0,1), Coupe_modif = NULL)
  expect_equal(chk[[1]], "La longueur de Coupe_ON doit etre egale a Horizon (1)")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour valeur de Coupe_ON", {
  chk = verifArguments(NbIter=2, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = c(20), Coupe_modif = NULL)
  expect_equal(chk[[1]], "Les valeurs de Coupe_ON doivent etre entre 0 et 19 ou NA")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Coupe_modif", {
  chk = verifArguments(NbIter=2, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = c(1), Coupe_modif = c(1))
  expect_equal(chk[[1]], "L'argument Coupe_modif doit etre une liste")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Coupe_modif et Horizon", {
  chk = verifArguments(NbIter=2, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = c(1), Coupe_modif = list(100,NA))
  expect_equal(chk[[1]], "La longueur de Coupe_modif doit etre egale a Horizon (1)")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour valeur Coupe_modif", {
  chk = verifArguments(NbIter=2, Horizon=3, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = c(2, NA, 3), Coupe_modif = list(-90, NA, 170))
  expect_equal(chk[[1]], "Le modificateur a la position 3 doit etre entre -80 et 160")
})


test_that("La fonction verifArguments() fonctionne tel qu'attendu pour valeur Coupe_modif data frame", {
  modif <- data.frame(essind=c('ERS','HEG'),
                      modifier=c(50,80))
  chk = verifArguments(NbIter=2, Horizon=3, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = c(2, NA, 3), Coupe_modif = list(NA, NA, modif))
  expect_equal(chk[[1]], "Le data.frame a la position 3 doit contenir les colonnes 'ess_ind' et 'modifier'")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour valeur Coupe_modif data frame nombre", {
  modif <- data.frame(ess_ind=c('ERS','HEG'),
                      modifier=c(-90,80))
  chk = verifArguments(NbIter=2, Horizon=3, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F, Coupe_ON = c(2, NA, 3), Coupe_modif = list(NA, NA, modif))
  expect_equal(chk[[1]], "Tous les modificateurs dans le data.frame a la position 3 doivent etre entre -80 et 160")
})



test_that("La fonction verifArguments() fonctionne tel qu'attendu pour 2 erreurs", {
  # cubage et Data en erreur
  chk = verifArguments(NbIter=2, Horizon=1, RecruesGaules=0, Data=, Gaules=, MCH=0, cubage='F', Coupe_ON = NULL, Coupe_modif = NULL)
  expect_equal(length(chk),2)
})
