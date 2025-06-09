# fct 1: CheckArguments(): vérifications des paramètres.

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Data", {
  chk = verifArguments(NbIter=2, Horizon=2, RecruesGaules=0, Data=, Gaules=, MCH=0, cubage=FALSE)
  expect_equal(chk, "Data doit etre specifie")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour RecruesGaules", {
  chk = verifArguments(NbIter=2, Horizon=2, RecruesGaules=3, Data=Test, Gaules=, MCH=0, cubage=FALSE)
  expect_equal(chk, "RecruesGaules doit etre 0 ou 1")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour MCH", {
  chk = verifArguments(NbIter=2, Horizon=2, RecruesGaules=0, Data=Test, Gaules=, MCH=3, cubage=FALSE)
  expect_equal(chk, "MCH doit etre 0 ou 1")
})

test_that("La fonction verifArguments() fonctionne tel qu'attendu pour cubage", {
  chk = verifArguments(NbIter=2, Horizon=2, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage='oui')
  expect_equal(chk, "cubage doit etre TRUE ou FALSE")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Horizon", {
  chk = verifArguments(NbIter=2, Horizon=0, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F)
  expect_equal(chk, "Horizon doit etre de 1 a 12")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Horizon", {
  chk = verifArguments(NbIter=2, Horizon=13, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F)
  expect_equal(chk, "Horizon doit etre de 1 a 12")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu pour NbIter", {
  chk = verifArguments(NbIter=1, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F)
  expect_equal(chk, "NbIter doit etre > 1")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu pour NbIter", {
  chk = verifArguments(NbIter=3, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F)
  expect_equal(chk, "NbIter doit etre un nombre pair")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu pour Gaules", {
  chk = verifArguments(NbIter=2, Horizon=1, RecruesGaules=1, Data=Test, Gaules=, MCH=0, cubage=F)
  expect_equal(chk, "Avec RecruesGaules==1 Gaules doit etre specifie")
})
test_that("La fonction verifArguments() fonctionne tel qu'attendu sans erreur", {
  chk = verifArguments(NbIter=2, Horizon=1, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=F)
  expect_equal(chk, "ok")
})
# test_that("La fonction verifArguments() fonctionne tel qu'attendu pour multi_session", {
#   chk = verifArguments(NbIter=2, Horizon=2, RecruesGaules=0, Data=Test, Gaules=, MCH=0, cubage=TRUE, multi_session='oui')
#   expect_equal(chk, "multi_session doit etre TRUE ou FALSE")
# })

