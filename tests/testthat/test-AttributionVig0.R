test_that("La fonction AttribVigu0() fonctionne correctement sans vigueur", {

  set.seed(NULL)
  set.seed(3)

  Plac_test <- readRDS(test_path("fixtures/Vigu0_Prod0", "Fic_test_complet.rds"))
  param <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRVig.rds"))
  attendu <- readRDS(test_path("fixtures/Vigu0_Prod0", "resultat_attendu_vig.rds")) #%>% dplyr::select(-vigu0)

  obtenu_vig <- AttribVigu0(Plac_test, param) #%>% dplyr::select(-vigu0)

  expect_equal(obtenu_vig, attendu)

})

test_that("La fonction AttribVigu0() fonctionne correctement avec Vigueur fourni", {

  Plac_test <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds"))
  # ajouter des resineux avec vigueeur 5 et 6
  res1 <- Plac_test %>% filter(GrEspece=='HEG') %>% mutate(GrEspece='SAB', Vigueur=5)
  res2 <- Plac_test %>% filter(GrEspece=='ERS') %>% mutate(GrEspece='EPX', Vigueur=6)
  Plac_test <- bind_rows(Plac_test, res1, res2)

  Para.Vig <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRVig.rds"))

  obtenu <- AttribVigu0(Plac_test, Para.Vig)
  obtenu1 <- obtenu %>% mutate(verif = ifelse(Vigueur %in% c(1,2,5) & vigu0=='ViG', 0,
                                              ifelse(Vigueur %in% c(3,4,6) & vigu0=='NONVIG', 0,
                                                     1))) %>%
    select(GrEspece, DHPcm, Vigueur, vigu0, verif) %>%
    summarise(nb=sum(verif))


  expect_equal(obtenu1$nb, 0)


})


test_that("La fonction AttribVigu0() fonctionne correctement sans vigueur sans mscr", {

  Para.Vig <- readRDS(test_path("fixtures/Vigu0_Prod0", "Para.ConvMSCRVig.rds"))
  Plac_test <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
    mutate(Vigueur=NA, MSCR=NA)

  obtenu <- AttribVigu0(Plac_test, Para.Vig)

  expect_equal(obtenu$vigu0, rep('ViG',18))
  expect_equal(ncol(obtenu), ncol(Plac_test)+3)


})

# fic <- Test2500m2 %>% mutate(DHPcm=20, Vigueur=NA)
# fic <- fic[1:1,]
# data = NULL
# for (mscr in c('M','S','C','R')) {
#   for (esp in c('BOJ','ERR','ERS','HEG','BOP','CHR','SAB')) {
#     fic$MSCR = mscr
#     fic$Espece = esp
#     data = bind_rows(data,fic)
#   }
# }
# set.seed(NULL)
# set.seed(1)
# result <- SimulSaMARE(NbIter = 10000, Horizon = 2, Data = data)
# result0 = result %>% filter(Annee==2026)
# prop <- result0 %>%
#   group_by(GrEspece, MSCR, Vigueur) %>%
#   summarise(nb = n()) %>%
#   group_by(GrEspece, MSCR) %>%
#   mutate(ntot = sum(nb),
#          prop = nb/ntot*100) %>%
#   select(-ntot, -nb)
#
# prop2 <- prop %>% group_by(GrEspece, MSCR) %>% pivot_wider(names_from = Vigueur, values_from = prop)  %>%
#   mutate(code = ifelse(MSCR=='M','1-M',
#                        ifelse(MSCR=='S','2-S',
#                               ifelse(MSCR=='C','3-C',
#                                      '4-R')))) %>%
#   arrange(GrEspece, code) %>%
#   select(-code)
# readr::write_delim(prop2,"ConversionMSCR30cm.csv", delim=';')

# la classe 3 est pour les >23cm
# la classe 4 est pour les <23cm
