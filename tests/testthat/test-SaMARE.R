# test_that("SaMARE returns the expected data frame without Gaules and MCH = 0", {
#   set.seed(NULL)
#   set.seed(3)
#   ListeItertest <- data.frame(PlacetteID ="TEM23APC5000_1",
#                               Placette = "TEM23APC5000",
#                               Iter = 1)
#
#   Data_test_for_simul_samare <- readRDS(test_path("fixtures", "Data_test_for_simul_samare.rds"))
#   RandomTest <- readRDS(test_path("fixtures", "RandomTest.rds"))
#   result_simul2<-SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA, ListeIter=ListeItertest, AnneeDep=2023,
#                         Horizon = 6 ,
#                         RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
#                         Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)
#
#   result_samare_sans_gaules_test <- readRDS(test_path("fixtures", "result_samare_sans_gaules_test.rds"))
#   set.seed(NULL)
#   expect_equal(result_simul2, result_samare_sans_gaules_test)
# })

# ok
test_that("SaMARE returns the expected data frame without Gaules and MCH = 0, avec coupe, sans qualite, sans mscr", {
  set.seed(NULL)
  set.seed(3)
  # ListeItertest <- data.frame(PlacetteID ="TEM23APC5000_1",
  #                         Placette = "TEM23APC5000",
  #                         Iter = 1)

  # ntrt=1, annee_coupe=2021, sans MSCR, sans ABCD, sans MSCR
  Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>% mutate(Annee_Inventaire=2023)

  RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
  obtenu<-SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                    Horizon = 6 ,
                    Iteration = 1,
                    #seed_value=NULL,
                    RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                    Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)

  #attendu <- readRDS(test_path("fixtures/samare", "result_samare_sans_gaules_test.rds"))
  #attendu <- readRDS(test_path("fixtures/samare", "result_samare_sans_gaules_test_new.rds"))
  attendu <- readRDS(test_path("fixtures/samare", "result_samare_sans_gaules_test_new2.rds"))
  set.seed(NULL)
  obtenu2 <- obtenu %>% select(-t0, -trt, -ntrt, -t0_aj_, -mch, -Annee_Inventaire)
  expect_equal(as.data.frame(obtenu2), as.data.frame(attendu))

  # vérification mch
  mch <- unique(obtenu$mch)
  expect_equal(mch, c(NA, 0))

  # vérification t0
  t0 <- unique(obtenu$t0)
  t0_attendu <- unique(Data_test_for_simul_samare$Annee_Coupe)
  expect_equal(t0, t0_attendu)

  # vérification des valeurs de trt
  trt <- unique(obtenu$trt)
  expect_equal(trt, 'CP')

  # vérification des valeurs de ntrt
  ntrt <- unique(obtenu$ntrt)
  expect_equal(ntrt, 1)

  # vérification des valeurs de t0_aj_
  t0_aj <- obtenu %>% group_by(Placette, Annee) %>% slice(1) %>% ungroup() %>% select(t0_aj_)
  expect_equal(t0_aj$t0_aj_, c(NA, 2.1, 7.1, 12.1, 17.1, 0, 0))

})

# ok
test_that("SaMARE returns the expected data frame without Gaules and MCH = 1, avec coupe, sans qualite, sans mscr", {
  set.seed(NULL)
  set.seed(3)
  # ListeItertest <- data.frame(PlacetteID ="TEM23APC5000_1",
  #                             Placette = "TEM23APC5000",
  #                             Iter = 1)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>% mutate(Annee_Inventaire=2023)
  RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
  obtenu <- SaMARE(Random = RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                   Iteration=1,
                   Horizon = 6 ,
                   RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                   Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                   MCH = 1)

  attendu <- readRDS(test_path("fixtures/samare", "result_samare_sans_gaules_test_MCH_new2.rds"))
  set.seed(NULL)
  obtenu2 <- obtenu %>% select(-t0, -trt, -ntrt, -t0_aj_, -mch, -Annee_Inventaire)
  expect_equal(as.data.frame(obtenu2), as.data.frame(attendu))

  # vérification mch
  mch <- unique(obtenu$mch)
  expect_equal(mch, c(NA, 1))

})


# ok
test_that("SaMARE returns the expected data frame without Gaules and MCH = 0, sans coupe, sans qualite, sans mscr", {
  set.seed(NULL)
  set.seed(3)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare_sans_coupe.rds")) %>% mutate(Annee_Inventaire=2023)
  RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
  obtenu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                    Horizon = 6 ,
                    Iteration = 1,
                    #seed_value=NULL,
                    RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                    Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                    MCH = 0)
  attendu <- readRDS(test_path("fixtures/samare", "result_samare_sans_gaules_test_sans_coupe.rds"))
  set.seed(NULL)
  obtenu2 <- obtenu %>% select(-t0, -trt, -ntrt, -t0_aj_, -mch, -Annee_Inventaire)
  expect_equal(obtenu2, attendu)

  # vérification t0
  t0 <- unique(obtenu$t0)
  expect_equal(t0, NA_real_)

  # vérification des valeurs de trt
  trt <- unique(obtenu$trt)
  expect_equal(trt, 'TEM')

  # vérification des valeurs de ntrt
  ntrt <- unique(obtenu$ntrt)
  expect_equal(ntrt, 0)

  # vérification des valeurs de t0_aj_
  t0_aj <- obtenu %>% group_by(Placette, Annee) %>% slice(1) %>% ungroup() %>% select(t0_aj_)
  expect_equal(t0_aj$t0_aj_, c(NA, 0, 0, 0, 0, 0, 0))


})

# ok
test_that("SaMARE returns the expected data frame without Gaules and MCH = 0, avec coupe, sans qualite, avec mscr tous", {
  set.seed(NULL)
  set.seed(3)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare_avec_mscr_tous.rds")) %>% mutate(Annee_Inventaire=2023)
  RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
  obtenu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                   Horizon = 6 ,
                   Iteration = 1,
                   #seed_value=NULL,
                   RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                   Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                   MCH = 0)
  obtenu2 <- obtenu %>% select(-t0, -trt, -ntrt, -t0_aj_, -mch, -Annee_Inventaire)
  nom <- names(obtenu2)
  #attendu <- readRDS(test_path("fixtures/samare", "result_samare_avec_mscr_tous.rds"))
  attendu <- readRDS(test_path("fixtures/samare", "result_samare_avec_mscr_tous_v2.rds"))
  attendu <- as.data.frame(attendu) %>% select(all_of(nom))
  # MSCR de la step 0 est gardé et MSCR des autres steps est calculé
  set.seed(NULL)
  obtenu2 <- as.data.frame(obtenu2)
  expect_equal(obtenu2, attendu)
})

# ok
test_that("SaMARE returns the expected data frame without Gaules and MCH = 0, avec coupe, sans qualite, avec mscr pour certain", {
  set.seed(NULL)
  set.seed(3)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare_avec_mscr_certain.rds")) %>% mutate(Annee_Inventaire=2023)
  RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
  obtenu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                   Horizon = 6 ,
                   Iteration = 1,
                   #seed_value=NULL,
                   RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                   Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                   MCH = 0)
  attendu <- readRDS(test_path("fixtures/samare", "result_samare_avec_mscr_certain_v2.rds"))
  # MSCR de la step 0 est calculé pour tous et MSCR des autres steps est calculé
  set.seed(NULL)
  obtenu2 <- obtenu %>% select(-t0, -trt, -ntrt, -t0_aj_, -mch, -Annee_Inventaire)
  expect_equal(as.data.frame(obtenu2), as.data.frame(attendu))
})


# ok
test_that("SaMARE returns the expected data frame without Gaules and MCH = 0, avec coupe, avec qualite, sans mscr", {
  set.seed(NULL)
  set.seed(3)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare_avec_qualite.rds")) %>% mutate(Annee_Inventaire=2023)
  RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
  obtenu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                   Horizon = 6 ,
                   Iteration = 1,
                   #seed_value=NULL,
                   RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                   Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                   MCH = 0)
  attendu <- readRDS(test_path("fixtures/samare", "result_samare_avec_qualite_v2.rds"))
  # MSCR de la step 0 est calculé pour tous et MSCR des autres steps est calculé
  set.seed(NULL)
  obtenu2 <- obtenu %>% select(-t0, -trt, -ntrt, -t0_aj_, -mch, -Annee_Inventaire)
  obtenu2 <- as.data.frame(obtenu2)
  attendu2 <- as.data.frame(attendu)

  expect_equal(obtenu2, attendu2)
})

# ok
test_that("SaMARE returns the expected data frame without Gaules and MCH = 0, sans coupe, sans qualite, sans mscr, avec martele", {
  set.seed(NULL)
  set.seed(3)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare_avec_martele_sansCP.rds")) %>% mutate(Annee_Inventaire=2023)
  RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
  obtenu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                   Horizon = 1 ,
                   Iteration = 1,
                   #seed_value=NULL,
                   RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                   Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                   MCH = 0)
  attendu <- readRDS(test_path("fixtures/samare", "result_samare_avec_martele_sansCP.rds"))
  # en 2023, on doit avoir la liste d'arbres 2 fois, une avec residuel=0 (tous les arbres du fichier de départ)
  #                                                  une avec residuel=1 (les arbres du fichier de départ sans les martelés,
  # la simulation se fait ensuite avec ntrt=1 et annee_coupe=annee_inventaire
  set.seed(NULL)
  obtenu2 <- obtenu %>% select(-t0, -trt, -ntrt, -t0_aj_, -mch, -Annee_Inventaire)
  expect_equal(obtenu2, attendu)


  # vérification t0
  t0 <- unique(obtenu$t0)
  t0_attendu <- unique(Data_test_for_simul_samare$Annee_Inventaire) # pas de coupe, mais martele, donc t0 est l'année d'inventaire
  expect_equal(t0, c(NA, t0_attendu)) # NA au temps 0

  # vérification des valeurs de trt
  trt <- unique(obtenu$trt)
  expect_equal(trt, 'CP')

  # vérification des valeurs de ntrt
  ntrt <- unique(obtenu$ntrt)
  expect_equal(ntrt, c(0,1)) # au temps 0, pour residuel 0 ntrt=0 car pas de coupe, mais a residuel=1, le martelage est fait donc ntrt=1

  # vérification des valeurs de t0_aj_
  t0_aj <- obtenu %>% group_by(Placette, Annee) %>% slice(1) %>% ungroup() %>% select(t0_aj_)
  expect_equal(t0_aj$t0_aj_, c(NA, 0.1))



})

# ok
test_that("SaMARE returns the expected data frame without Gaules and MCH = 0, avec coupe, sans qualite, sans mscr, avec martele", {
  set.seed(NULL)
  set.seed(3)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare_avec_martele_avecCP.rds")) %>% mutate(Annee_Inventaire=2023)
  RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
  obtenu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                   Horizon = 1 ,
                   Iteration = 1,
                   #seed_value=NULL,
                   RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                   Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                   MCH = 0)
  attendu <- readRDS(test_path("fixtures/samare", "result_samare_avec_martele_avecCP.rds"))
  # en 2023, on doit avoir la liste d'arbres 2 fois, une avec residuel=0 (tous les arbres du fichier de départ)
  #                                                  une avec residuel=1 (les arbres du fichier de départ sans les martelés,
  # la simulation se fait ensuite avec ntrt+1 et annee_coupe=annee_inventaire
  set.seed(NULL)
  obtenu2 <- obtenu %>% select(-t0, -trt, -ntrt, -t0_aj_, -mch, -Annee_Inventaire)
  expect_equal(obtenu2, attendu)

  # vérification t0
  t0 <- unique(obtenu$t0)
  expect_equal(t0, c(2010, 2023)) # au temps 0, pour residuel=0, t0=annee_coupe, mais à residuel=1, le martelage est fait donc t0=annee_inventaire

  # vérification des valeurs de trt
  trt <- unique(obtenu$trt)
  expect_equal(trt, 'CP')

  # vérification des valeurs de ntrt
  ntrt <- unique(obtenu$ntrt)
  expect_equal(ntrt, c(1,2))  # au temps 0, pour residuel=0, ntrt=1, mais à residuel=1, le martelage est fait donc ntrt=1

  # vérification des valeurs de t0_aj_
  t0_aj <- obtenu %>% group_by(Placette, Annee) %>% slice(1) %>% ungroup() %>% select(t0_aj_)
  expect_equal(t0_aj$t0_aj_, c(NA, 0.1))


})


# ok
test_that("SaMARE returns the expected data frame without Gaules and MCH = 0, avec plusieurs placettes", {
  set.seed(NULL)
  set.seed(3)

  data <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare_plusieurs_placettes.rds")) %>% mutate(Annee_Inventaire = AnneeDep)
  RandomTest <- readRDS(test_path("fixtures/samare", "Random_tous.rds"))
  obtenu <- SaMARE(Random =RandomTest, Data = data, Gaules =NA,
                   Horizon = 4 ,
                   Iteration = 1,
                   #seed_value=NULL,
                   RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                   Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                   MCH = 0)


  attendu <- readRDS(test_path("fixtures/samare", "result_samare_plusieurs_placettes_v2.rds"))
  set.seed(NULL)
  expect_equal(obtenu, attendu)


  # avec CP, avec marteles, sans mscr, sans qualite
  # data1 <- Placette=1, AnneeDep=2023, Etat = ifelse(DHPcm>29, 11, Etat), ntrt=1, Annee_Coupe=2010)

  # sans CP, avec marteles, sans mscr, sans qualite
  # data2 <- Placette=2, AnneeDep=2015, Etat = ifelse(DHPcm>29, 11, Etat), ntrt=0, Annee_Coupe=NA)

  # sans CP, sans MSCR, sans martele, avec qualite
  # data3 <- Placette=3, AnneeDep=2022, MSCR=NA, DHPcm=28, DHPcm = ifelse(NoArbre==1, 22.9, DHPcm), ABCD = rep(c('A','B','C','D','A','B'),3)...

  # avec CP, avec MSCR, sans martele, sans qualite
  # data4 <- Placette=4, AnneeDep=2012, MSCR='C')

  # avec CP, sans MSCR, sans martele, sans qualite, d'autres essences
  # data5 <- Placette=5, AnneeDep=2020, Espece = rep(c('SAB','BOJ','ERS','BOP','HEG','THO','PRU','EPN','ERR'),2)) %>%

  # avec CP, sans MSCR, sans martele, sans qualite, d'autres essences et d'autres DHP
  # data6 <- data5 %>% mutate(Placette=6, DHPcm = rep(c(9.5, 10.2, 12.4, 11.2, 10.1, 13.0),3))

   obtenu1 <- obtenu %>% filter(Placette==1) # de 2023 + 30 ans = 2053, residuel 0/1 en 2023, qualite vide
   obtenu2 <- obtenu %>% filter(Placette==2) # de 2015 + 30 ans = 2045, residuel 0/1 en 2023, qualite vide
   obtenu3 <- obtenu %>% filter(Placette==3) # de 2022 + 30 ans = 2052, tous residuel=0, tous feuillus (6) dhp>23 avec qualite
   obtenu4 <- obtenu %>% filter(Placette==4) # de 2012 + 30 ans = 2042, tous residuel=0, qualite vide
   obtenu5 <- obtenu %>% filter(Placette==5) # de 2020 + 30 ans = 2050, tous residuel=0, qualite vide
   obtenu6 <- obtenu %>% filter(Placette==6) # de 2020 + 30 ans = 2050, tous residuel=0, qualite vide

  # max(obtenu1$Annee)
  # max(obtenu2$Annee)
  # max(obtenu3$Annee)
  # max(obtenu4$Annee)
  # max(obtenu5$Annee)
  # max(obtenu6$Annee)
  #
  # unique(obtenu1$Residuel)
  # unique(obtenu2$Residuel)
  # unique(obtenu3$Residuel)
  # unique(obtenu4$Residuel)
  # unique(obtenu5$Residuel)
  # unique(obtenu6$Residuel)
  #
  # unique(obtenu1$ABCD)
  # unique(obtenu2$ABCD)
  # unique(obtenu3$ABCD)
  # unique(obtenu4$ABCD)
  # unique(obtenu5$ABCD)
  # unique(obtenu6$ABCD)


  # placette 1
  # avec CP, avec marteles
  # AnneeDep=2023, Annee_Coupe=2010

  # vérification t0
  t0 <- unique(obtenu1$t0)
  expect_equal(t0, c(2010, 2023)) # au temps 0, pour residuel=0, t0=annee_coupe, mais à residuel=1, le martelage est fait donc t0=annee_inventaire

  # vérification des valeurs de trt
  trt <- unique(obtenu1$trt)
  expect_equal(trt, 'CP')

  # vérification des valeurs de ntrt
  ntrt <- unique(obtenu1$ntrt)
  expect_equal(ntrt, c(1,2))  # au temps 0, pour residuel=0, ntrt=1, mais à residuel=1, le martelage est fait donc ntrt=1

  # vérification des valeurs de t0_aj_
  t0_aj <- obtenu1 %>% group_by(Placette, Annee) %>% slice(1) %>% ungroup() %>% select(t0_aj_)
  expect_equal(t0_aj$t0_aj_, c(NA, 0.1, 5.1, 10.1, 15.1))



  # placette 2
  # sans CP, avec marteles
  # AnneeDep=2015, Annee_Coupe=NA

  # vérification t0
  t0 <- unique(obtenu2$t0)
  expect_equal(t0, c(NA, 2015))

  # vérification des valeurs de trt
  trt <- unique(obtenu2$trt)
  expect_equal(trt, 'CP')

  # vérification des valeurs de ntrt
  ntrt <- unique(obtenu2$ntrt)
  expect_equal(ntrt, c(0,1))

  # vérification des valeurs de t0_aj_
  t0_aj <- obtenu2 %>% group_by(Placette, Annee) %>% slice(1) %>% ungroup() %>% select(t0_aj_)
  expect_equal(t0_aj$t0_aj_, c(NA, 0.1, 5.1, 10.1, 15.1))


  # placette 3
  # sans CP, sans martele
  # AnneeDep=2022

  # vérification t0
  t0 <- unique(obtenu3$t0)
  expect_equal(t0, c(NA_real_))

  # vérification des valeurs de trt
  trt <- unique(obtenu3$trt)
  expect_equal(trt, 'TEM')

  # vérification des valeurs de ntrt
  ntrt <- unique(obtenu3$ntrt)
  expect_equal(ntrt, c(0))

  # vérification des valeurs de t0_aj_
  t0_aj <- obtenu3 %>% group_by(Placette, Annee) %>% slice(1) %>% ungroup() %>% select(t0_aj_)
  expect_equal(t0_aj$t0_aj_, c(NA, 0, 0, 0, 0))

  # placette 4
  # avec CP, sans martele
  # AnneeDep=2012, Annee_coupe=2021

  # vérification t0
  t0 <- unique(obtenu4$t0)
  expect_equal(t0, c(2007))

  # vérification des valeurs de trt
  trt <- unique(obtenu4$trt)
  expect_equal(trt, 'CP')

  # vérification des valeurs de ntrt
  ntrt <- unique(obtenu4$ntrt)
  expect_equal(ntrt, c(1))

  # vérification des valeurs de t0_aj_
  t0_aj <- obtenu4 %>% group_by(Placette, Annee) %>% slice(1) %>% ungroup() %>% select(t0_aj_)
  expect_equal(t0_aj$t0_aj_, c(NA, 5.1, 10.1, 15.1, 20.1))


  # placette 5
  # avec CP, sans martele
  # AnneeDep=2020,

  # vérification t0
  t0 <- unique(obtenu5$t0)
  expect_equal(t0, c(2020))

  # vérification des valeurs de trt
  trt <- unique(obtenu5$trt)
  expect_equal(trt, 'CP')

  # vérification des valeurs de ntrt
  ntrt <- unique(obtenu5$ntrt)
  expect_equal(ntrt, c(1))

  # vérification des valeurs de t0_aj_
  t0_aj <- obtenu5 %>% group_by(Placette, Annee) %>% slice(1) %>% ungroup() %>% select(t0_aj_)
  expect_equal(t0_aj$t0_aj_, c(NA, 0.1, 5.1, 10.1, 15.1))


  # placette 6
  # avec CP, sans martele
  # Annee_Inventaire=2020, Annee_Coupe=2021

  # vérification t0
  t0 <- unique(obtenu6$t0)
  expect_equal(t0, c(2020))

  # vérification des valeurs de trt
  trt <- unique(obtenu6$trt)
  expect_equal(trt, 'CP')

  # vérification des valeurs de ntrt
  ntrt <- unique(obtenu6$ntrt)
  expect_equal(ntrt, c(1))

  # vérification des valeurs de t0_aj_
  t0_aj <- obtenu6 %>% group_by(Placette, Annee) %>% slice(1) %>% ungroup() %>% select(t0_aj_)
  expect_equal(t0_aj$t0_aj_, c(NA, 0.1, 5.1, 10.1, 15.1))

})




# si dans le fichier de départ, ce sont déjà des arbres qui proviennent d'un peuplement qui a eu une CP auparavent
# et qu'il y a des arbres à martelés
# on doit faire ntrt+1 (car on enlevera les martelé et ça fera donc une cp de plus), et mettre annee_coupe=annee_inventaire
# mais on ne voit pas ces variables dans le output final
# il faudrait que la section des arbres marteles soit dans une fonction à part et tester cette fct





# il n'est pas possible de générer un cas où il n'y aura pas eu au moins 1 pas de simulation à l'intérieur de la fct SaMARE
# donc le else qui gère les MSCR ne sera jamais exécuté
# un cas avec MSCR dont certaines lignes ont le code et d'autres non + sans simul
# un cas avec MSCR est fourni pour tous + sans simul




#################################################################################
#################################################################################
#################################################################################

# ok
test_that("SaMARE returns the expected data frame with Gaules MCH=0", {
  set.seed(NULL)
  set.seed(3)

  Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>% mutate(Annee_Inventaire=2023)
  RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
  RandPlacStepGaules_test <- readRDS(test_path("fixtures/samare/avec_gaules", "RandPlacStepGaules_test.rds"))
  Gaules_test <- readRDS(test_path("fixtures/samare/avec_gaules", "Gaules_test.rds"))

  obtenu <- SaMARE(Random =RandomTest,RandomGaules=RandPlacStepGaules_test, Data = Data_test_for_simul_samare,
                       Gaules =Gaules_test, Horizon = 4 , Iteration=1,
                        RecruesGaules =1,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                        Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)

  set.seed(NULL)
  #attendu <- readRDS(test_path("fixtures/samare/avec_gaules", "result_samare_avec_gaules_test_new.rds"))
  attendu2 <- readRDS(test_path("fixtures/samare/avec_gaules", "result_samare_avec_gaules_test_new2.rds"))

  # att_gau1 <- attendu %>% filter(Annee>2025) %>% rename(Nb_Gaules_Ha1=Nb_Gaules_Ha) %>% select(Nb_Gaules_Ha1)
  # att_gau2 <- attendu2 %>% filter(Annee>2025) %>% rename(Nb_Gaules_Ha2=Nb_Gaules_Ha)  %>% select(Nb_Gaules_Ha2)
  # compare <- bind_cols(att_gau1, att_gau2) %>% mutate(diff=Nb_Gaules_Ha2-Nb_Gaules_Ha1) %>% filter(diff != 0)
  # expect_equal(nrow(compare),0)

  obtenu2 <- obtenu %>% select(-t0, -trt, -ntrt, -t0_aj_, -mch, -Annee_Inventaire)
  nom <- names(obtenu2)
  attendu2 <- attendu2 %>% select(all_of(nom))

  expect_equal(as.data.frame(obtenu2), as.data.frame(attendu2))

})

# ok
test_that("SaMARE returns the expected data frame with Gaules MCH=1", {
  set.seed(NULL)
  set.seed(3)

  RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
  RandPlacStepGaules_test <- readRDS(test_path("fixtures/samare/avec_gaules", "RandPlacStepGaules_test.rds"))
  Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>% mutate(Annee_Inventaire=2023)
  Gaules_test <- readRDS(test_path("fixtures/samare/avec_gaules", "Gaules_test.rds"))
  obtenu<-SaMARE(Random =RandomTest,RandomGaules=RandPlacStepGaules_test, Data = Data_test_for_simul_samare,
                       Gaules =Gaules_test, Iteration=1, Horizon = 4 ,
                       RecruesGaules =1,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                       Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 1)

  set.seed(NULL)
  attendu <- readRDS(test_path("fixtures/samare/avec_gaules", "result_samare_test_MCH_new2.rds"))

  obtenu2 <- obtenu %>% select(-t0, -trt, -ntrt, -t0_aj_, -mch, -Annee_Inventaire)
  nom <- names(obtenu2)
  attendu <- attendu %>% select(all_of(nom)) # pour enlever nb_gaules_68_ha, car n'est pas la somme de toutes les essences car seulement 4 ess sur les 10

  expect_equal(as.data.frame(obtenu2), as.data.frame(attendu))

})


# test avec gaules et plusieurs placettes

# ok
test_that("SaMARE returns the expected data frame without Gaules and MCH = 0, avec plusieurs placettes", {
  set.seed(NULL)
  set.seed(3)

  data_tous <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare_plusieurs_placettes.rds")) %>% mutate(Annee_Inventaire = AnneeDep)
  RandomTest <- readRDS(test_path("fixtures/samare", "Random_tous.rds"))

  RandPlacStepGaules_tous <- readRDS(test_path("fixtures/samare/avec_gaules", "RandPlacStepGaules_tous.rds")) # variance des effets aléatoires
  Gaules_tous <- readRDS(test_path("fixtures/samare/avec_gaules", "Gaules_tous.rds"))

  obtenu <- SaMARE(Random =RandomTest, Data = data_tous, Gaules = Gaules_tous, RandomGaules=RandPlacStepGaules_tous,
                    Horizon = 4 ,
                    Iteration = 1,
                    #seed_value=NULL,
                    RecruesGaules = 1, CovParms=MatchModuleCovparms, CovParmsGaules=CovparmGaules,
                    Para=MatchModuleParameters, ParaGaules=ParametresGaules, Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                    MCH = 0)

  attendu <- readRDS(test_path("fixtures/samare/avec_gaules", "result_samare_avec_gaules_plusieurs_placettes.rds"))

  expect_equal(obtenu, attendu)

})
