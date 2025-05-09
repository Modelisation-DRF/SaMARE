#############################################################################
#############################################################################


# Mort Un dataframe qui contient la liste des arbres pour lesquels
# t La longueur du pas de simulation en annee (en annees).
# MCH Variable prenant la veleur de 1 en présence de maladie corticale du hêtre dans la placette et 0 lorsque la maladie est absente. Lorsque la maladie corticale
# Para.mort Un dataframe  contenant les paramettres du module de mortalité.
# RandomMort Un dataframe contenant les effets aléatoires du module de mortalité


set.seed(10)
t=5
NbIter=1
Horizon=5

# préparer un fichier d'arbres
GrEspece <- c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB")
vigu0 <- c("NONVIG","ViG")
prod0 <- c("pate","resineux","sciage")
trt <- c("TEM","CP")
ntrt <- c(0,1,2)
type_pe_Plac <- c("type0","type1","type2")
rid1 <- c("2o","3a","3b","3c","3d","4e","4o","DU","SV")

data_test_module <- CJ(rid1=rid1, trt=trt, ntrt=ntrt, type_pe_Plac=type_pe_Plac, GrEspece = GrEspece, vigu0 = vigu0, prod0 = prod0)  # CJ est l'équivalent de expand.grid
data_test_module[, Placette := .GRP, by = .(rid1, trt, ntrt, type_pe_Plac)]

data_test_module <- data_test_module %>% mutate(Nombre=1, DHPcm=24, Etat='vivant', temp=1.2, prec=1000,
                                                DHPcm = ifelse(GrEspece=='ERR' & vigu0=='NONVIG', 15, DHPcm), # pour le test de produit il faut des nonvig en bas de 23cm
                                                DHPcm = ifelse(GrEspece=='BOJ', 23, DHPcm), # pour le test attribtion de qualité, il faut des arbres qui vont passer le seuil de >23cm
                                                ABCD = ifelse(DHPcm<23.05 | GrEspece %in% c("EPX","RES","SAB", "AUT"), NA, 'C'), # les <23 et les résineux n'ont pas de qualité
                                                Sup_PE = ifelse(type_pe_Plac=="type0", 0.04,
                                                                ifelse(type_pe_Plac=="type1",0.25, 1)),
                                                t0_aj_ = ifelse(trt=="TEM", 0,
                                                                ifelse(ntrt==1, 2, 5)),
                                                fact_red = ifelse(trt=="TEM", 0,
                                                                  ifelse(t0_aj_ <= 3, 1, 0))
)

data_test_module <- data_test_module %>% filter(trt=='TEM' & ntrt==0 | trt=='CP' & ntrt>0)
data_test_module <- data_test_module %>% filter(prod0=='resineux' & GrEspece %in% c("EPX","RES","SAB") | prod0!='resineux' & GrEspece %in% c("AUT","BOJ","ERR","ERS","FEN","FIN","HEG")) %>%
  mutate(ArbreID = row_number()) %>%
  group_by(Placette) %>%
  mutate(st_tot0 = sum((DHPcm/200)^2 *3.1416*Nombre)/Sup_PE,
         dens_tot0 = sum(Nombre)/Sup_PE)


# générer les effets aléatoires
RandPlacStep <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=data_test_module, NbIter=NbIter, NbPeriodes=Horizon)
RandomMort <- RandPlacStep %>% filter(SubModuleID==1, Step==2, Iter==1)

# paramètres des effets fixes
Para <- MatchModuleParameters %>%
  mutate(Effect = str_to_lower(Effect)) %>%
  rename(GrEspece=Ess_groupe) %>%
  select(-VegPotID,-Veg_Pot)
Para.mort <- ParaOmega(ModuleID = 1, ParaOri=Para, ParaIter=Para, Omega=MatchModuleOmega, NbIter=1)

mort_attendu_mch0 <- mort(data_test_module, t, MCH=0 , Para.mort, RandomMort, seed_value=10)

mort_attendu_mch1 <- mort(data_test_module, t, MCH=1 , Para.mort, RandomMort, seed_value=10)


saveRDS(data_test_module, "tests/testthat/fixtures/mortalite/data_test_module.rds")

saveRDS(mort_attendu_mch0, "tests/testthat/fixtures/mortalite/mort_attendu_mch0.rds")
saveRDS(mort_attendu_mch1, "tests/testthat/fixtures/mortalite/mort_attendu_mch1.rds")
saveRDS(Para.mort, "tests/testthat/fixtures/mortalite/Para.mort.rds")
saveRDS(RandomMort, "tests/testthat/fixtures/mortalite/RandomMort.rds")



#############################################################################
#############################################################################


######################################################################
######################################################################

# Accrois Un dataframe qui contient la liste des arbres à simuler
# t    La longueur du pas de simulation en annee (en annees).
# Para.acc  Un dataframe  contenant les paramettres du module d'accroissement.
# RandomAcc Un dataframe  contenant les effets aléatoire du module d'accroissement.
# Res Un dataframe d'une colonne contenant les erreur residuelles du module d'accroissement.


set.seed(10)
t=5
NbIter=1
Horizon=5


# data_test_module_acc <- data_test_module
# data_test_module_acc[1,"Etat"] <- 'mort'
# data_test_module_acc <- data_test_module_acc %>% rename(Etat1=Etat)

# Utiliser comme intrant le fichier généré pour le test de mortalité: mort_attendu_mch0
data_test_module_acc <- readRDS("tests/testthat/fixtures/mortalite/mort_attendu_mch0.rds")

# générer les effets aléatoires
RandPlacStep <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=data_test_module_acc, NbIter=NbIter, NbPeriodes=Horizon)
RandomAcc <- RandPlacStep %>% filter(SubModuleID==2, Step==2, Iter==1)

# paramètres des effets fixes
Para <- MatchModuleParameters %>%
  mutate(Effect = str_to_lower(Effect)) %>%
  rename(GrEspece=Ess_groupe) %>%
  select(-VegPotID,-Veg_Pot)
Para.acc <- ParaOmega(ModuleID = 2, ParaOri=Para, ParaIter=Para, Omega=MatchModuleOmega, NbIter=1)

# Erreur résiduelle
# Residus[,k+2]
# Le fichier Residus a une ligne par placette et une colonne par période (Periode_1, ..., Periode=n),
# en plus des colonnes Placette et ArbreID
# on ne passe que la colonne de la periode concernée dans la fct accroissement et son nom n'a pas d'importance
Res <- rnorm(nrow(data_test_module_acc))

acc_attendu <- accrois(data_test_module_acc, t, Para.acc, RandomAcc, Res)

saveRDS(data_test_module_acc, "tests/testthat/fixtures/accroissement/data_test_module_acc.rds")

saveRDS(acc_attendu, "tests/testthat/fixtures/accroissement/acc_attendu.rds")
saveRDS(Para.acc, "tests/testthat/fixtures/accroissement/Para.acc.rds")
saveRDS(RandomAcc, "tests/testthat/fixtures/accroissement/RandomAcc.rds")
saveRDS(Res, "tests/testthat/fixtures/accroissement/Res.rds")



######################################################################
######################################################################

# module de vigueur

set.seed(10)
t=5
NbIter=1
Horizon=5


# Utiliser comme intrant le fichier généré pour le test de accroissement
data_test_module_vig <- readRDS("tests/testthat/fixtures/accroissement/acc_attendu.rds")


# générer les effets aléatoires
RandPlacStep <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=data_test_module_vig, NbIter=NbIter, NbPeriodes=Horizon)
RandomVig <- RandPlacStep %>% filter(SubModuleID==3, Step==2, Iter==1)

# paramètres des effets fixes
Para <- MatchModuleParameters %>%
  mutate(Effect = str_to_lower(Effect)) %>%
  rename(GrEspece=Ess_groupe) %>%
  select(-VegPotID,-Veg_Pot)
Para.vig <- ParaOmega(ModuleID = 3, ParaOri=Para, ParaIter=Para, Omega=MatchModuleOmega, NbIter=1)

vig_attendu <- vig(data_test_module_vig, Para.vig, RandomVig, seed_value=10)


saveRDS(data_test_module_vig, "tests/testthat/fixtures/vigueur/data_test_module_vig.rds")
saveRDS(vig_attendu, "tests/testthat/fixtures/vigueur/vig_attendu.rds")
saveRDS(Para.vig, "tests/testthat/fixtures/vigueur/Para.vig.rds")
saveRDS(RandomVig, "tests/testthat/fixtures/vigueur/RandomVig.rds")



######################################################################
######################################################################

# module de produit

set.seed(10)
t=5
NbIter=1
Horizon=5


# Utiliser comme intrant le fichier généré pour le test de vigueur
data_test_module_prod <- readRDS("tests/testthat/fixtures/vigueur/vig_attendu.rds")


# générer les effets aléatoires
RandPlacStep <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=data_test_module_prod, NbIter=NbIter, NbPeriodes=Horizon)
RandomProd <- RandPlacStep %>% filter(SubModuleID==4, Step==2, Iter==1)

# paramètres des effets fixes
Para <- MatchModuleParameters %>%
  mutate(Effect = str_to_lower(Effect)) %>%
  rename(GrEspece=Ess_groupe) %>%
  select(-VegPotID,-Veg_Pot)
Para.prod <- ParaOmega(ModuleID = 4, ParaOri=Para, ParaIter=Para, Omega=MatchModuleOmega, NbIter=1)

prod_attendu <- produit(data_test_module_prod, Para.prod, RandomProd, seed_value=10)


saveRDS(data_test_module_prod, "tests/testthat/fixtures/produit/data_test_module_prod.rds")
saveRDS(prod_attendu, "tests/testthat/fixtures/produit/prod_attendu.rds")
saveRDS(Para.prod, "tests/testthat/fixtures/produit/Para.prod.rds")
saveRDS(RandomProd, "tests/testthat/fixtures/produit/RandomProd.rds")



######################################################################
######################################################################

# module de evolution de la qualité

set.seed(10)
t=5
NbIter=1
Horizon=5





# Générer les paramètres evolution de la qualité
ParaBOJ<-ParametresEvolQual[which(ParametresEvolQual$Ess_groupe=="BOJ"),]
ParaERR<-ParametresEvolQual[which(ParametresEvolQual$Ess_groupe=="ERR"),]
ParaERS<-ParametresEvolQual[which(ParametresEvolQual$Ess_groupe=="ERS"),]
ParaFEN<-ParametresEvolQual[which(ParametresEvolQual$Ess_groupe=="FEN"),]
ParaHEG<-ParametresEvolQual[which(ParametresEvolQual$Ess_groupe=="HEG"),]

OmegaBOJ<-OmegaEvolQual[which(OmegaEvolQual$Ess_groupe=="BOJ"),]
OmegaERR<-OmegaEvolQual[which(OmegaEvolQual$Ess_groupe=="ERR"),]
OmegaERS<-OmegaEvolQual[which(OmegaEvolQual$Ess_groupe=="ERS"),]
OmegaFEN<-OmegaEvolQual[which(OmegaEvolQual$Ess_groupe=="FEN"),]
OmegaHEG<-OmegaEvolQual[which(OmegaEvolQual$Ess_groupe=="HEG"),]


Para.EvolQualBOJ1<-ParaOmega(ModuleID = 1,ParaOri=ParaBOJ, ParaIter=ParaBOJ,Omega=OmegaBOJ,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualBOJ2<-ParaOmega(ModuleID = 2,ParaOri=ParaBOJ, ParaIter=ParaBOJ,Omega=OmegaBOJ,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualBOJ3<-ParaOmega(ModuleID = 3,ParaOri=ParaBOJ, ParaIter=ParaBOJ,Omega=OmegaBOJ,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualERR1<-ParaOmega(ModuleID = 1,ParaOri=ParaERR, ParaIter=ParaERR,Omega=OmegaERR,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualERR2<-ParaOmega(ModuleID = 2,ParaOri=ParaERR, ParaIter=ParaERR,Omega=OmegaERR,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualERS1<-ParaOmega(ModuleID = 1,ParaOri=ParaERS, ParaIter=ParaERS,Omega=OmegaERS,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualERS2<-ParaOmega(ModuleID = 2,ParaOri=ParaERS, ParaIter=ParaERS,Omega=OmegaERS,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualERS3<-ParaOmega(ModuleID = 3,ParaOri=ParaERS, ParaIter=ParaERS,Omega=OmegaERS,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualFEN1<-ParaOmega(ModuleID = 1,ParaOri=ParaFEN, ParaIter=ParaFEN,Omega=OmegaFEN,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualFEN2<-ParaOmega(ModuleID = 2,ParaOri=ParaFEN, ParaIter=ParaFEN,Omega=OmegaFEN,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualHEG1<-ParaOmega(ModuleID = 1,ParaOri=ParaHEG, ParaIter=ParaHEG,Omega=OmegaHEG,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualHEG2<-ParaOmega(ModuleID = 2,ParaOri=ParaHEG, ParaIter=ParaHEG,Omega=OmegaHEG,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])
Para.EvolQualHEG3<-ParaOmega(ModuleID = 3,ParaOri=ParaHEG, ParaIter=ParaHEG,Omega=OmegaHEG,NbIter=1) #%>%mutate(Iter=PlacOri$Iter[1])

Para.EvolQual<-list(Para.EvolQualBOJ1,Para.EvolQualBOJ2,Para.EvolQualBOJ3,Para.EvolQualERR1,Para.EvolQualERR2,Para.EvolQualERS1,
                    Para.EvolQualERS2,Para.EvolQualERS3,Para.EvolQualFEN1,Para.EvolQualFEN2,Para.EvolQualHEG1,Para.EvolQualHEG2,
                    Para.EvolQualHEG3)
Para.EvolQualTot<-c()
for(i in 1:13){

  Parai<-Para.EvolQual[[i]][,3]
  Parai<-Parai[which(Parai$ParameterEstimate!=0),]
  Para.EvolQualTot<- rbind(Para.EvolQualTot,Parai)

}
rm(ParaBOJ,ParaERR,ParaERS,ParaFEN,ParaHEG,OmegaBOJ,OmegaERR,OmegaERS,OmegaFEN,OmegaHEG,Para.EvolQualBOJ1,Para.EvolQualBOJ2,
   Para.EvolQualBOJ3,Para.EvolQualERR1,Para.EvolQualERR2,Para.EvolQualERS1,Para.EvolQualERS2,Para.EvolQualERS3,Para.EvolQualFEN1,
   Para.EvolQualFEN2,Para.EvolQualHEG1,Para.EvolQualHEG2,Para.EvolQualHEG3,Para.EvolQual)


# créer un fichier test juste pour evol qualite pour tester tous les cas possibes
# utiliser rid1 <- c("2o","3a","3b","3c","3d","4e","4o","DU","SV") pour changer le dhp de certaines essences

# Utiliser comme intrant le fichier généré pour le test de produit
data_test_module_qual <- readRDS("tests/testthat/fixtures/produit/prod_attendu.rds")


# if faut des dhpcm1 < 33.1: ok, ils le sont tous
data_test_module_qual <- data_test_module_qual %>%
  mutate(DHPcm1 = ifelse(rid1=="2o" & GrEspece %in% c("BOJ","ERS","HEG") & Etat1=='vivant', 40, DHPcm1),  # il faut des BOJ ERS-HEG avec dhpcm1>39.1, donc dhpcm>40
         DHPcm1 = ifelse(rid1=="3a" & Etat1=='vivant', 33.2, DHPcm1),  # si un arbre vient de franchir le seuil du 33.1 cm: il faut des arbres à dhpcm=33cm avec dhpcm1=33.2
         DHPcm = ifelse(rid1=="3a" & Etat1=='vivant', 33, DHPcm),
         DHPcm1 = ifelse(rid1=="3b" & GrEspece %in% c("BOJ","ERS","HEG") & Etat1=='vivant', 39.2, DHPcm1),   # si un BOJ/ERS/HEG vient de franchir le seuil du 39.1 cm: donc il faut des BOJ/ERS/HEG à dhpcm=39cm
         DHPcm = ifelse(rid1=="3b" & GrEspece %in% c("BOJ","ERS","HEG") & Etat1=='vivant', 39, DHPcm),
         ABCD = ifelse(rid1=="3c" & GrEspece %in% c("ERR","FIN","FEN") & Etat1=='vivant', "A", ABCD) # il n'y a pas de modèle d'évol de la qualité pour les ERR/FEN/FIN de qualité A
         )

qual_attendu <- EvolQual(data_test_module_qual, Para.EvolQualTot, seed_value=10)


saveRDS(data_test_module_qual, "tests/testthat/fixtures/qualite_evol/data_test_module_qual.rds")
saveRDS(qual_attendu, "tests/testthat/fixtures/qualite_evol/qual_attendu.rds")
saveRDS(Para.EvolQualTot, "tests/testthat/fixtures/qualite_evol/Para.qual.rds")



######################################################################
######################################################################

# module de evolution de la qualité

set.seed(10)
t=5
NbIter=1
Horizon=5


# Utiliser comme intrant le fichier généré pour le test d'évolution de la qualité
data_test_module_qual <- readRDS("tests/testthat/fixtures/qualite_evol/qual_attendu.rds")

qual_attendu <- AttribQualFct(data_test_module_qual, seed_value=10)


saveRDS(data_test_module_qual, "tests/testthat/fixtures/qualite_attrib/data_test_module_qual.rds")
saveRDS(qual_attendu, "tests/testthat/fixtures/qualite_attrib/qual_attendu.rds")




######################################################################
######################################################################

# module de recrutementc nb pi

# test préparé par Junior

set.seed(10)
t=5
NbIter=1
Horizon=5

Para.rec_n_for_test <- readRDS(test_path("fixtures/recrue_nb_pi", "Para.rec_n_for_test.rds")) # fichier préparé par Junior
Rec_test <- readRDS(test_path("fixtures/recrue_nb_pi", "Rec_test.rds")) # fichier préparé par Junior

# je dois mettre maintenant les info placette dans le fichier
Rec_test_pi <- Rec_test %>%
  mutate(Placette=4,
         st_tot0=36.75691,
         t0_aj_=27.1,
         type_pe_Plac="type0",
         ntrt=1) %>%
  select(-contains("pred"))

# effet aléatoire de placette du module de probabilite de recrue
RandPlacStep <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=data_test_module_prod, NbIter=NbIter, NbPeriodes=Horizon)
RandomRec <- RandPlacStep %>% filter(SubModuleID==5, Step==2, Iter==1) %>% filter(Placette==4)

rec_pi_attendu <- rec_pi(Rec=Rec_test_pi,5, Para.rec_n_for_test, RandomRec)

saveRDS(rec_pi_attendu, "tests/testthat/fixtures/recrue_nb_pi/rec_pi_attendu.rds")
saveRDS(Rec_test_pi, "tests/testthat/fixtures/recrue_nb_pi/data_test_rec_pi.rds")
saveRDS(RandomRec, "tests/testthat/fixtures/recrue_nb_pi/RandomRec.rds")


######################################################################
######################################################################

# module de recrutement nb lambda

# test préparé par Junior

set.seed(10)
t=5
NbIter=1
Horizon=5

Para.rec_n_for_test <- readRDS(test_path("fixtures/recrue_nb_lambda", "Para.rec_n_for_test.rds")) # fichier préparé par Junior
Rec_test <- readRDS(test_path("fixtures/recrue_nb_lambda", "rec_lam_test.rds"))  # fichier préparé par Junior

# je dois mettre maintenant les info placette dans le fichier
Rec_test_lambda <- Rec_test %>%
  mutate(Placette=4,
         st_tot0=21.5900825754271,
         type_pe_Plac="type0") %>%
  select(-contains("pred"))

rec_lambda_attendu <- rec_lambda(Rec=Rec_test_lambda,5, Para.rec_n_for_test)

saveRDS(rec_lambda_attendu, "tests/testthat/fixtures/recrue_nb_lambda/rec_lambda_attendu.rds")
saveRDS(Rec_test_lambda, "tests/testthat/fixtures/recrue_nb_lambda/Rec_test_lambda.rds")



######################################################################
######################################################################

# module de recrutement nb delta

# test préparé par Junior

set.seed(10)
t=5
NbIter=1
Horizon=5

Para.rec_n_for_test <- readRDS(test_path("fixtures/recrue_nb_delta", "Para.rec_n_for_test.rds")) # fichier préparé par Junior
Rec_test <-            readRDS(test_path("fixtures/recrue_nb_delta", "rec_lam_test.rds"))  # fichier préparé par Junior

# je dois mettre maintenant les info placette dans le fichier
Rec_test_delta <- Rec_test %>%
  mutate(Placette=4,
         st_tot0=21.5900825754271,
         type_pe_Plac="type0",
         ntrt=1,
         t0_aj_=2.1) %>%
  select(-contains("pred"))


rec_delta_attendu <- rec_delta(Rec=Rec_test_delta, Para.rec_n_for_test)

saveRDS(rec_delta_attendu, "tests/testthat/fixtures/recrue_nb_delta/rec_delta_attendu.rds")
saveRDS(Rec_test_delta, "tests/testthat/fixtures/recrue_nb_delta/Rec_test_delta.rds")



######################################################################
######################################################################

# nombre de recrues

# Préparer le fichier pour le test

set.seed(10)
t=5
NbIter=1
Horizon=5

var_plot <- c("AnneeDep", "trt", "ntrt", "t0", "pente", "Sup_PE", "type_pe_Plac", "dom", "rid1",
              "vegp", "latitude", "longitude", "altitude", "reg", "teco",
              "prec", "temp", "grwd", "t0_aj_", "fact_red", "st_tot0", "dens_tot0")

# fichier des placettes
Plac <- readRDS("tests/testthat/fixtures/produit/data_test_module_prod.rds")
Plac <- Plac %>%
  mutate(
    AnneeDep=2025,
    t0=NA,
    pente=6.5,
    dom='2',
    vegp='FE3',
    latitude=46.5,
    longitude=-76,
    altitude=100,
    reg='2a',
    teco='FE32',
    grwd=500
  )

Rec1 <- data.frame("GrEspece"=c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),
                   "GrEssRec"=c("feu","feu","rex","feu","ers","feu","rex","heg","rex","sab"))

# répéter rec autant de fois que le nombre de placettes
list_plot <- unique(Plac$Placette)
Rec <- NULL
for (i in 1:length(list_plot)){
  Rec_temp <- Rec1 %>% mutate(Placette=list_plot[i])
  Rec <- bind_rows(Rec,Rec_temp)
}

# calcul de la ST des 10-14cm par essence
StEss_1014 <- Plac %>%
  mutate(Stm2ha = ifelse(Etat=="vivant" & DHPcm <15.1, (DHPcm/200)^2*3.1416*Nombre/Sup_PE, 0)) %>%
  group_by(Placette, GrEspece) %>%
  summarise(logst_ess_1014 = log(sum(Stm2ha)+0.01))

Rec <- left_join(Rec, StEss_1014, by=c('Placette','GrEspece')) %>%
  mutate(logst_ess_1014 = ifelse(is.na(logst_ess_1014)==TRUE, log(0.01), logst_ess_1014))

  # ajouter les variables à l'échelle de la placette
Rec <- Plac %>%
  select(Placette, all_of(var_plot)) %>%
  group_by(Placette) %>%
  slice(1) %>%
  left_join(Rec,by='Placette')

Para.rec_n <- readRDS(test_path("fixtures/recrue_nb_delta", "Para.rec_n_for_test.rds"))

RandPlacStep <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=data_test_module_prod, NbIter=NbIter, NbPeriodes=Horizon)
RandomRec <- RandPlacStep %>% filter(SubModuleID==5, Step==2, Iter==1)

rec_nb_attendu <- rec_n(Rec, t, Para.rec_n, RandomRec, seed_value = 10)

saveRDS(rec_nb_attendu, "tests/testthat/fixtures/recrue_nb/rec_nb_attendu.rds")
saveRDS(Para.rec_n, "tests/testthat/fixtures/recrue_nb/Para.rec_n.rds")
saveRDS(RandomRec, "tests/testthat/fixtures/recrue_nb/RandomRec.rds")
saveRDS(Rec, "tests/testthat/fixtures/recrue_nb/Rec.rds")



######################################################################
######################################################################

# module de recrutement dhp

# test préparé par Junior

set.seed(10)
t=5
NbIter=1
Horizon=5

Para.rec_dhp_for_test <- readRDS(test_path("fixtures/recrue_dhp", "Para.rec_dhp_for_test.rds")) # fichier préparé par Junior
Rec_test <- readRDS(test_path("fixtures/recrue_dhp", "RecSelect_test.rds"))  # fichier préparé par Junior

# je dois mettre maintenant les info placette dans le fichier
Rec_test_dhp <- Rec_test %>%
  mutate(Placette=4,
         st_tot0=27.6458293053024,
         dens_tot0 =453.325,
         type_pe_Plac="type0",
         ntrt=1) %>%
  select(-vigu1, -prod1, -DHPcm1, -Etat1)

# paramètres pour dhp des recrues
RandPlacStep <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=data_test_module_prod, NbIter=NbIter, NbPeriodes=Horizon)
RandomRec <- RandPlacStep %>% filter(SubModuleID==6, Step==2, Iter==1) %>% filter(Placette==4)
varRecDhp <- CovParms$ParameterEstimate[which(CovParms$CovParm=="sigma2_res")]
theta <- CovParms$ParameterEstimate[which(CovParms$CovParm=="theta")]

rec_dhp_attendu <- rec_dhp(Rec=Rec_test_dhp, t, Para.rec_dhp_for_test, RandomRec, varRecDhp, theta, seed_value=10)

saveRDS(rec_dhp_attendu, "tests/testthat/fixtures/recrue_dhp/rec_dhp_attendu.rds")
saveRDS(Rec_test_dhp, "tests/testthat/fixtures/recrue_dhp/Rec_test_dhp.rds")
saveRDS(RandomRec, "tests/testthat/fixtures/recrue_dhp/RandomRec.rds")
saveRDS(theta, "tests/testthat/fixtures/recrue_dhp/theta.rds")
saveRDS(varRecDhp, "tests/testthat/fixtures/recrue_dhp/varRecDhp.rds")



######################################################################
######################################################################

# module de recrutement vigueur

# test préparé par Junior

set.seed(10)
t=5
NbIter=1
Horizon=5

Para.rec_vig_test <- readRDS(test_path("fixtures/recrue_vig", "Para.rec_vig_test.rds")) # fichier préparé par Junior
Rec_test <- readRDS(test_path("fixtures/recrue_vig", "RecSelect_test.rds"))  # fichier préparé par Junior

# je dois mettre maintenant les info placette dans le fichier
Rec_test_vig <- Rec_test %>%
  mutate(Placette=4,
         st_tot0=27.6458293053024,
         dens_tot0 =453.325,
         type_pe_Plac="type0",
         ntrt=1,
         latitude=46.7) %>%
  select(-vigu1, -prod1, -DHPcm1, -Etat1)

# paramètres pour vig des recrues
data_test_module_prod <- readRDS("tests/testthat/fixtures/vigueur/vig_attendu.rds") # fichier des placettes
RandPlacStep <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=data_test_module_prod, NbIter=NbIter, NbPeriodes=Horizon)
RandomRec <- RandPlacStep %>% filter(SubModuleID==7, Step==2, Iter==1) %>% filter(Placette==4)

rec_vig_attendu <- rec_vig(Rec_test_vig, Para.rec_vig_test, RandomRec, seed_value=10)

saveRDS(rec_vig_attendu, "tests/testthat/fixtures/recrue_vig/rec_vig_attendu.rds")
saveRDS(Rec_test_vig, "tests/testthat/fixtures/recrue_vig/Rec_test_vig.rds")
saveRDS(RandomRec, "tests/testthat/fixtures/recrue_vig/RandomRec.rds")


######################################################################
######################################################################

# module de recrutement produit

# test préparé par Junior

set.seed(10)
t=5
NbIter=1
Horizon=5

Para.rec_prod_test <- readRDS(test_path("fixtures/recrue_prod", "Para.rec_prod_test.rds")) # fichier préparé par Junior
Rec_test <- readRDS(test_path("fixtures/recrue_prod", "RecSelect_test.rds"))  # fichier préparé par Junior

# je dois mettre maintenant les info placette dans le fichier
Rec_test_prod <- Rec_test %>%
  mutate(Placette=4,
         st_tot0=27.6458293053024,
         dens_tot0 =453.325,
         type_pe_Plac="type0",
         ntrt=1,
         latitude=46.7,
         rid1='3b') %>%
  select(-prod1, -DHPcm1, -Etat1)

# paramètres pour prod des recrues
data_test_module_prod <- readRDS("tests/testthat/fixtures/vigueur/vig_attendu.rds") # fichier des placettes
RandPlacStep <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=data_test_module_prod, NbIter=NbIter, NbPeriodes=Horizon)
RandomRec <- RandPlacStep %>% filter(SubModuleID==7, Step==2, Iter==1) %>% filter(Placette==4)

rec_prod_attendu <- rec_prod(Rec_test_prod, Para.rec_prod_test, RandomRec, seed_value=10)

saveRDS(rec_prod_attendu, "tests/testthat/fixtures/recrue_prod/rec_prod_attendu.rds")
saveRDS(Rec_test_prod, "tests/testthat/fixtures/recrue_prod/Rec_test_prod.rds")
saveRDS(RandomRec, "tests/testthat/fixtures/recrue_prod/RandomRec.rds")



######################################################################
######################################################################

# Évolution des gaules


# Nombre de gaules total

# Rec Dataframe qui contient les variables St_Ess_Ha, lnNb_Ess_Ha, trt, t0_aj_ par Placette/GrEspece
Rec <- data.frame(Placette=rep(1, 10),
                  GrEspece = c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),
                  St_Ess_Ha = c(2, 10, 0, 2, 15, 0, 0, 5, 0, 2),
                  trt='TEM',
                  t0_aj_=0,
                  dens_tot0 = 1500) %>%
  mutate(lnNb_Ess_Ha = log(St_Ess_Ha+1))
Rec2 <- Rec %>% mutate(Placette=2, trt='CP', t0_aj_=0.1)
Rec <- bind_rows(Rec, Rec2)

# RecGaules Dataframe qui contient la variable Nb_Gaules_Ess_Ha par Placette/GrEspece
RecGaules <- data.frame(Placette=rep(1, 10),
                        GrEspece = c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),
                        Nb_Gaules_Ess_Ha = c(10, 1000, 2, 50, 0, 2000, 2, 1, 500, 20))
RecGaules2 <- RecGaules %>% mutate(Placette=2)
RecGaules <- bind_rows(RecGaules, RecGaules2)

# RandomPlacGaules Dataframe contenant les effets aléatoires à l'échelle de la placette du module de gaules
RandomPlacGaules <- RandomPlacStepGaules(CovParms=CovparmGaules, Data=RecGaules, NbIter=1)

# Para.nb_gaules Paramètres de l'équation de la prévision du nombre total de gaules.
ParaGaules <- ParametresGaules %>% rename(GrEspece=Ess_groupe) # ParametresGaules est un fichier interne
Para.nb_gaules <- ParaOmega(ModuleID = 11,ParaOri=ParaGaules,ParaIter=ParaGaules,Omega=OmegaGaulesFormat,NbIter=1) %>% mutate(Iter=1)

attendu <- nb_Gaules(Rec=Rec, RecGaules=RecGaules, t=5, RandomPlacGaules=RandomPlacGaules, Para.nb_gaules=Para.nb_gaules)

saveRDS(attendu, test_path("fixtures/gaules_evolution", "attendu_nb_gaules_total.rds"))
saveRDS(Para.nb_gaules, test_path("fixtures/gaules_evolution", "Para.nb_gaules.rds"))
saveRDS(RandomPlacGaules, test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))
saveRDS(RecGaules, test_path("fixtures/gaules_evolution", "RecGaules_nb_gaules_total.rds"))
saveRDS(Rec, test_path("fixtures/gaules_evolution", "Rec_nb_gaules_total.rds"))


# ratio_pi_Gaules

# RecGaules Dataframe qui contient les variables lnNb_Gaules_Ess_Ha et Ratio par Placette/GrEspece
RecGaules <- data.frame(Placette=rep(1, 10),
                        GrEspece = c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),
                        Nb_Gaules_Ess_Ha = c(10, 1000, 2, 50, 0, 2000, 2, 1, 500, 20)) %>%
  mutate(lnNb_Gaules_Ess_Ha = log(Nb_Gaules_Ess_Ha+1))
RecGaules2 <- RecGaules %>% mutate(Placette=2)
RecGaules <- bind_rows(RecGaules, RecGaules2)

# Rec Dataframe qui contient les variables lnNb_Ess_Ha, longitude, latitude par Placette/GrEspece
Rec <- data.frame(Placette=rep(1, 10),
                  GrEspece = c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),
                  St_Ess_Ha = c(2, 10, 0, 2, 15, 0, 0, 5, 0, 2),
                  trt='TEM',
                  t0_aj_=0,
                  dens_tot0 = 1500,
                  longitude=-74,
                  latitude=50) %>%
  mutate(lnNb_Ess_Ha = log(St_Ess_Ha+1))
Rec2 <- Rec %>% mutate(Placette=2, trt='CP', t0_aj_=0.1)
Rec <- bind_rows(Rec, Rec2)

RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))

# Para.nb_gaules Paramètres de l'équation de la prévision du nombre total de gaules.
ParaGaules <- ParametresGaules %>% rename(GrEspece=Ess_groupe) # ParametresGaules est un fichier interne
Para.ratio_gaules <- ParaOmega(ModuleID = 12,ParaOri=ParaGaules,ParaIter=ParaGaules,Omega=OmegaGaulesFormat,NbIter=1) %>% mutate(Iter=1)

Ratio <- data.frame(Placette=c(rep(1, 10), rep(2, 10)),
                    GrEspece = rep(c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),2))

attendu <- ratio_pi_Gaules(Ratio=Ratio, Rec=Rec, RecGaules=RecGaules, t=5, RandomPlacGaules=RandomPlacGaules, Para.ratio_gaules=Para.ratio_gaules)

saveRDS(attendu, test_path("fixtures/gaules_evolution", "attendu_pi_gaules.rds"))
saveRDS(Para.ratio_gaules, test_path("fixtures/gaules_evolution", "Para.ratio_gaules.rds"))
saveRDS(RecGaules, test_path("fixtures/gaules_evolution", "RecGaules_pi_gaules.rds"))
saveRDS(Rec, test_path("fixtures/gaules_evolution", "Rec_pi_gaules.rds"))
saveRDS(Ratio, test_path("fixtures/gaules_evolution", "Ratio_pi_gaules.rds"))



# ratio_count_Gaules



# RecGaules Dataframe qui contient les variables lnNb_Gaules_Ess_Ha et Ratio par Placette/GrEspece
RecGaules <- data.frame(Placette=rep(1, 10),
                        GrEspece = c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),
                        Nb_Gaules_Ess_Ha = c(10, 1000, 2, 50, 0, 2000, 2, 1, 500, 20)) %>%
  group_by(Placette) %>%
  mutate(lnNb_Gaules_Ess_Ha = log(Nb_Gaules_Ess_Ha+1),
         Ratio = Nb_Gaules_Ess_Ha/sum(Nb_Gaules_Ess_Ha))
RecGaules2 <- RecGaules %>% mutate(Placette=2)
RecGaules <- bind_rows(RecGaules, RecGaules2)

# Rec Dataframe qui contient les variables st_tot0, dens_tot0, lnNb_Ess_Ha, prec, trt, t0_aj_, longitude Placette/GrEspece
Rec <- data.frame(Placette=rep(1, 10),
                  GrEspece = c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),
                  St_Ess_Ha = c(2, 10, 0, 2, 15, 0, 0, 5, 0, 2),
                  trt='TEM',
                  t0_aj_=0,
                  dens_tot0 = 1500,
                  longitude=-74,
                  latitude=50) %>%
  group_by(Placette) %>%
  mutate(lnNb_Ess_Ha = log(St_Ess_Ha+1),
         st_tot0 = sum(St_Ess_Ha),
         prec=1000)
Rec2 <- Rec %>% mutate(Placette=2, trt='CP', t0_aj_=0.1)
Rec <- bind_rows(Rec, Rec2)

# Ratio Dataframe qui possède une ligne par groupe d'espèce dans lequel les prévisions de ratios seront rapportées.
Ratio <- data.frame(Placette=c(rep(1, 10), rep(2, 10)),
                    GrEspece = rep(c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),2))

Para.ratio_gaules <- readRDS(test_path("fixtures/gaules_evolution", "Para.ratio_gaules.rds"))
RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))

attendu <- ratio_count_Gaules(Ratio=Ratio, Rec=Rec, RecGaules=RecGaules, t=5, RandomPlacGaules=RandomPlacGaules, Para.ratio_gaules=Para.ratio_gaules)

saveRDS(attendu, test_path("fixtures/gaules_evolution", "attendu_count_gaules.rds"))
saveRDS(RecGaules, test_path("fixtures/gaules_evolution", "RecGaules_count_gaules.rds"))
saveRDS(Rec, test_path("fixtures/gaules_evolution", "Rec_count_gaules.rds"))
saveRDS(Ratio, test_path("fixtures/gaules_evolution", "Ratio_count_gaules.rds"))


#  ratio_Gaules

Para.ratio_gaules <- readRDS(test_path("fixtures/gaules_evolution", "Para.ratio_gaules.rds"))
RecGaules <- readRDS(test_path("fixtures/gaules_evolution", "RecGaules_count_gaules.rds"))
Rec <- readRDS(test_path("fixtures/gaules_evolution", "Rec_count_gaules.rds"))
RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))
Ratio <- readRDS(test_path("fixtures/gaules_evolution", "Ratio_count_gaules.rds"))
Para.nb_gaules <- readRDS(test_path("fixtures/gaules_evolution", "Para.nb_gaules.rds"))

attendu <- ratio_Gaules(Ratio=Ratio, Rec=Rec, RecGaules=RecGaules, t=5, RandomPlacGaules=RandomPlacGaules, Para.nb_gaules=Para.nb_gaules, Para.ratio_gaules=Para.ratio_gaules)

saveRDS(attendu, test_path("fixtures/gaules_evolution", "attendu_ratio_gaules.rds"))


# fichier pour les pi68XXX et count68XXX

# Ratio Dataframe qui contient les variables Nb_Gaules_Ha et lnNb_Gaules_Ess_Ha par Placette/GrEspece
Ratio <- data.frame(Placette=c(rep(1, 10), rep(2, 10)),
                    GrEspece = rep(c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),2),
                    Nb_Gaules_Ess_Ha = c(10, 1000, 2, 50, 0, 2000, 2, 1, 500, 20)) %>%
  group_by(Placette) %>%
  mutate(lnNb_Gaules_Ess_Ha = log(Nb_Gaules_Ess_Ha+1),
         Nb_Gaules_Ha = sum(Nb_Gaules_Ess_Ha))

# Rec Dataframe qui contient les variables St_Ess_Ha, lnNb_Ess_Ha, trt, t0_aj_, altitude, latitude, dens_tot0, grwd par Placette/GrEspece
Rec <- data.frame(Placette=rep(1, 10),
                  GrEspece = c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),
                  St_Ess_Ha = c(2, 10, 0, 2, 15, 0, 0, 5, 0, 2),
                  trt='TEM',
                  t0_aj_=0,
                  dens_tot0 = 1500,
                  longitude=-74,
                  latitude=50,
                  altitude=200,
                  grwd=200,
                  prec=1000) %>%
  group_by(Placette) %>%
  mutate(lnNb_Ess_Ha = log(St_Ess_Ha+1),
         st_tot0 = sum(St_Ess_Ha))
Rec2 <- Rec %>% mutate(Placette=2, trt='CP', t0_aj_=0.1)
Rec <- bind_rows(Rec, Rec2)

# RecGaules Dataframe qui contient la variable lnNb_Gaules_68_Ess_Ha et lnNb_Gaules_24_Ess_Ha par Placette/GrEspece
RecGaules <- data.frame(Placette=rep(1, 10),
                        GrEspece = c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"),
                        Nb_Gaules_68_Ess_Ha = c(0, 500, 0, 0, 1000, 0, 0, 250, 0, 10),
                        Nb_Gaules_24_Ess_Ha= c(0, 500, 0, 0, 1000, 0, 0, 250, 0, 10)) %>%
  mutate(lnNb_Gaules_68_Ess_Ha = log(Nb_Gaules_68_Ess_Ha+1),
         lnNb_Gaules_24_Ess_Ha = log(Nb_Gaules_24_Ess_Ha+1))
RecGaules2 <- RecGaules %>% mutate(Placette=2)
RecGaules <- bind_rows(RecGaules, RecGaules2)

# Para.68_ERS Paramètre du modèle du nombre de gaules 68 de ERS
# Para.68_HEG Paramètre du modèle du nombre de gaules 68 de HEG
# Para.68_BOJ Paramètre du modèle du nombre de gaules 68 de BOJ
# Para.68_SAB Paramètre du modèle du nombre de gaules 68 de SAB
ParaGaules <- ParametresGaules %>% rename(GrEspece=Ess_groupe) # ParametresGaules est un fichier interne
Para.68_ERS<-ParaOmega(ModuleID = 13,ParaOri=ParaGaules,ParaIter=ParaGaules,Omega=OmegaGaulesFormat,NbIter=1) %>% mutate(Iter=1)
Para.68_HEG<-ParaOmega(ModuleID = 14,ParaOri=ParaGaules,ParaIter=ParaGaules,Omega=OmegaGaulesFormat,NbIter=1) %>% mutate(Iter=1)
Para.68_BOJ<-ParaOmega(ModuleID = 15,ParaOri=ParaGaules,ParaIter=ParaGaules,Omega=OmegaGaulesFormat,NbIter=1) %>% mutate(Iter=1)
Para.68_SAB<-ParaOmega(ModuleID = 16,ParaOri=ParaGaules,ParaIter=ParaGaules,Omega=OmegaGaulesFormat,NbIter=1) %>% mutate(Iter=1)

RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))

attendu_pi68BOJ <- pi68BOJ(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_BOJ=Para.68_BOJ)
attendu_pi68ERS <- pi68ERS(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_ERS=Para.68_ERS)
attendu_pi68HEG <- pi68HEG(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_HEG=Para.68_HEG)
attendu_pi68SAB <- pi68SAB(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_SAB=Para.68_SAB)


attendu_count68BOJ <- count68BOJ(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, t=5, RandomPlacGaules=RandomPlacGaules, Para.68_BOJ=Para.68_BOJ)
attendu_count68ERS <- count68ERS(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_ERS=Para.68_ERS)
attendu_count68HEG <- count68HEG(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_HEG=Para.68_HEG)
attendu_count68SAB <- count68SAB(RecGaules=RecGaules, Ratio=Ratio, Rec=Rec, RandomPlacGaules=RandomPlacGaules, Para.68_SAB=Para.68_SAB)

saveRDS(attendu_pi68BOJ, test_path("fixtures/gaules_evolution", "attendu_pi68BOJ.rds"))
saveRDS(attendu_pi68ERS, test_path("fixtures/gaules_evolution", "attendu_pi68ERS.rds"))
saveRDS(attendu_pi68HEG, test_path("fixtures/gaules_evolution", "attendu_pi68HEG.rds"))
saveRDS(attendu_pi68SAB, test_path("fixtures/gaules_evolution", "attendu_pi68SAB.rds"))

saveRDS(attendu_count68BOJ, test_path("fixtures/gaules_evolution", "attendu_count68BOJ.rds"))
saveRDS(attendu_count68ERS, test_path("fixtures/gaules_evolution", "attendu_count68ERS.rds"))
saveRDS(attendu_count68HEG, test_path("fixtures/gaules_evolution", "attendu_count68HEG.rds"))
saveRDS(attendu_count68SAB, test_path("fixtures/gaules_evolution", "attendu_count68SAB.rds"))


saveRDS(RecGaules, test_path("fixtures/gaules_evolution", "RecGaules_68_gaules.rds"))
saveRDS(Rec, test_path("fixtures/gaules_evolution", "Rec_68_gaules.rds"))
saveRDS(Ratio, test_path("fixtures/gaules_evolution", "Ratio_68_gaules.rds"))

saveRDS(Para.68_BOJ, test_path("fixtures/gaules_evolution", "Para.68_BOJ.rds"))
saveRDS(Para.68_ERS, test_path("fixtures/gaules_evolution", "Para.68_ERS.rds"))
saveRDS(Para.68_HEG, test_path("fixtures/gaules_evolution", "Para.68_HEG.rds"))
saveRDS(Para.68_SAB, test_path("fixtures/gaules_evolution", "Para.68_SAB.rds"))



# gaules_68

RecGaules <- readRDS(test_path("fixtures/gaules_evolution", "RecGaules_68_gaules.rds"))
Rec <- readRDS(test_path("fixtures/gaules_evolution", "Rec_68_gaules.rds"))
Ratio <- readRDS(test_path("fixtures/gaules_evolution", "Ratio_68_gaules.rds"))
RandomPlacGaules <- readRDS(test_path("fixtures/gaules_evolution", "RandomPlacGaules.rds"))

Para.68_BOJ <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_BOJ.rds"))
Para.68_ERS <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_ERS.rds"))
Para.68_HEG <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_HEG.rds"))
Para.68_SAB <- readRDS(test_path("fixtures/gaules_evolution", "Para.68_SAB.rds"))

attendu <- gaules_68(Ratio=Ratio, Rec=Rec, RecGaules=RecGaules, t=5, RandomPlacGaules=RandomPlacGaules,
                     Para.68_ERS=Para.68_ERS, Para.68_HEG=Para.68_HEG, Para.68_BOJ=Para.68_BOJ, Para.68_SAB=Para.68_SAB)

saveRDS(attendu, test_path("fixtures/gaules_evolution", "attendu_gaules_68.rds"))

######################################################################
######################################################################

# fonction SaMARE

# Junior a créé un fichier de resultats de simulation de 6 pas de simulation sur une placette pour une itération
# il a seulement sauvegardé le fichier des effets aléatoire, le fichier d'intrant et le fichier des résultats, mais pas le fichier des paramètres
# Il a mis un set.seed(3)
# il faut donc que je réussise à reproduire la simulation avec ce set.seed
# mais puisque R générère d'avance toute la série de nombres aléatoires à partir du seed,
# si mon code et le code de junior ne traite pas des data avec le même nombre de lignes (inclue ou exclus des lignes avec NA)
# ou ne font pas les choses dans le même ordre, les résultats ne seront jamais les même malgré le seed
# Nos deux codes font les choses dans le même ordre, mais celui de Junior exclus toujours les morts avant de passer une fonction d'un module,
# Tandis que dans le mien, pour économiser les manipulations (un filtre avant pour les exclure et un merge après pour les remettre),
# je les laisse dans le fichier, car ils ne dérangent pas. Leur variables explications sont à NA, donc le résultat de l'équation sera un NA.
# Pour vérifier que mon code arrive aux mêmes résultats de simulation, j'ai mis en commentaires les filtres sur les morts dans le code de Junior,
# et nos deux codes donnent les mêmes résultats.
# Je prends donc pour acquis que mon code est ok, et je vais recréer le fichier de résulats attendu

# avec coupe,  sans gaules et MCH=0
set.seed(NULL)
set.seed(3)

Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>% mutate(AnneeDep=2023) # Annee_Coupe=2021 dans ce fichier
RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
attendu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
               Horizon = 6 ,
               Iteration = 1,
               #seed_value=NULL,
               RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
               Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)
saveRDS(attendu, test_path("fixtures/samare", "result_samare_sans_gaules_test_new.rds"))


# avec coupe, sans gaules et MCH=1
set.seed(NULL)
set.seed(3)

Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>% mutate(AnneeDep=2023)
RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
attendu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                  Horizon = 6 ,
                  Iteration = 1,
                  #seed_value=NULL,
                  RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                  Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                  MCH = 1)
saveRDS(attendu, test_path("fixtures/samare", "result_samare_sans_gaules_test_MCH_new.rds"))




# sans coupe, sans gaules et MCH=0
set.seed(NULL)
set.seed(3)

Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(AnneeDep=2023, ntrt=0, Annee_Coupe=NA) # enlever la coupe
RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
attendu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                  Horizon = 6 ,
                  Iteration = 1,
                  #seed_value=NULL,
                  RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                  Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                  MCH = 0)
saveRDS(attendu, test_path("fixtures/samare", "result_samare_sans_gaules_test_sans_coupe.rds"))
saveRDS(Data_test_for_simul_samare, test_path("fixtures/samare", "Data_test_for_simul_samare_sans_coupe.rds"))


# avec MSCR fourni pour tous les arbres en entrée
set.seed(NULL)
set.seed(3)

Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(AnneeDep=2023, MSCR='C')
RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
attendu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                  Horizon = 6 ,
                  Iteration = 1,
                  #seed_value=NULL,
                  RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                  Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)
saveRDS(attendu, test_path("fixtures/samare", "result_samare_avec_mscr_tous.rds"))
saveRDS(Data_test_for_simul_samare, test_path("fixtures/samare", "Data_test_for_simul_samare_avec_mscr_tous.rds"))


# avec MSCR fourni pour certains arbres en entrée
set.seed(NULL)
set.seed(3)

Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(AnneeDep=2023, MSCR=ifelse(NoArbre==1, 'C', NA))
RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
attendu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                  Horizon = 6 ,
                  Iteration = 1,
                  #seed_value=NULL,
                  RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                  Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)
saveRDS(attendu, test_path("fixtures/samare", "result_samare_avec_mscr_certain.rds"))
saveRDS(Data_test_for_simul_samare, test_path("fixtures/samare", "Data_test_for_simul_samare_avec_mscr_certain.rds"))


# avec qualité
set.seed(NULL)
set.seed(3)

Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(AnneeDep=2023, MSCR=NA, DHPcm=28, DHPcm = ifelse(NoArbre==1, 22.9, DHPcm),
         ABCD = rep(c('A','B','C','D','A','B'),3),
         ABCD = ifelse(NoArbre==1, NA, ABCD))

RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
attendu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                  Horizon = 6 ,
                  Iteration = 1,
                  #seed_value=NULL,
                  RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                  Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)
# j'ai vérifié avec la version de junior et ça donn e la même chose
saveRDS(attendu, test_path("fixtures/samare", "result_samare_avec_qualite.rds"))
saveRDS(Data_test_for_simul_samare, test_path("fixtures/samare", "Data_test_for_simul_samare_avec_qualite.rds"))



# avec des arbres martelés, avec ntrt=0
set.seed(NULL)
set.seed(3)

# les arbres martelés affectent les variable trt, ntrt
Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(AnneeDep=2023, Etat = ifelse(DHPcm>29, 11, Etat), ntrt=0, Annee_Coupe=NA)

RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
attendu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                  Horizon = 1 ,
                  Iteration = 1,
                  #seed_value=NULL,
                  RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                  Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)

# j'ai vérifié avec la version de junior et ça donne la même chose
saveRDS(attendu, test_path("fixtures/samare", "result_samare_avec_martele_sansCP.rds"))
saveRDS(Data_test_for_simul_samare, test_path("fixtures/samare", "Data_test_for_simul_samare_avec_martele_sansCP.rds"))


# avec des arbres martelés, avec ntrt=1
set.seed(NULL)
set.seed(3)

# les arbres martelés affectent les variable trt, ntrt
Data_test_for_simul_samare <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(AnneeDep=2023, Etat = ifelse(DHPcm>29, 11, Etat), ntrt=1, Annee_Coupe=2010)

RandomTest <- readRDS(test_path("fixtures/samare", "RandomTest.rds"))
attendu <- SaMARE(Random =RandomTest, Data = Data_test_for_simul_samare, Gaules =NA,
                  Horizon = 1 ,
                  Iteration = 1,
                  #seed_value=NULL,
                  RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                  Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)

# j'ai vérifié avec la version de junior et ça donn e la même chose
saveRDS(attendu, test_path("fixtures/samare", "result_samare_avec_martele_avecCP.rds"))
saveRDS(Data_test_for_simul_samare, test_path("fixtures/samare", "Data_test_for_simul_samare_avec_martele_avecCP.rds"))


# Avec plusieurs placettes
# un cas avec plusieurs placettes, certaines avec des arbres martelés, certaines avec qualite, certaines avec mscr
# différentes essences, différents dhp


# avec CP, avec marteles, sans mscr, sans qualite
data1 <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(Placette=1, AnneeDep=2023, Etat = ifelse(DHPcm>29, 11, Etat), ntrt=1, Annee_Coupe=2010)

# sans CP, avec marteles, sans mscr, sans qualite
data2 <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(Placette=2, AnneeDep=2015, Etat = ifelse(DHPcm>29, 11, Etat), ntrt=0, Annee_Coupe=NA)

# sans CP, sans MSCR, sans martele, avec qualite
data3 <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(Placette=3, AnneeDep=2022, MSCR=NA, DHPcm=28, DHPcm = ifelse(NoArbre==1, 22.9, DHPcm),
         ABCD = rep(c('A','B','C','D','A','B'),3),
         ABCD = ifelse(NoArbre==1, NA, ABCD),
         ntrt=0, Annee_Coupe=NA)

# avec CP, avec MSCR, sans martele, sans qualite
data4 <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(Placette=4, AnneeDep=2012, MSCR='C', Annee_Coupe=2007)

# avec CP, sans MSCR, sans martele, sans qualite, d'autres essences
data5 <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare.rds")) %>%
  mutate(Placette=5, AnneeDep=2020, Espece = rep(c('SAB','BOJ','ERS','BOP','HEG','THO','PRU','EPN','ERR'),2)) %>%
  select(-GrEspece)
ListeSp <- merge(MatchSpeciesGroups, SpeciesGroups, by="SpeciesGroupID") %>%
  merge(Species, by="SpeciesID") %>%
  rename(Espece=SpeciesName,GrEspece=SpeciesGroupName) %>%
  select(GrEspece,Espece)
data5 <- data5 %>% left_join(ListeSp, by="Espece") %>% mutate(Annee_Coupe=2020)

# avec CP, sans MSCR, sans martele, sans qualite, d'autres essences et d'autres DHP
data6 <- data5 %>%
  mutate(Placette=6, DHPcm = rep(c(9.5, 10.2, 12.4, 11.2, 10.1, 13.0),3))

data_tous <- bind_rows(data1, data2, data3, data4, data5, data6) %>% mutate(Annee_Inventaire = AnneeDep)

set.seed(NULL)
set.seed(3)
#Random_tous <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=data_tous, NbIter=1, NbPeriodes=6)
Random_tous <- readRDS(test_path("fixtures/samare", "Random_tous.rds"))
set.seed(NULL)
set.seed(3)
attendu <- SaMARE(Random =Random_tous, Data = data_tous, Gaules =NA,
                  Horizon = 4 ,
                  Iteration = 1,
                  #seed_value=NULL,
                  RecruesGaules =0,CovParms=MatchModuleCovparms,CovParmsGaules=CovparmGaules,
                  Para=MatchModuleParameters,ParaGaules=ParametresGaules,Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat, MCH = 0)

#saveRDS(Random_tous, test_path("fixtures/samare", "Random_tous.rds"))
#saveRDS(attendu, test_path("fixtures/samare", "result_samare_plusieurs_placettes.rds"))
saveRDS(attendu, test_path("fixtures/samare", "result_samare_plusieurs_placettes_v2.rds"))
saveRDS(data_tous, test_path("fixtures/samare", "Data_test_for_simul_samare_plusieurs_placettes.rds"))




######################################################################
######################################################################

# fonction AttribMSCR

# j'ai fait rouler la version de la fonction SaMARE de Junior (en corrigeant le traitement du groupe AUT)
attendu <- read_delim("tests/testthat/fixtures/mscr_attrib/resultat_attendu_version_junior.csv", delim=';')
saveRDS(attendu, test_path("fixtures/mscr_attrib", "resultat_attendu_version_junior.rds"))

# fichier d'intrant pour mon test
data_test <- attendu %>% select(-MSCR)
saveRDS(data_test, test_path("fixtures/mscr_attrib", "data_test.rds"))

# fichier des paramètres
set.seed(NULL)
set.seed(3)
Para <- MatchModuleParameters %>%
  mutate(Effect = str_to_lower(Effect)) %>%
  rename(GrEspece=Ess_groupe) %>%
  select(-VegPotID,-Veg_Pot)
saveRDS(Para, test_path("fixtures/mscr_attrib", "param.rds"))
obtenu = AttribMSCR(Data=data_test, Para.ConvVigMSCR=Para, seed_value=3)
saveRDS(obtenu, test_path("fixtures/mscr_attrib", "resultat_attendu_prob.rds"))





######################################################################
######################################################################

# fonction du calcul du nombre de recrues à partir des gaules

# fichier des info placette et sur les arbres marchands
Rec_test_gaules <- readRDS(test_path("fixtures/recrues_avec_gaules", "Rec_test_gaules.rds")) %>% mutate(st_tot0=21.5900825754271, Placette='TEM23APC5000')
Rec_test_gaules2 <- Rec_test_gaules %>% mutate(Placette='TEM23APC5001')
Rec_test_gaules <- bind_rows(Rec_test_gaules, Rec_test_gaules2)

# fichier des info sur le nombre de gaules
RecGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RecGaules_test.rds")) %>% mutate(Placette='TEM23APC5000')
RecGaules_test2 <- RecGaules_test %>% mutate(Placette='TEM23APC5001')
RecGaules_test <- bind_rows(RecGaules_test, RecGaules_test2)

# fichier des effets aléatoires de placettes
RandomPlacGaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "RandomPlacGaules_test.rds"))
RandomPlacGaules_test2 <- RandomPlacGaules_test %>% mutate(Placette='TEM23APC5001')
RandomPlacGaules_test <- bind_rows(RandomPlacGaules_test, RandomPlacGaules_test2)

# fichier des paramètres
Para.rec_gaules_test <- readRDS(test_path("fixtures/recrues_avec_gaules", "Para.rec_gaules_test.rds"))




set.seed(NULL)
attendu <- rec_n_Gaules(Rec=Rec_test_gaules, RecGaules=RecGaules_test, t=5, CovParmsGaules=CovparmGaules, RandomPlacGaules=RandomPlacGaules_test, Para.rec_gaules=Para.rec_gaules_test, seed_value=3)
set.seed(NULL)
saveRDS(attendu, test_path("fixtures/recrues_avec_gaules", "resultat_attendu_nb_recrues_avec_gaules.rds"))

##################################################################


# Pour test de samare avec gaules et plusieurs placettes

# fichier de gaules
Gaules_test <- readRDS(test_path("fixtures/samare/avec_gaules", "Gaules_test.rds"))
Gaules_tous <- NULL
for (i in 1:6){
  temp <- Gaules_test %>% mutate(Placette=i)
  Gaules_tous <- bind_rows(Gaules_tous, temp)
}

# fichier des affets aléatoires pour le module de gaules
RandPlacStepGaules_test <- readRDS(test_path("fixtures/samare/avec_gaules", "RandPlacStepGaules_test.rds"))
RandPlacStepGaules_tous <- NULL
for (i in 1:6){
  temp <- RandPlacStepGaules_test %>% mutate(Placette=i)
  RandPlacStepGaules_tous <- bind_rows(RandPlacStepGaules_tous, temp)
}

# fichier de données
data_tous <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare_plusieurs_placettes.rds")) %>% mutate(Annee_Inventaire = AnneeDep)
RandomTest <- readRDS(test_path("fixtures/samare", "Random_tous.rds"))



data_tous <- readRDS(test_path("fixtures/samare", "Data_test_for_simul_samare_plusieurs_placettes.rds")) %>% mutate(Annee_Inventaire = AnneeDep)
RandomTest <- readRDS(test_path("fixtures/samare", "Random_tous.rds"))
RandPlacStepGaules_tous <- readRDS(test_path("fixtures/samare/avec_gaules", "RandPlacStepGaules_tous.rds")) # variance des effets aléatoires

set.seed(NULL)
set.seed(3)
attendu <- SaMARE(Random =RandomTest, Data = data_tous, Gaules = Gaules_tous, RandomGaules=RandPlacStepGaules_tous,
                 Horizon = 4 ,
                 Iteration = 1,
                 #seed_value=NULL,
                 RecruesGaules = 1, CovParms=MatchModuleCovparms, CovParmsGaules=CovparmGaules,
                 Para=MatchModuleParameters, ParaGaules=ParametresGaules, Omega=MatchModuleOmega, OmegaGaules=OmegaGaulesFormat,
                 MCH = 0)
set.seed(NULL)
saveRDS(attendu, test_path("fixtures/samare/avec_gaules", "result_samare_avec_gaules_plusieurs_placettes.rds"))
saveRDS(RandPlacStepGaules_tous, test_path("fixtures/samare/avec_gaules", "RandPlacStepGaules_tous.rds"))
saveRDS(Gaules_tous, test_path("fixtures/samare/avec_gaules", "Gaules_tous.rds"))


##########################################################################

# cubage_arbres sans gaules et avec residuel 0/1 et avec ess NC


# j'ai fait rouler la première partie de Simulateur Samare avec:
ess_nc <- data.frame( Placette='Test', NoArbre=seq(1:10),
                      Espece=c("AME","AUR","ERE","ERG","ERP","MAS","PRP","SAL","SOA","SOD"),
                      Vigueur=4, Nombre=1, DHPcm=12, Etat=10, Annee_Coupe=NA,
                      Latitude=48, Longitude=-77, Altitude=200, Ptot=1000,
                      Tmoy=0, Reg_Eco='3a', Type_Eco='FE32', ntrt=0,
                      Sup_PE=0.04, Pente=10, GrwDays=100, MSCR=NA, ABCD=NA)
Data_test <- bind_rows(Test400m2, ess_nc)
NbIter=3; Horizon=3; RecruesGaules=0; Data = Data_test; Gaules = NULL; MCH = 0;
#saveRDS(Simul, test_path("fixtures/cubage_arbres", "Simul.rds"))  # Simul est généré par la 1ere partie de SimulSaMARE
#saveRDS(Data, test_path("fixtures/cubage_arbres", "Data_test.rds")) # Data est généré par la 1ere partie de SimulSaMARE
Simul <- readRDS(test_path("fixtures/cubage_arbres", "Simul.rds"))
Data_test <- readRDS(test_path("fixtures/cubage_arbres", "Data_test.rds"))
attendu <- cubage_arbres(data=Data_test, simul_data=Simul, NbIter=3, Horizon=3)
saveRDS(attendu, test_path("fixtures/cubage_arbres", "attendu_sans_gaules.rds"))



# cubage_arbres avec gaules

# j'ai fait rouler la première partie de Simulateur Samare avec:
NbIter=3; Horizon=3; RecruesGaules=1; Data = Test2500m2; Gaules = GaulesTest2500m2; MCH = 0;
#saveRDS(Simul, test_path("fixtures/cubage_arbres", "Simul_avec_gaules.rds")) # Simul est généré par la 1ere partie de SimulSaMARE
#saveRDS(Data, test_path("fixtures/cubage_arbres", "Data_test_avec_gaules.rds")) # Data est généré par la 1ere partie de SimulSaMARE
Simul <- readRDS(test_path("fixtures/cubage_arbres", "Simul_avec_gaules.rds"))
Data_test <- readRDS(test_path("fixtures/cubage_arbres", "Data_test_avec_gaules.rds"))
attendu <- cubage_arbres(data=Data_test, simul_data=Simul, NbIter=3, Horizon=3)
saveRDS(attendu, test_path("fixtures/cubage_arbres", "attendu_avec_gaules.rds"))


##########################################################################

# Sommaire_Classes_DHP

# avec volume
simul <- SimulSaMARE(NbIter = 2, Horizon = 2, Data = Test2500m2, cubage = T)
attendu <- Sommaire_Classes_DHP(simul)
saveRDS(attendu, test_path("fixtures/sortie_sommaire_cl_dhp", "attendu_sommaire_DHP.rds"))
saveRDS(simul, test_path("fixtures/sortie_sommaire_cl_dhp", "sortie_simul.rds"))




