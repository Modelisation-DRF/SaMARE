#' Fonction qui structure un dataframe créé par SimulSAMARE pour lequel on rapporte pour chaque
#' placette, annee et groupe d'espèce la moyenne des itérations (et l'écart-type) du diamètre quadratique moyen,
#' de la surface terrière, du volume et de la hauteur dominante, pour les vivants.
#' La fonction calcul aussi les accroissements survivant, mort, recrue, brut et net.

#' @param SimulHtVol un dataframe créé par  \code{SimulSAMARE}.
#' @param simplifier Un booléen indiquant si les résultats doivent être simplifiés pour ne garder
#'                    que la première et la dernière année de la simulation. Par défaut, \code{FALSE}.
#' @return  Retourne un dataframe contenant la moyenne des itérations par placette, groupe d'espèces, année
#'          de la surface terrière, du volume marchand brut, du diamètre moyen quadratique et de la hauteur dominante.
#'          Les 10 groupes d'especes de SaMARE seront présentes dans chaque placette/annee
#' @export

SortieDendroSamare <- function(SimulHtVol, simplifier=FALSE){
  select=dplyr::select

  NbIter <- length(unique(SimulHtVol$Iter))
  Horizon <- length(unique(SimulHtVol$Temps))-1

  # indicateur si le volume présent
  vol_pres <- SimulHtVol %>% lazy_dt() %>% filter(!is.na(Vol_dm3)) %>% as.data.frame()

  # mettre les 10 essences dans toutes les iter/placette/annee
  ListeGrSp <- data.frame("GrEspece"=Especes)  # Especes est un objet interne

  # Liste des essence par placette au temps 0
  ListeSpIni <- SimulHtVol %>% lazy_dt() %>%
    group_by(Placette, Iter, Annee, Temps, Etat, Residuel, GrEspece) %>%
    summarise(.groups="drop") %>%
    filter(Temps==0) %>%
    as.data.frame()

  # ajouter les 10 essences aux placettes pour les temps>0
  ListeMerge <- SimulHtVol %>% lazy_dt() %>%
    group_by(Placette, Iter, Annee, Temps, Etat, Residuel) %>%
    summarise(.groups="drop") %>%
    filter(Temps>0) %>%
    merge(ListeGrSp) %>%
    rbind(ListeSpIni) %>%
    filter(Etat=="vivant") %>%
    arrange(Iter,Placette,Annee,Temps,Etat,Residuel,GrEspece)

  # préparer les données
  # prep_data_vivant <- SimulHtVol %>% lazy_dt() %>%
  #   mutate(Etat = ifelse(Etat=="mort","mort","vivant")) %>%
  #   filter(Etat=="vivant") %>%
  #   as.data.frame()

  # sommaire par iter/placette/annee/essence
  dendro_sp <- SortieDendroIterSamare(SimulHtVol)

  # ajouter les essences manquantes et les mettres à 0 et calculer la moyenne des itérations
  dendro_sp2 <- dendro_sp %>% lazy_dt %>%
    full_join(ListeMerge, by = c('Placette', 'Iter', 'Annee', 'Temps', 'GrEspece', 'Etat', 'Residuel')) %>%
    mutate(ST_m2ha = replace_na(ST_m2ha, 0),
           Vol_m3ha = replace_na(Vol_m3ha, 0),
           Ti_ha = replace_na(Ti_ha, 0)) %>%
    # calculer la moyenne des itérations
    group_by(Placette, Annee, Temps, Residuel, GrEspece, Etat) %>%
    summarise(EcartType_Ti_HA = sd(Ti_ha),
              EcartType_ST_HA = sd(ST_m2ha),
              EcartType_Vol_HA = sd(Vol_m3ha),
              EcartType_HDom = sd(HDom_m, na.rm=TRUE),
              EcartType_DQM = sd(DQM_cm, na.rm=TRUE),

              Ti_ha = mean(Ti_ha),
              ST_m2ha = mean(ST_m2ha),
              Vol_m3ha = mean(Vol_m3ha),
              HDom_m = mean(HDom_m, na.rm=TRUE),
              ,.groups="drop") %>%
    mutate(DQM_cm = dqm(ST_m2ha, Ti_ha)) %>%
    arrange(Placette, Annee, Temps, Residuel, GrEspece, desc(Etat)) %>%
    relocate(Placette, Annee, Temps, Residuel, GrEspece, Etat, Ti_ha, ST_m2ha, DQM_cm, Vol_m3ha, HDom_m,
             EcartType_Ti_HA, EcartType_ST_HA, EcartType_DQM, EcartType_Vol_HA, EcartType_HDom) %>%
    as.data.frame()


# RECRUTEMENT
# accroissement annuel courant des recrues par iter et moyenne des iter
Recrue <- SimulHtVol %>% lazy_dt() %>% filter(Etat=="recrue") %>% as.data.frame()
# calcul de la st des recrues par iter/placette/annee/essence
dendro_recrue <- SortieDendroIterSamare(Recrue)
# calcul de la moyenne des iter et de l'AAC
Recrutement <- dendro_recrue %>% lazy_dt() %>%
  group_by(Placette, Annee, Temps, GrEspece, Residuel) %>%
  summarise(AAC_Recrue_m2haan = sum(ST_m2ha)/(t*NbIter),  # t= 5 dans samare
            .groups="drop") %>%
  as.data.frame()


# MORTALITÉ
# accroissement annuel courant des morts par iter et moyenne des iter
Morts <- SimulHtVol %>% lazy_dt() %>%
  group_by(Placette, Iter, ArbreID, GrEspece, Residuel) %>%
  mutate(DHP0 = lag(DHPcm)) %>% # aller chercher le dhp du pas de temps précédent pour chaque arbre
  filter(Etat=="mort") %>%
  mutate(Etat='vivant') %>% # pour que la fct SortieDendroIterSamare marche
  select(-DHPcm) %>% rename(DHPcm=DHP0) %>%
  as.data.frame()
# calcul de la st des mort par iter/placette/annee/essence
dendro_mort <- SortieDendroIterSamare(Morts)
# calcul de la moyenne des iter et de l'AAC
Mortalite <- dendro_mort %>% lazy_dt() %>%
  group_by(Placette, Annee, Temps, GrEspece, Residuel) %>%
  summarise(AAC_Mort_m2haan = -sum(ST_m2ha)/(t*NbIter),  # t= 5 dans samare, on met un négatif, car la mortalité se soustrait
            .groups="drop") %>%
  as.data.frame()


# SURVIVANTS
# accroissement annuel courant des survivant par iter et moyenne des iter
survivant <- SimulHtVol %>% lazy_dt() %>%
  group_by(Placette, Iter, ArbreID, GrEspece, Residuel) %>%
  mutate(DHP0 = lag(DHPcm)) %>% # aller chercher le dhp du pas de temps précédent pour chaque arbre
  filter(Etat=="vivant") %>%
  mutate(Stm2ha0 = stm2ha(DHP0, Nombre, Sup_PE),
         Stm2ha = stm2ha(DHPcm, Nombre, Sup_PE)) %>%
  mutate(AccSt = Stm2ha0-Stm2ha) %>%
  as.data.frame()
# calcul de la moyenne des iter et de l'AAM par GrEspece
Accroissementsp <- survivant %>% lazy_dt() %>%
  group_by(Placette, Iter, Annee, Temps, GrEspece, Residuel) %>%
  summarise(AACAccrM2Ha = sum(AccSt), .groups="drop") %>%
  group_by(Placette, Annee, Temps, GrEspece, Residuel) %>%
  summarise(AAC_Survie_m2haan = -sum(AACAccrM2Ha)/(t*NbIter),  # on met un négatifs car on a fait la diff des ST à l'envers
            .groups="drop") %>%
  as.data.frame()
# calcul de la moyenne des iter et de l'AAC toute essence et ajouter par espece
Accroissement <- Accroissementsp %>% lazy_dt() %>%
  group_by(Placette, Annee, Temps, Residuel) %>%
  summarise(AAC_Survie_m2haan = sum(AAC_Survie_m2haan), .groups="drop") %>%
  mutate(GrEspece = "TOT") %>%
  as.data.frame() %>%
  rbind(Accroissementsp) %>%
  arrange(Placette, Annee, Temps, Residuel, GrEspece)


# mettre tous les data ensemble
DendroSamare <- dendro_sp2 %>% lazy_dt() %>%
  left_join(Accroissement, by=c("Placette", "Annee", "Temps", "Residuel", "GrEspece")) %>%
  left_join(Mortalite, by=c("Placette", "Annee", "Temps", "Residuel", "GrEspece")) %>%
  left_join(Recrutement, by=c("Placette", "Annee", "Temps", "Residuel", "GrEspece")) %>%
  filter (Etat=="vivant") %>%
  mutate(AAC_Survie_m2haan = replace_na(AAC_Survie_m2haan, 0),
         AAC_Mort_m2haan = replace_na(AAC_Mort_m2haan, 0),
         AAC_Recrue_m2haan = replace_na(AAC_Recrue_m2haan, 0),
         AAC_Brut_m2haan = AAC_Survie_m2haan + AAC_Recrue_m2haan, # brut = survivant + recrue
         AAC_Net_m2haan = AAC_Brut_m2haan + AAC_Mort_m2haan) %>%    # net = brut + mort
  select(-Etat) %>%
  as.data.frame()


# si le volume était absent remettre à NA (car c'est maintenant des 0)
if (nrow(vol_pres)==0){
  DendroSamare <- DendroSamare %>% lazy_dt() %>%
    mutate(Vol_m3ha = NA,
           EcartType_Vol_HA = NA) %>%
    as.data.frame()
}


if(simplifier == TRUE){

  MinAnnee = min(SimulHtVol$Temps) # Il faut utiliser Temps, car Annee peut être diufférente par placette
  MaxAnnee = max(SimulHtVol$Temps)

  # sélectionner la temps 0
  DendroIterSamare_simp_min <- DendroSamare %>% filter(Temps == MinAnnee)
  # sélectioner le dernier temps pour les variables dendro
  DendroIterSamare_simp_maxa <- DendroSamare %>% filter(Temps == MaxAnnee) %>% select(-contains('AAC'))
  # faire la moyenne des AAC des temps > 0
  DendroIterSamare_simp_maxb <- DendroSamare %>% filter(Temps != MinAnnee) %>%
    group_by(Placette, Residuel, GrEspece) %>%
    summarise(AAC_Survie_m2haan = sum(AAC_Survie_m2haan)/Horizon,
              AAC_Mort_m2haan = sum(AAC_Mort_m2haan)/Horizon,
              AAC_Recrue_m2haan = sum(AAC_Recrue_m2haan)/Horizon,
              AAC_Brut_m2haan = sum(AAC_Brut_m2haan)/Horizon,
              AAC_Net_m2haan = sum(AAC_Net_m2haan)/Horizon,
              .groups="drop") %>%
    mutate(Temps = MaxAnnee)



  DendroIterSamare_simp_max <- DendroIterSamare_simp_maxa %>% left_join(DendroIterSamare_simp_maxb, by = c('Placette', 'Temps', 'Residuel', 'GrEspece'))

  DendroSamare <- rbind(DendroIterSamare_simp_min, DendroIterSamare_simp_max)

}

  return (DendroSamare)

}


