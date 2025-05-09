#' Structurer un dataframe de sortie pour les simulations SaMARE
#'
#' La fonction \code{SortieDendroIterSamare} structure un dataframe de sortie pour lequel
#' on rapporte, pour chaque placette, année, groupe d'espèces et itération, le diamètre
#' quadratique moyen, la surface terrière, le volume et la hauteur dominante à l'ha.
#' Cette fonction prend en paramètre un dataframe produit par la fonction \code{SimulSaMARE}
#'
#' @param SimulHtVol Un dataframe produit par la fonction \code{SimulSaMARE}.
#' @param simplifier Un booléen indiquant si les résultats doivent être simplifiés pour ne garder
#' que la première et la dernière année de la simulation. Par défaut, \code{FALSE}.
#'
#' @return Un dataframe contenant, pour chaque placette, groupe d'espèces, année et itération,
#' la surface terrière (ST_m2ha), le volume marchand brut (Vol_m3ha), le diamètre moyen quadratique (DQM_cm) et la hauteur dominante (Hdom_m).
#' @export

SortieDendroIterSamare <- function(SimulHtVol, simplifier=FALSE){

  select=dplyr::select

  # indicateur si le volume présent
  vol_pres <- SimulHtVol %>% lazy_dt() %>% filter(!is.na(Vol_dm3)) %>% as.data.frame()

  # préparer les variables
  data_prep <- SimulHtVol %>% lazy_dt() %>%
    mutate(Etat = ifelse(Etat=="mort", "mort", "vivant"), # mettre recrue avec vivant
           Vol = ifelse(is.na(Vol_dm3), 0, as.numeric(Vol_dm3))) %>%
    as.data.frame()

  # data des vivants
  vivant <- data_prep %>% lazy_dt() %>% filter(Etat=="vivant") %>% as.data.frame()

  # Sommaire par GrEspece
  by_vars <- c('Placette', 'Iter', 'Annee', 'Temps', 'GrEspece', 'Etat', 'Residuel')
  dendro_sp <- calcul_var_dendro(vivant, DHPcm, Vol, Nombre, Sup_PE, by_vars)
  hd_sp <- calcul_hdom(vivant, Hautm, Nombre, Sup_PE, by_vars)

  # ajouter hd_sp aux dendro_sp
  dendro_sp <- dendro_sp %>% lazy_dt() %>%
    left_join(hd_sp, by = c("Placette", "Iter", "Annee", "Temps", "GrEspece", "Etat", "Residuel")) %>%
    as.data.frame()

  # Sommaire toutes essences et par etat
  by_vars <- c('Placette', 'Iter', 'Annee', 'Temps', 'Etat', 'Residuel')
  dendro_tot <- calcul_var_dendro(data_prep, DHPcm, Vol, Nombre, Sup_PE, by_vars)
  dendro_tot <- dendro_tot %>% lazy_dt() %>%
    mutate(DQM_cm = ifelse(Etat=="mort", NA, DQM_cm),
           ST_m2ha = ifelse(Etat=="mort", NA, ST_m2ha),
           Vol_m3ha = ifelse(Etat=="mort", NA, Vol_m3ha),
           GrEspece="TOT") %>%
    as.data.frame()
  hd_tot <- calcul_hdom(vivant, Hautm, Nombre, Sup_PE, by_vars)

   # ajouter hd_tot aux dendro_tot
  dendro_tot <- dendro_tot %>% lazy_dt() %>%
    left_join(hd_tot, by = c("Placette", "Iter", "Annee", "Temps", "Etat", "Residuel")) %>%
    as.data.frame()

  # mettre total et par espece ensemble
  setDT(dendro_tot); setDT(dendro_sp)
  dendro_tous <- rbind(dendro_sp, dendro_tot)
  setorder(dendro_tous, Iter, Placette, Annee, Temps, Residuel, GrEspece, -Etat)

  # si le volume était absent remettre à NA (car c'est maintenant des 0)
  if (nrow(vol_pres)==0){
    dendro_tous <- dendro_tous %>%
      lazy_dt() %>%
      mutate(Vol_m3ha = NA) %>%
      as.data.frame()
  }


if(simplifier == TRUE){

  MinAnnee = min(SimulHtVol$Temps) # il faut utiliser Temps, car Annee peut varier d'une placette à l'autre
  MaxAnnee = max(SimulHtVol$Temps)

  dendro_tous_min <- dendro_tous %>% lazy_dt() %>% filter(Temps==MinAnnee) %>% as.data.frame() # il faut utiliser Temps, car Annee peut varier d'une placette à l'autre
  dendro_tous_max <- dendro_tous %>% lazy_dt() %>% filter(Temps==MaxAnnee) %>% as.data.frame()
  dendro_tous <- rbind(dendro_tous_min, dendro_tous_max)

}

  return (dendro_tous)

}
