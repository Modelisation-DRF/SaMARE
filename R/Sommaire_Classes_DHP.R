#' Fonction qui compile une sortie de SimulSAMARE par classe de DHP, par placette, groupe d'espèces, vigueur,
#' et fait la moyenne des itérations. La sortie présente donc un sommaire de la table de peuplement simulée
#' pour chacune des placettes.
#'
#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#'                    iterations du simulateur SaMARE. Typiquement un résultat retourné
#'                    par la fonction "SimulSaMARE".
#' @param simplifier Un booléen indiquant si les résultats doivent être simplifiés pour ne garder
#'                  que la première et la dernière année de la simulation. Par défaut, \code{FALSE}.
#' @return Retourne un dataframe contenant le sommaire des itérations par placette, année
#'          groupe d'espèce, vigueur, classe de DHP.
#' @export

Sommaire_Classes_DHP <- function(SimulHtVol, simplifier=FALSE){
  select=dplyr::select

    NbIter <- length(unique(SimulHtVol$Iter))

    # indicateur si le volume présent
    vol_pres <- SimulHtVol %>% lazy_dt() %>% filter(!is.na(Vol_dm3)) %>% as.data.frame()

    # préparer le data
    prep_data <- SimulHtVol %>% lazy_dt() %>%
      filter(Etat!="mort") %>%
      mutate(Vol = ifelse(is.na(Vol_dm3), 0, as.numeric(Vol_dm3)),
             DHP_cl = round(DHPcm/2)*2)

    # sommaire par iter de chaque placette/annee par essence/vig/cl-dhp
    by_vars <-  c('Placette', 'Annee', 'Temps', 'GrEspece', 'DHP_cl', 'Vigueur', 'Iter')
    dendro_cl_dhp <- calcul_var_dendro(prep_data, DHPcm, Vol, Nombre, Sup_PE, by_vars) %>% select(-DQM_cm, -Ti_ha)

    # moyenne des itérations
    dendro_cl_dhp <- dendro_cl_dhp %>% lazy_dt() %>%
      group_by(Placette, Annee, Temps, GrEspece, DHP_cl, Vigueur) %>%
      summarise(ST_m2ha = sum(ST_m2ha)/NbIter,  # on n'utilise pas mean car il peut y avoir des essences absente pour certaines iter
                Vol_m3ha = sum(Vol_m3ha)/NbIter,
                .groups="drop") %>%
      mutate(Ti_ha = round(ST_m2ha/((DHP_cl/200)^2*pi))) %>%
      arrange(Placette, Annee, GrEspece, DHP_cl, Vigueur) %>%
      as.data.frame()

    # sommaire toutes essences à partir de la moyenne des itérations
    dendro_cl_dhp_tot <- dendro_cl_dhp %>%
      lazy_dt() %>%
      group_by(Placette, Annee, Temps, DHP_cl, Vigueur) %>%
      summarise(Ti_ha = sum(Ti_ha),
                ST_m2ha = sum(ST_m2ha),
                Vol_m3ha = sum(Vol_m3ha),
                .groups="drop") %>%
      mutate(GrEspece="TOT") %>%
      as.data.frame() %>%
      rbind(dendro_cl_dhp) %>%
      arrange(Placette, Annee, Temps, GrEspece, DHP_cl, Vigueur) %>%
      relocate(Placette, Annee, Temps, GrEspece, DHP_cl, Vigueur, Ti_ha, ST_m2ha, Vol_m3ha) %>%
      as.data.frame()

    # si le volume était absent remettre à NA (car c'est maintenant des 0)
    if (nrow(vol_pres)==0){
      dendro_cl_dhp_tot <- dendro_cl_dhp_tot %>%
        lazy_dt() %>%
        mutate(Vol_m3ha = NA) %>%
        as.data.frame()
    }
    #SommaireClassesDHP <- SommaireClassesDHP[,c(1,2,8,3,4,5,6,7)]

if(simplifier == TRUE){

  MinAnnee = min(SimulHtVol$Temps) # il faut utiliser Temps, car Annee peut être différente pour chaque placette
  MaxAnnee = max(SimulHtVol$Temps)

  SommaireClassesDHP_simp_min <- dendro_cl_dhp_tot %>% lazy_dt() %>% filter(Temps==MinAnnee) %>% as.data.frame()
  SommaireClassesDHP_simp_max <- dendro_cl_dhp_tot %>% lazy_dt() %>% filter(Temps==MaxAnnee) %>% as.data.frame()
  dendro_cl_dhp_tot <- rbind(SommaireClassesDHP_simp_min, SommaireClassesDHP_simp_max)

}

  return(dendro_cl_dhp_tot)
}
