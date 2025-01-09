#' Fonction qui structure un dataframe de sortie dont chaque ligne correspond
#' à chacun des arbres par placette, par itération et par année.
#'
#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#'                    iterations du simulateur SaMARE. Typiquement un résultat retourné
#'                    par la fonction "SimulSaMARE".
#' @param simplifier Un booléen indiquant si les résultats doivent être simplifiés pour ne garder
#'                  que la première et la dernière année de la simulation. Par défaut, \code{FALSE}.
#' @return  Retourne un dataframe contenant l'ensemble des arbres pour chacune des
#'          placettes, années, itérations.
#' @export


SortieArbreSamare <- function(SimulHtVol ,simplifier=FALSE){

  select=dplyr::select

  MinAnnee = min(SimulHtVol$Annee)
  MaxAnnee = max(SimulHtVol$Annee)

  ArbreSamare <- SimulHtVol %>%
    mutate (Stm2=pi*(DHPcm/200)^2,

            Vigueur = case_when(
              vigu0 == "ViG" & prod0 == "sciage" ~ 1,
              vigu0 == "ViG" & prod0 == "pate" ~ 2,
              vigu0 == "NONVIG" & prod0 == "sciage" & DHPcm>=23.1 ~ 3,
              vigu0 == "NONVIG" & prod0 == "sciage" & DHPcm<23.1 ~ 4,
              vigu0 == "NONVIG" & prod0 == "pate" ~ 4,
              vigu0 == "ViG" & prod0 == "resineux" ~ 5,
              vigu0 == "NONVIG" & prod0 == "resineux" ~ 6,
              TRUE ~ NA_integer_
            )) %>%
    select(Placette, Annee, Residuel, ArbreID, NoArbre, Nombre, GrEspece, Espece,
              Etat, DHPcm, Iter, hauteur_pred, vol_dm3, Stm2,Vigueur, MSCR, ABCD) %>%
    rename(PlacetteID=Placette, origTreeID=NoArbre, ST_m2=Stm2, Vol_dm3=vol_dm3, Hautm=hauteur_pred) %>%
    relocate(PlacetteID,Annee,Iter,Residuel,ArbreID,origTreeID, Espece,GrEspece, Etat, Nombre,DHPcm,Hautm,
               ST_m2,Vol_dm3, MSCR, ABCD, Vigueur)


  if(simplifier == TRUE){
    ArbreSamare_simp_min <-ArbreSamare %>% filter(Annee==MinAnnee )
    ArbreSamare_simp_max <-ArbreSamare %>% filter(Annee==MaxAnnee )
    ArbreSamare <-rbind(ArbreSamare_simp_min,ArbreSamare_simp_max)
  }

  return (ArbreSamare)

}
