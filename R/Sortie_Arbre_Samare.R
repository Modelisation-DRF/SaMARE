#' Fonction qui structure un dataframe de sortie dont chaque ligne correspond
#' à chacun des arbres par placette, par itération et par année.
#'
#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#'                    iterations du simulateur SaMARE. Typiquement un résultat retourné
#'                    par la fonction "SimulSaMARE".
#' @return  Retourne un dataframe contenant l'ensemble des arbres pour chacune des
#'          placettes, années, itérations.
#' @examples

SortieArbreSamare <- function(SimulHtVol){

  ArbreSamare <- SimulHtVol %>%
    mutate (Stm2ha=pi*(DHPcm/200)^2,

            Vigueur = case_when(
              vigu0 == "ViG" & prod0 == "sciage" ~ 1,
              vigu0 == "ViG" & prod0 == "pate" ~ 2,
              vigu0 == "NONVIG" & prod0 == "sciage" ~ 3,
              vigu0 == "NONVIG" & prod0 == "pate" ~ 4,
              TRUE ~ NA_integer_
            )) %>%
    group_by(Placette,Annee,GrEspece,Iter,Residuel) %>%
    select( c(Placette, Annee, Residuel, ArbreID, NoArbre, Nombre, GrEspece, Espece,
              Etat, DHPcm, Iter, hauteur_pred, vol_dm3, PlacetteID, Stm2ha,Vigueur)) %>%
    relocate(c(Placette,Annee,Iter,PlacetteID, Residuel,ArbreID,NoArbre, Espece,GrEspece, Etat, Nombre,DHPcm,hauteur_pred,
               Stm2ha,vol_dm3 ))

  # summarise(DQM=(mean(DHPcm^2,na.rm=TRUE))^0.5,ST_HA=sum(Stm2ha),VolM3Ha=sum(vol_dm3)/1000,
  #           nbTi_HA=sum(Nombre/Sup_PE),HDomM=ifelse(nbTi_HA>100,mean(DHPcm[1:100],na.rm = TRUE),mean(DHPcm)), .groups="drop")%>%


  #
  # if(ArbreSamare$vigu0=="ViG"&&ArbreSamare$prod0=="sciage"){
  #   ArbreSamare$Vigueur = 1
  #
  # }else if(ArbreSamare$vigu0=="ViG"&&ArbreSamare$prod0=="pate"){
  #   ArbreSamare$Vigueur = 2
  #
  # }else if(ArbreSamare$vigu0=="NONVIG"&&ArbreSamare$prod0=="sciage"){
  #   ArbreSamare$Vigueur = 3
  #
  # }else if(ArbreSamare$vigu0=="NONVIG"&&ArbreSamare$prod0=="pate"){
  #   ArbreSamare$Vigueur = 4
  #
  # }
  #




  return (ArbreSamare)

}
