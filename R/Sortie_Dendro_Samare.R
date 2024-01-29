#' Fonction qui structure un dataframe de sortie pour lequels on rapporte pour chaque
#' placette, annee, groupe d'espèce et itération le diamètre quadratique moyen,
#' la surface terrière, le volume et la hauteur dominante.

#' cette function prend en parametre un dafra qui a déja simuller et billoner

#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#'                    iterations du simulateur SaMARE. Typiquement un résultat retourné
#'                    par la fonction "SimulSaMARE".
#' @return  Retourne un dataframe contenant par placette, groupe d'espèce, année
#'          et iteration la surface terrière le volume marchand brut, le diamètre
#'          moyen quadratique et la hauteur dominante.
#' @examples
#'

SortieDendroSamare <- function(SimulHVol){
  select=dplyr::select
  DendroSamaresp <- SimulHtVol %>%
                    filter(Etat=="vivant") %>%
                    mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE,
                            vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE)) %>%
                    arrange( desc(DHPcm))%>%
                    group_by(Placette,Annee,GrEspece,Iter,Residuel,PlacetteID) %>%
                    summarise(DQM=(mean(DHPcm^2,na.rm=TRUE))^0.5,ST_HA=sum(Stm2ha),VolM3Ha=sum(vol_dm3)/1000,
                              nbTi_HA=sum(Nombre/Sup_PE),HDomM=ifelse(nbTi_HA>100,mean(DHPcm[1:100],na.rm = TRUE),mean(DHPcm)), .groups="drop")

  DendroSamare <- SimulHtVol %>%
                     filter(Etat=="vivant") %>%
                     mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE,
                             vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE)) %>%
                     arrange( desc(hauteur_pred))%>%
                     group_by(Placette,Annee,Iter,Residuel,PlacetteID) %>%
                    summarise(DQM=(mean(DHPcm^2,na.rm=TRUE))^0.5,ST_HA=sum(Stm2ha),VolM3Ha=sum(vol_dm3)/1000,
                              nbTi_HA=sum(Nombre/Sup_PE),HDomM=ifelse(nbTi_HA>100,mean(DHPcm[1:100],na.rm = TRUE),mean(DHPcm)), .groups="drop") %>%
                    mutate(GrEspece="TOT") %>%
                    rbind(DendroSamaresp) %>%
    relocate(Placette,Annee,Iter,PlacetteID,Residuel,GrEspece,nbTi_HA,ST_HA,DQM,VolM3Ha,HDomM)







  return (DendroSamare)

}
