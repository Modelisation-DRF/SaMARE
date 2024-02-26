#' Fonction qui structure un dataframe de sortie pour lequels on rapporte pour chaque
#' placette, annee, groupe d'espèce et itération le diamètre quadratique moyen,
#' la surface terrière, le volume et la hauteur dominante.

#' Cette fonction prend en paramètre un dataframe qui a déjà été simulé

#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#'                    iterations du simulateur SaMARE. Typiquement un résultat retourné
#'                    par la fonction "SimulSaMARE".
#' @return  Retourne un dataframe contenant par placette, groupe d'espèce, année
#'          et iteration la surface terrière le volume marchand brut, le diamètre
#'          moyen quadratique et la hauteur dominante.
#' @examples
#'


SortieDendroIterSamare <- function(SimulHtVol,simplifier=FALSE){
  select=dplyr::select
  MinAnnee = min(SimulHtVol$Annee)
  MaxAnnee = max(SimulHtVol$Annee)

  DendroIterSamaresp <- SimulHtVol %>%
                    mutate(Etat=ifelse(Etat=="mort","mort","vivant")) %>%
                    filter(Etat=="vivant") %>%
                    mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE,
                            vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE)) %>%
                    arrange( desc(DHPcm))%>%
                    group_by(Placette,Iter,Annee,GrEspece,Etat,Residuel) %>%
                    mutate(NbCum=cumsum(Nombre)) %>%
                    summarise(DQM=(mean(DHPcm^2,na.rm=TRUE))^0.5,ST_HA=sum(Stm2ha),Vol_HA=sum(vol_dm3)/1000,
                              nbTi_HA=sum(Nombre/Sup_PE),HDomM=ifelse(nbTi_HA>100,mean(hauteur_pred[1:first(which((NbCum/Sup_PE)>100))],na.rm = TRUE),mean(hauteur_pred)), .groups="drop")



  DendroIterSamare <- SimulHtVol %>%
                     mutate(Etat=ifelse(Etat=="mort","mort","vivant")) %>%
                     mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE,
                             vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE)) %>%
                     arrange( desc(hauteur_pred))%>%
                     group_by(Placette,Annee,Iter,Etat,Residuel) %>%
                     group_by(Placette,Iter,Annee,Etat,Residuel) %>%
                     mutate(NbCum=cumsum(Nombre)) %>%
                     summarise(DQM=(mean(DHPcm^2,na.rm=TRUE))^0.5,ST_HA=sum(Stm2ha),Vol_HA=sum(vol_dm3)/1000,
                               nbTi_HA=sum(Nombre/Sup_PE),HDomM=ifelse(nbTi_HA>100,mean(hauteur_pred[1:first(which((NbCum/Sup_PE)>100))],na.rm = TRUE),mean(hauteur_pred)), .groups="drop") %>%
                    mutate(GrEspece="TOT") %>%
                    rbind(DendroIterSamaresp) %>%
                    arrange(Placette,Annee,Residuel,Iter,GrEspece,desc(Etat)) %>%
                    relocate(Placette,Annee,Iter,Residuel,GrEspece,Etat,nbTi_HA,ST_HA,DQM,Vol_HA,HDomM)

if(simplifier == TRUE){
  DendroIterSamare_simp_min <-DendroIterSamare %>% filter(Annee==MinAnnee )
  DendroIterSamare_simp_max <-DendroIterSamare %>% filter(Annee==MaxAnnee )
  DendroIterSamare <-rbind(DendroIterSamare_simp_min,DendroIterSamare_simp_max)
}

  return (DendroIterSamare)

}
