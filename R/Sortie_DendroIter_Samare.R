#' Structurer un dataframe de sortie pour les simulations SaMARE
#'
#' La fonction \code{SortieDendroIterSamare} structure un dataframe de sortie pour lequel
#' on rapporte, pour chaque placette, année, groupe d'espèce et itération, le diamètre
#' quadratique moyen, la surface terrière, le volume et la hauteur dominante.
#' Cette fonction prend en paramètre un dataframe qui a déjà été simulé.
#'
#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#' itérations du simulateur SaMARE. Typiquement un résultat retourné par la fonction \code{SimulSaMARE}.
#' @param simplifier Un booléen indiquant si les résultats doivent être simplifiés pour ne garder
#' que les années minimales et maximales. Par défaut, \code{FALSE}.
#'
#' @return Un dataframe contenant, pour chaque placette, groupe d'espèce, année et itération,
#' la surface terrière, le volume marchand brut, le diamètre moyen quadratique et la hauteur dominante.
#'
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe `SimulHtVol` contenant les résultats de simulation
#'
#' # Appel de la fonction
#' resultats <- SortieDendroIterSamare(SimulHtVol)
#'
#' # Pour simplifier les résultats
#' resultats_simplifies <- SortieDendroIterSamare(SimulHtVol, simplifier = TRUE)
#' }
#'
#' @export

SortieDendroIterSamare <- function(SimulHtVol,simplifier=FALSE){
  select=dplyr::select
  MinAnnee = min(SimulHtVol$Annee)
  MaxAnnee = max(SimulHtVol$Annee)

  DendroIterSamaresp <- SimulHtVol %>%
                    mutate(Etat=ifelse(Etat=="mort","mort","vivant")) %>%
                    filter(Etat=="vivant") %>%
                    mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE,
                            vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE)) %>%
                    arrange(  desc(hauteur_pred))%>%
                    group_by(Placette,Iter,Annee,GrEspece,Etat,Residuel) %>%
                    mutate(NbCum=cumsum(Nombre)) %>%
                    summarise(DQM=(sum(DHPcm^2*Nombre)/sum(Nombre))^0.5,ST_HA=sum(Stm2ha),Vol_HA=sum(vol_dm3)/1000,
                              nbTi_HA=sum(Nombre/Sup_PE),HDomM=ifelse(nbTi_HA>100,mean(hauteur_pred[1:first(which((NbCum/Sup_PE)>=100))],na.rm = TRUE),mean(hauteur_pred)), .groups="drop")



  DendroIterSamare <- SimulHtVol %>%
                     mutate(Etat=ifelse(Etat=="mort","mort","vivant")) %>%
                     mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE,
                             vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE)) %>%
                     arrange( desc(hauteur_pred))%>%
                     group_by(Placette,Iter,Annee,Etat,Residuel) %>%
                     mutate(NbCum=cumsum(Nombre)) %>%
                     summarise(DQM=(sum(DHPcm^2*Nombre)/sum(Nombre))^0.5,
                               ST_HA=sum(Stm2ha),
                               Vol_HA=sum(vol_dm3)/1000,
                               nbTi_HA=sum(Nombre/Sup_PE),
                               HDomM=ifelse(nbTi_HA>100,mean(hauteur_pred[1:first(which((NbCum/Sup_PE)>=100))],na.rm = TRUE),mean(hauteur_pred)), .groups="drop") %>%
                    mutate(DQM=ifelse(Etat=="mort",NA,DQM),ST_HA=ifelse(Etat=="mort",NA,ST_HA),
                                      Vol_HA=ifelse(Etat=="mort",NA,Vol_HA),HDomM=ifelse(Etat=="mort",NA,HDomM)) %>%
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
