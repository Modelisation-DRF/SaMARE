

#' cette function prend en parametre un dafra qui a déja simuller et billoner

#' @param Simul
#' @return
#' @examples
#'

SortieDendroSamare <- function(Simul){

  DendroSamare <- Simul %>% filter(Etat=="vivant") %>%
                                    mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE,
                                            vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE)) %>%arrange( desc(DHPcm))%>%
                                     group_by(Placette,Annee,GrEspece,Iter,Residuel) %>%
                                    summarise(DQM=(mean(DHPcm^2,na.rm=TRUE))^0.5,ST_HA=sum(Stm2ha),VolM3Ha=sum(vol_dm3)/1000,
                                              nbTi_HA=sum(Nombre/Sup_PE),HDomM=ifelse(nbTi_HA>100,mean(DHPcm[1:100],na.rm = TRUE),mean(DHPcm)), .groups="drop")%>%








  return (DendroSamare)

}
