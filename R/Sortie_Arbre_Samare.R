

#' cette function prend en parametre un dafra qui a déja simuller
#'
#' @param SimulHtVol
#' @return
#' @examples

SortieArbreSamare <- function(SimulHtVol){

  ArbreSamare <- SimulHtVol %>%
    mutate (Stm2ha=pi*(DHPcm/200)^2) %>%
    group_by(Placette,Annee,GrEspece,Iter,Residuel) %>%
    select(-c(vigu0, Nb_Gaules_ERS,Nb_Gaules_BOJ,Nb_Gaules_SAB , Nb_Gaules_68_ERS,
              Nb_Gaules_68_HEG,Nb_Gaules_68_BOJ,Nb_Gaules_68_SAB,
              Nb_Gaules_68_Ha , Nb_Gaules_HEG,Nb_Gaules_Ha,nb_tige,veg_pot,milieu, Tmoy,Ptot,Altitude, Type_Eco, reg_eco, prod0, Sup_PE))

    # summarise(DQM=(mean(DHPcm^2,na.rm=TRUE))^0.5,ST_HA=sum(Stm2ha),VolM3Ha=sum(vol_dm3)/1000,
    #           nbTi_HA=sum(Nombre/Sup_PE),HDomM=ifelse(nbTi_HA>100,mean(DHPcm[1:100],na.rm = TRUE),mean(DHPcm)), .groups="drop")%>%








    return (ArbreSamare)

}
