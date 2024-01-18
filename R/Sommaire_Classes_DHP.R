
#'  sortie Sommaire Classes DHP
#'
#' @param SimulHtVol
#' @param NbIter
#' @return
#' @examples

Sommaire_Classes_DHP <- function(SimulHtVol,NbIter){


  SommaireClassesDHPSp<-SimulHtVol %>%
    filter(Etat!="mort") %>%
    mutate(Stm2ha=pi*(DHPcm/200)^2/Sup_PE,      #Calcul surface terrière par ha
           DHPcm2=DHPcm^2,
           Nb=Nombre/Sup_PE,                   #Nb tige ha
           vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE),#volume par ha en dm3 a mettre en m3
           DHP_cl=round(DHPcm/2)*2) %>%
    group_by(Placette,GrEspece,DHP_cl,Annee,Iter) %>%
    summarise(StM2Ha=sum(Stm2ha), NbHa=sum(Nb), DQM=(mean(DHPcm2,na.rm=TRUE))^0.5,
              VolM3Ha=sum(vol_dm3)/1000, .groups="drop") %>%
    group_by(Placette,Annee,GrEspece,DHP_cl) %>%
    summarise(NbHa=sum(NbHa)/NbIter, StM2Ha=sum(StM2Ha)/NbIter,
              VolM3Ha=sum(VolM3Ha)/NbIter, .groups="drop") %>%
    arrange(Placette,Annee,GrEspece,DHP_cl)

  SommaireClassesDHP<-SommaireClassesDHPSp %>%
    group_by(Placette,Annee,DHP_cl) %>%
    summarise(NbHa=sum(NbHa), StM2Ha=sum(StM2Ha),VolM3Ha=sum(VolM3Ha)) %>%
    mutate(GrEspece="TOT") %>%
    rbind(SommaireClassesDHPSp) %>%
    arrange(Placette,Annee,GrEspece,DHP_cl)

  return(SommaireClassesDHP)
}
