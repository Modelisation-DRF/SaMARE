#' Fonction qui structure un dataframe de sortie dont chaque ligne correspond
#' à la moyenne des itérations du nombre d'arbres par placette, groupe d'espèce,
#' classe de DHP et année. La sortie présente donc un sommaire de la table de peuplement simulée
#' pour chacune des placettes.
#'
#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#'                    iterations du simulateur SaMARE. Typiquement un résultat retourné
#'                    par la fonction "SimulSaMARE".
#' @param NbIter Nombre d'iterations utilisées pour créer le dataframe "SimulHtVol"
#' @return Retourne un dataframe contenant le sommaire des itérations par placette,
#'          groupe d'espèce, classe de DHP et année.
#' @examples

Sommaire_Classes_DHP <- function(SimulHtVol,NbIter){

  select=dplyr::select
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
