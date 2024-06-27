#' Fonction qui structure un dataframe de sortie dont chaque ligne correspond
#' à la moyenne des itérations du nombre d'arbres par placette, groupe d'espèce,
#' classe de DHP et année. La sortie présente donc un sommaire de la table de peuplement simulée
#' pour chacune des placettes.
#'
#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#'                    iterations du simulateur SaMARE. Typiquement un résultat retourné
#'                    par la fonction "SimulSaMARE".
#' @param simplifier Un booléen indiquant si les résultats doivent être simplifiés pour ne garder
#'                  que la première et la dernière année de la simulation. Par défaut, \code{FALSE}.
#' @return Retourne un dataframe contenant le sommaire des itérations par placette,
#'          groupe d'espèce, classe de DHP et année.
#' @examples

Sommaire_Classes_DHP <- function(SimulHtVol ,simplifier=FALSE){
  select=dplyr::select

  MinAnnee = min(SimulHtVol$Annee)
  MaxAnnee = max(SimulHtVol$Annee)

  NbIter<-length(unique(SimulHtVol$Iter))

    SommaireClassesDHPSp<-SimulHtVol %>%
    filter(Etat!="mort") %>%
    mutate(vigueur=ifelse(vigu0=="ViG" & prod0=="sciage",1,
                          ifelse(vigu0=="ViG" & prod0=="pate",2,
                                ifelse(vigu0=="NONVIG" & prod0=="sciage",3,
                                       ifelse(vigu0=="NONVIG" & prod0=="pate",4,
                                              ifelse(vigu0=="ViG" & prod0=="resineux",5,6)))))) %>%
    mutate(Stm2ha=pi*(DHPcm/200)^2/Sup_PE,      #Calcul surface terrière par ha
           DHPcm2=DHPcm^2,
           vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE),#volume par ha en dm3 a mettre en m3
           DHP_cl=round(DHPcm/2)*2) %>%
    group_by(Placette,GrEspece,DHP_cl,vigueur,Annee,Iter) %>%
    summarise(StM2Ha=sum(Stm2ha), VolM3Ha=sum(vol_dm3)/1000, .groups="drop") %>%
    group_by(Placette,Annee,GrEspece,DHP_cl,vigueur) %>%
    summarise(StM2Ha=sum(StM2Ha)/NbIter,
              VolM3Ha=sum(VolM3Ha)/NbIter, .groups="drop") %>%
    mutate(NbHa=StM2Ha/((DHP_cl/200)^2*pi)) %>%
    arrange(Placette,Annee,GrEspece,DHP_cl,vigueur)

suppressMessages(
SommaireClassesDHP<-SommaireClassesDHPSp %>%
    group_by(Placette,Annee,DHP_cl,vigueur) %>%
    summarise(NbHa=sum(NbHa), StM2Ha=sum(StM2Ha),VolM3Ha=sum(VolM3Ha)) %>%
    mutate(GrEspece="TOT") %>%
    rbind(SommaireClassesDHPSp) %>%
    arrange(Placette,Annee,GrEspece,DHP_cl,vigueur))

SommaireClassesDHP<-SommaireClassesDHP[,c(1,2,8,3,4,5,6,7)]

if(simplifier == TRUE){

  SommaireClassesDHP_simp_min <-SommaireClassesDHP %>% filter(Annee==MinAnnee )
  SommaireClassesDHP_simp_max <-SommaireClassesDHP %>% filter(Annee==MaxAnnee )
  SommaireClassesDHP <-rbind(SommaireClassesDHP_simp_min,SommaireClassesDHP_simp_max)

}

  return(SommaireClassesDHP)
}
