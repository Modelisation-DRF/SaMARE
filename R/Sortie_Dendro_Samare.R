#' Fonction qui structure un dataframe de sortie pour lequels on rapporte pour chaque
#' placette, annee et groupe d'espèce le diamètre quadratique moyen,
#' la surface terrière, le volume et la hauteur dominante.

#' cette function prend en parametre un daframe qui a déja été simullé

#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#'                    iterations du simulateur SaMARE. Typiquement un résultat retourné
#'                    par la fonction "SimulSaMARE".
#' @return  Retourne un dataframe contenant par placette, groupe d'espèce, année
#'          et iteration la surface terrière le volume marchand brut, le diamètre
#'          moyen quadratique et la hauteur dominante.
#' @examples
#'


SortieDendroSamare <- function(SimulHtVol,simplifier=FALSE){
  select=dplyr::select

  MinAnnee = min(SimulHtVol$Annee)
  MaxAnnee = max(SimulHtVol$Annee)
  NbIter<-length(unique(SimulHtVol$Iter))
  Horizon=length(unique(SimulHtVol$Annee))-1


  DendroSamaresp <- SimulHtVol %>%
                    mutate(Etat=ifelse(Etat=="mort","mort","vivant")) %>%
                    filter(Etat=="vivant") %>%
                    mutate (Stm2ha=pi*(DHPcm/200)^2*Nombre/Sup_PE,
                            vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE*Nombre)) %>%
                    arrange(Placette,Iter,Annee,GrEspece,Etat,Residuel,desc(DHPcm))%>%
                    group_by(Placette,Iter,Annee,GrEspece,Etat,Residuel) %>%
                    mutate(NbCum=cumsum(Nombre)) %>%
                    summarise(DQM=(sum(DHPcm^2*Nombre)/sum(Nombre))^0.5,ST_HA=sum(Stm2ha),Vol_HA=sum(vol_dm3)/1000,
                              nbTi_HA=sum(Nombre/Sup_PE),HDomM=ifelse(nbTi_HA>100,mean(hauteur_pred[1:first(which((NbCum/Sup_PE)>100))],na.rm = TRUE),mean(hauteur_pred)), .groups="drop") %>%
                    group_by(Placette,Annee,GrEspece,Etat,Residuel) %>%
                    summarise(EcartType_DQM=sd(DQM),DQM=mean(DQM),EcartType_ST_HA=sd(ST_HA),ST_HA=mean(ST_HA),
                              EcartType_Vol_HA=sd(Vol_HA),Vol_HA=mean(Vol_HA), EcartType_nbTi_HA=sd(nbTi_HA),
                              nbTi_HA=mean(nbTi_HA),EcartType_HDomM=sd(HDomM),HDomM=mean(HDomM),.groups="drop")

  DendroSamare <- SimulHtVol %>%
                     mutate(Etat=ifelse(Etat=="mort","mort","vivant")) %>%
                     mutate (Stm2ha=pi*(DHPcm/200)^2*Nombre/Sup_PE,
                             vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3*Nombre/Sup_PE)) %>%
                     arrange(Placette,Iter,Annee,Etat,Residuel,desc(hauteur_pred))%>%
                     group_by(Placette,Iter,Annee,Etat,Residuel) %>%
                     mutate(NbCum=cumsum(Nombre)) %>%
                     summarise(DQM=(sum(DHPcm^2*Nombre)/sum(Nombre))^0.5,ST_HA=sum(Stm2ha),Vol_HA=sum(vol_dm3)/1000,
                              nbTi_HA=sum(Nombre/Sup_PE),HDomM=ifelse(nbTi_HA>100,mean(hauteur_pred[1:first(which((NbCum/Sup_PE)>100))],na.rm = TRUE),mean(hauteur_pred)), .groups="drop") %>%
                    mutate(GrEspece="TOT") %>%
                    group_by(Placette,GrEspece,Annee,Etat,Residuel) %>%
                    summarise(EcartType_DQM=sd(DQM),DQM=mean(DQM),EcartType_ST_HA=sd(ST_HA),ST_HA=mean(ST_HA),
                              EcartType_Vol_HA=sd(Vol_HA),Vol_HA=mean(Vol_HA), EcartType_nbTi_HA=sd(nbTi_HA),
                              nbTi_HA=mean(nbTi_HA),EcartType_HDomM=sd(HDomM),HDomM=mean(HDomM),.groups="drop") %>%
                    rbind(DendroSamaresp) %>%
                    arrange(Placette,Annee,Residuel,GrEspece,desc(Etat)) %>%
                    relocate(Placette,Annee,Residuel,GrEspece,Etat,nbTi_HA,ST_HA,DQM,Vol_HA,HDomM,
                             EcartType_nbTi_HA,EcartType_ST_HA,EcartType_DQM,EcartType_Vol_HA,EcartType_HDomM)
suppressMessages(
Recrutementsp<-SimulHtVol %>%
             filter(Etat=="recrue") %>%
              mutate (Stm2ha=pi*(DHPcm/200)^2*Nombre/Sup_PE) %>%
              group_by(Placette,Iter,Annee,GrEspece,Residuel) %>%
              summarise(AACRecrM2Ha=sum(Stm2ha),nbTi_HARecrues=sum(Nombre/Sup_PE)) %>%
              group_by(Placette,Annee,GrEspece,Residuel) %>%
              summarise(AACRecrM2HaAn=sum(AACRecrM2Ha)/(5*NbIter),.groups="drop"))
suppressMessages(
Recrutement <- Recrutementsp %>%
                group_by(Placette,Annee,Residuel) %>%
                summarise(AACRecrM2HaAn=sum(AACRecrM2HaAn)) %>%
                mutate(GrEspece="TOT") %>%
                rbind(Recrutementsp) %>%
                arrange(Placette,Annee,Residuel,GrEspece))
suppressMessages(
Mortalitesp<-SimulHtVol %>%
           group_by(Placette,Iter,ArbreID,GrEspece,Residuel) %>%
           mutate(DHP0=lag(DHPcm))%>%
           filter(Etat=="mort") %>%
            mutate (Stm2ha=pi*(DHP0/200)^2*Nombre/Sup_PE) %>%
            group_by(Placette,Iter,Annee,GrEspece,Residuel) %>%
            summarise(AACMortM2Ha=sum(Stm2ha)) %>%
            group_by(Placette,Annee,GrEspece,Residuel) %>%
            summarise(AACMortM2HaAn=-sum(AACMortM2Ha)/(5*NbIter),.groups="drop"))
suppressMessages(
Mortalite <-  Mortalitesp %>%
              group_by(Placette,Annee,Residuel) %>%
               summarise(AACMortM2HaAn=sum(AACMortM2HaAn)) %>%
                mutate(GrEspece="TOT") %>%
                rbind(Mortalitesp) %>%
                arrange(Placette,Annee,Residuel,GrEspece))
suppressMessages(
Accroissementsp<-SimulHtVol %>%
               group_by(Placette,Iter,ArbreID,GrEspece,Residuel) %>%
               mutate(DHP0=lag(DHPcm))%>%
               filter(Etat=="vivant") %>%
               mutate (Stm2ha0=pi*(DHP0/200)^2*Nombre/Sup_PE, Stm2ha=pi*(DHPcm/200)^2*Nombre/Sup_PE, AccSt=Stm2ha0-Stm2ha) %>%
               group_by(Placette,Iter,Annee,GrEspece,Residuel) %>%
               summarise(AACAccrM2Ha=sum(AccSt)) %>%
               group_by(Placette,Annee,GrEspece,Residuel) %>%
               summarise(AACAccrM2HaAn=-sum(AACAccrM2Ha)/(5*NbIter),.groups="drop"))
suppressMessages(
Accroissement <-Accroissementsp %>%
            group_by(Placette,Annee,Residuel) %>%
            summarise(AACAccrM2HaAn=sum(AACAccrM2HaAn)) %>%
            mutate(GrEspece="TOT") %>%
            rbind(Accroissementsp) %>%
            arrange(Placette,Annee,Residuel,GrEspece))
suppressMessages(
DendroSamare<-DendroSamare %>%
              left_join(Accroissement) %>%
              left_join(Mortalite) %>%
              left_join(Recrutement) %>%
              mutate(AACAccrM2HaAn=ifelse(is.na(AACAccrM2HaAn)==TRUE | Etat=="mort",0,AACAccrM2HaAn),
                     AACMortM2HaAn=ifelse(is.na(AACMortM2HaAn)==TRUE | Etat=="mort",0,AACMortM2HaAn),
                     AACRecrM2HaAn=ifelse(is.na(AACRecrM2HaAn)==TRUE | Etat=="mort",0,AACRecrM2HaAn),
                     AACBrut=AACAccrM2HaAn+AACRecrM2HaAn,
                     AACNet=AACBrut+AACMortM2HaAn) %>%
                    filter (Etat=="vivant") %>%
                    select(-Etat))




if(simplifier == TRUE){
  DendroIterSamare_simp_min <-DendroSamare %>% filter(Annee==MinAnnee )
  DendroIterSamare_simp_maxa <-DendroSamare %>%
                              filter(Annee==MaxAnnee ) %>%
                              select(-AACAccrM2HaAn,-AACMortM2HaAn,-AACRecrM2HaAn,-AACBrut,-AACBrut)

  DendroIterSamare_simp_maxb <-DendroSamare %>%
                              filter(Annee!=MinAnnee) %>%
                              group_by(Placette,Residuel,GrEspece) %>%
                              mutate(AACAccrM2HaAn=sum(AACAccrM2HaAn)/Horizon,
                                     AACMortM2HaAn=sum(AACMortM2HaAn)/Horizon,
                                     AACRecrM2HaAn=sum(AACRecrM2HaAn)/Horizon,
                                     AACBrut=sum(AACBrut)/Horizon,
                                     AACBrut=sum(AACNet)/Horizon)
  suppressMessages(
  DendroIterSamare_simp_max <-DendroIterSamare_simp_maxa %>%
                              left_join(DendroIterSamare_simp_maxb))



  DendroSamare <-rbind(DendroIterSamare_simp_min,DendroIterSamare_simp_max)
}


  return (DendroSamare)

}
