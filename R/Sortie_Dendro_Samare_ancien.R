#' Fonction qui structure un dataframe de sortie pour lequels on rapporte pour chaque
#' placette, annee et groupe d'espèce le diamètre quadratique moyen,
#' la surface terrière, le volume et la hauteur dominante.

#' cette function prend en parametre un daframe qui a déja été simullé

#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des
#'                    iterations du simulateur SaMARE. Typiquement un résultat retourné
#'                    par la fonction "SimulSaMARE".
#' @param simplifier Un booléen indiquant si les résultats doivent être simplifiés pour ne garder
#'                    que la première et la dernière année de la simulation. Par défaut, \code{FALSE}.
#' @return  Retourne un dataframe contenant par placette, groupe d'espèces, année
#'          et iteration la surface terrière le volume marchand brut, le diamètre
#'          moyen quadratique et la hauteur dominante.
#'          Les 10 groupes d'especes de SaMARE seront présentes dans chaque placette/annee
#' @export


SortieDendroSamare_ancien <- function(SimulHtVol,simplifier=FALSE){
  select=dplyr::select


  MinAnnee = min(SimulHtVol$Annee)
  MaxAnnee = max(SimulHtVol$Annee)
  NbIter<-length(unique(SimulHtVol$Iter))
  Horizon=length(unique(SimulHtVol$Annee))-1


 ListeGrSp<-data.frame("GrEspece"=c("AUT","BOJ","EPX","ERR","ERS","FEN","FIN","HEG","RES","SAB"))
 ListeSpIni<-SimulHtVol %>%
              group_by(Placette,Iter,Annee,Etat,Residuel,GrEspece) %>%
              summarise() %>%
              filter(Annee==MinAnnee)
 suppressMessages(
  ListeMerge<-SimulHtVol %>%
             group_by(Placette,Iter,Annee,Etat,Residuel) %>%
             summarise() %>%
             filter(Annee!=MinAnnee) %>%
             merge(ListeGrSp) %>%
            rbind(ListeSpIni) %>%
            filter(Etat=="vivant"))
 suppressMessages(
   DendroSamaresp <- SimulHtVol %>%
                    mutate(Etat=ifelse(Etat=="mort","mort","vivant")) %>%
                    filter(Etat=="vivant") %>%
                    mutate (Stm2ha=pi*(DHPcm/200)^2*Nombre/Sup_PE,
                            vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3/Sup_PE*Nombre)) %>%
                    arrange(Placette,Iter,Annee,GrEspece,Etat,Residuel,desc(hauteur_pred))%>%
                    group_by(Placette,Iter,Annee,GrEspece,Etat,Residuel) %>%
                    mutate(NbCum=cumsum(Nombre)) %>%
                    summarise(ST_HA=sum(Stm2ha),Vol_HA=sum(vol_dm3)/1000,nbTi_HA=sum(Nombre/Sup_PE), DQM=(ST_HA/nbTi_HA/pi)^0.5*200,
                              HDomM=ifelse(nbTi_HA>100,mean(hauteur_pred[1:first(which((NbCum/Sup_PE)>=100))],na.rm = TRUE),NA), .groups="drop") %>%
                    right_join(ListeMerge) %>%
                    mutate(ST_HA=ifelse(is.na(ST_HA)==TRUE,0,ST_HA),Vol_HA=ifelse(is.na(Vol_HA)==TRUE,0,Vol_HA),
                           nbTi_HA=ifelse(is.na(nbTi_HA)==TRUE,0,nbTi_HA)) %>%
                    group_by(Placette,Annee,GrEspece,Etat,Residuel) %>%
                    summarise(EcartType_DQM=sd(DQM, na.rm=TRUE),
                              EcartType_ST_HA=sd(ST_HA),
                              ST_HA=mean(ST_HA),
                              EcartType_Vol_HA=sd(Vol_HA),
                              Vol_HA=mean(Vol_HA),
                              EcartType_nbTi_HA=sd(nbTi_HA),
                              nbTi_HA=mean(nbTi_HA),
                              EcartType_HDomM=sd(HDomM, na.rm=TRUE),
                              DQM=(ST_HA/nbTi_HA/pi)^0.5*200,
                              HDomM=mean(HDomM, na.rm=TRUE),
                              .groups="drop"))

   suppressMessages(
   DendroSamare <- SimulHtVol %>%
                     mutate(Etat=ifelse(Etat=="mort","mort","vivant")) %>%
                     mutate (Stm2ha=pi*(DHPcm/200)^2*Nombre/Sup_PE,
                             vol_dm3=ifelse(is.na(vol_dm3)==TRUE,0,vol_dm3*Nombre/Sup_PE)) %>%
                     arrange(Placette,Iter,Annee,Etat,Residuel,desc(hauteur_pred))%>%
                     group_by(Placette,Iter,Annee,Etat,Residuel) %>%
                     mutate(NbCum=cumsum(Nombre)) %>%
                     summarise(ST_HA=sum(Stm2ha),Vol_HA=sum(vol_dm3)/1000, nbTi_HA=sum(Nombre/Sup_PE), DQM=(ST_HA/nbTi_HA/pi)^0.5*200,
                              HDomM=ifelse(nbTi_HA>100,mean(hauteur_pred[1:first(which((NbCum/Sup_PE)>=100))],na.rm = TRUE),NA), .groups="drop") %>%
                    mutate(GrEspece="TOT") %>%
                    group_by(Placette,GrEspece,Annee,Etat,Residuel) %>%
                    summarise(EcartType_DQM=sd(DQM),EcartType_ST_HA=sd(ST_HA),ST_HA=mean(ST_HA),
                              EcartType_Vol_HA=sd(Vol_HA),Vol_HA=mean(Vol_HA), EcartType_nbTi_HA=sd(nbTi_HA),
                              nbTi_HA=mean(nbTi_HA),EcartType_HDomM=sd(HDomM), DQM=(ST_HA/nbTi_HA/pi)^0.5*200,
                              HDomM=mean(HDomM),.groups="drop") %>%
                    rbind(DendroSamaresp) %>%
                    arrange(Placette,Annee,Residuel,GrEspece,desc(Etat)) %>%
                    relocate(Placette,Annee,Residuel,GrEspece,Etat,nbTi_HA,ST_HA,DQM,Vol_HA,HDomM,
                             EcartType_nbTi_HA,EcartType_ST_HA,EcartType_DQM,EcartType_Vol_HA,EcartType_HDomM))


suppressMessages(
Recrutementsp<-SimulHtVol %>%
             filter(Etat=="recrue") %>%
              mutate (Stm2ha=pi*(DHPcm/200)^2*Nombre/Sup_PE) %>%
              group_by(Placette,Iter,Annee,GrEspece,Residuel) %>%
              summarise(AACRecrM2Ha=sum(Stm2ha),nbTi_HARecrues=sum(Nombre/Sup_PE)) %>%
              group_by(Placette,Annee,GrEspece,Residuel) %>%
              summarise(AACRecrM2HaAn = sum(AACRecrM2Ha)/(5*NbIter),.groups="drop"))
suppressMessages(
Recrutement <- Recrutementsp %>%
                group_by(Placette,Annee,Residuel) %>%
                summarise(AACRecrM2HaAn = sum(AACRecrM2HaAn)) %>%
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
            summarise(AACMortM2HaAn = -sum(AACMortM2Ha)/(5*NbIter),.groups="drop"))
suppressMessages(
Mortalite <-  Mortalitesp %>%
              group_by(Placette,Annee,Residuel) %>%
               summarise(AACMortM2HaAn = sum(AACMortM2HaAn)) %>% # pas besoin de négatif, car on somme les négatifs précédents
                mutate(GrEspece="TOT") %>%
                rbind(Mortalitesp) %>%
                arrange(Placette,Annee,Residuel,GrEspece))


suppressMessages(
Accroissementsp<-SimulHtVol %>%
               group_by(Placette,Iter,ArbreID,GrEspece,Residuel) %>%
               mutate(DHP0=lag(DHPcm))%>%
               filter(Etat=="vivant") %>%
               mutate (Stm2ha0=pi*(DHP0/200)^2*Nombre/Sup_PE, Stm2ha=pi*(DHPcm/200)^2*Nombre/Sup_PE,
                       AccSt = Stm2ha0-Stm2ha) %>%
               group_by(Placette,Iter,Annee,GrEspece,Residuel) %>%
               summarise(AACAccrM2Ha=sum(AccSt)) %>%
               group_by(Placette,Annee,GrEspece,Residuel) %>%
               summarise(AACAccrM2HaAn = -sum(AACAccrM2Ha)/(5*NbIter),  # on met un négatifs car on a fait la diif des ST à l'envers
                         .groups="drop"))
suppressMessages(
Accroissement <-Accroissementsp %>%
            group_by(Placette,Annee,Residuel) %>%
            summarise(AACAccrM2HaAn = sum(AACAccrM2HaAn)) %>%
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
                              #select(-AACAccrM2HaAn,-AACMortM2HaAn,-AACRecrM2HaAn,-AACBrut,-AACBrut) # erreur, il y a 2 fois le brut
                              select(-AACAccrM2HaAn,-AACMortM2HaAn,-AACRecrM2HaAn,-AACBrut,-AACNet)

  DendroIterSamare_simp_maxb <-DendroSamare %>%
                              filter(Annee!=MinAnnee) %>%
                              group_by(Placette,Residuel,GrEspece) %>%
                              mutate(AACAccrM2HaAn=sum(AACAccrM2HaAn)/Horizon,
                                     AACMortM2HaAn=sum(AACMortM2HaAn)/Horizon,
                                     AACRecrM2HaAn=sum(AACRecrM2HaAn)/Horizon,
                                     AACBrut=sum(AACBrut)/Horizon,
                                     #AACBrut=sum(AACNet)/Horizon) # erreur, brut au lieu de net
                                     AACNet=sum(AACNet)/Horizon)
  suppressMessages(
  DendroIterSamare_simp_max <-DendroIterSamare_simp_maxa %>%
                              left_join(DendroIterSamare_simp_maxb))



  DendroSamare <-rbind(DendroIterSamare_simp_min,DendroIterSamare_simp_max)
}


  return (DendroSamare)

}


