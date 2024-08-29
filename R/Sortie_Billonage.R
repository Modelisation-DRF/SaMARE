


SortieBillonage <- function(Data, Type ){

  select=dplyr::select

  Data<- Data %>% filter(DHPcm >23)

  data<- Data %>% filter(Espece %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHX") )

  if (nrow(data) == 0) {

    Data<- Data %>% mutate(erreur = "Code d'essence à l'extérieur de la plage de valeurs possibles pour billongae ")

    return(Data)
  }

  data1 <- data %>% mutate(bilonID = seq_len(nrow(data)))


  billo <- SIMBillonnageABCD_DHP(data1, Type)

  final <- left_join(data1, billo, by = "bilonID") %>%  mutate (Stm2ha=pi*(DHPcm/200)^2,
                                                                Vigueur = case_when(
                                                                  vigu0 == "ViG" & prod0 == "sciage" ~ 1,
                                                                  vigu0 == "ViG" & prod0 == "pate" ~ 2,
                                                                  vigu0 == "NONVIG" & prod0 == "sciage" & DHPcm>=23.1 ~ 3,
                                                                  vigu0 == "NONVIG" & prod0 == "sciage" & DHPcm<23.1 ~ 4,
                                                                  vigu0 == "NONVIG" & prod0 == "pate" ~ 4,
                                                                  vigu0 == "ViG" & prod0 == "resineux" ~ 5,
                                                                  vigu0 == "NONVIG" & prod0 == "resineux" ~ 6,
                                                                  TRUE ~ NA_integer_
                                                                ))

  final <- final %>% select(Annee,Residuel,ArbreID,Iter,NoArbre,Placette,Nombre,GrEspece,Espece,
                            Etat,DHPcm,MSCR,hauteur_pred,vol_dm3,Stm2ha,Sup_PE,ABCD,Vigueur,DER,F1,F2,F3,F4,P,type)


  return(final)

  }
