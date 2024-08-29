


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

  final <- left_join(data1, billo, by = "bilonID")
  final <- final %>% select(-bilonID)


  return(final)

  }
