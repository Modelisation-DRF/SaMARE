
valide_data <- function(data) {
  data <- renommer_les_colonnes(data)
  validations <- list(
    valide_espece = "Code d'essence non valide",
    valide_Etat = "Code d'Etat non valide",
    valide_DHPcm = "Valeurs de DHP non permise",
    valide_Vigueur = "Code de vigueur non permis",
    valide_Sup_PE = "Superficie de la placette en ha non valide",
    valide_Nombre = "valeur de nombre null",
    valide_Annee_Coupe = "Annee coupe non valide",
    valide_Latitude = "Latitude non valide",
    valide_Longitude = "Longitude non valide",
    valide_Altitude = "Altitude non valide",
    verifier_arbre_uniques_par_placette = "deux arbres identique pour la même placette",
   # valide_Ptot = "Ptot non valide",
   # valide_Tmoy = "Tmoy non valide",
    valide_Type_Eco = "valeur Type_Eco null",
    valide_MSCR = "MSCR non valide",
    valide_Reg_Eco = "Valeure non permise pour Reg_Eco",
    valide_ABCD = "ABCD non valide",
    valide_Pente = "Pente non valide",
    valide_ntrt = "Entrer le nombre de traitements"
   # valide_GrwDays = "GrwDays non valide"
  )

  # Initialiser la liste des erreurs
  erreurs <-list()

  # Itérer sur chaque validation
  for (nom_validation in names(validations)) {
    # Appeler dynamiquement la fonction de validation en utilisant do.call
    valide <- do.call(nom_validation, list(data = data))

    # Si la validation échoue, ajouter le message d'erreur correspondant à la liste
    if (!valide) {
      erreurs <- c(erreurs, validations[[nom_validation]])
    }
  }

  return(erreurs)
}

valide_data_gaules <- function(data) {

  validations <- list(
    valide_espece = "Code d'essence non valide",
    valide_DHPcm_gaules = "Valeurs de DHP non permise",
    valide_Sup_PE_gaules = "Superficie de la placette en ha non valide",
    valide_Nombre = "valeur de nombre null"
  )

  # Initialiser la liste des erreurs
  erreurs <-list()

  # Itérer sur chaque validation
  for (nom_validation in names(validations)) {
    # Appeler dynamiquement la fonction de validation en utilisant do.call
    valide <- do.call(nom_validation, list(data = data))

    # Si la validation échoue, ajouter le message d'erreur correspondant à la liste
    if (!valide) {
      erreurs <- c(erreurs, validations[[nom_validation]])
    }
  }

  return(erreurs)
}

valide_Nombre <- function(data){

  if(!"Nombre" %in% names(data)){
    return (FALSE)
  }
  return(all(data$Nombre>0))

}




valide_espece <- function(data){

  if(!"Espece" %in% names(data)){
    return (FALSE)
  }

  valeurs_autorisees <- c("BOG","BOJ","BOP", "CAC", "CAF", "CAR", "CEO", "CET", "CHB", "CHE","CHG",
                          "CHR", "EPB", "EPN", "EPO","EPR","ERA" , "ERG","ERN", "ERP","ERR","ERS",
                          "FRA","FRN","FRP", "HEG", "JUV","MAS" , "MEJ" , "MEL", "MEU","NOC","ORA",
                          "ORR","ORT","OSV","PEB","PED","PEG","PEH","PET","PIB","PID","PIG","PIR",
                          "PIS","PRP","PRU","SAB","SAL","SOA","SOD","THO","TIL" ,"AME"  ,"AUR","ERE"
                          ,"TIA" ,"CEP")

  return(all(data$Espece %in% valeurs_autorisees))
}


valide_Etat <- function(data){

  if(!"Etat" %in% names(data)){
    return (FALSE)
  }

  valeurs_autorisees<-c(10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25, 26, 27,
                        28, 29, 30, 32, 34, 36, 40, 42, 43, 44, 46, 50, 52, 54, 56 )

  return(all(data$Etat %in% valeurs_autorisees))

}

valide_DHPcm <- function(data){

  if(!"DHPcm" %in% names(data)){
    return (FALSE)
  }
  return(all(data$DHPcm <= 160 ))

}

valide_DHPcm_gaules <- function(data){

  if(!"DHPcm" %in% names(data)){
    return (FALSE)
  }
  return(all(between(data$DHPcm, 1.0, 9.0) ))

}

valide_Vigueur <- function(data){
  if (!all(c("Vigueur", "MSCR") %in% names(data))) {
    return(FALSE)
  }
   valeurs_autorisees <- c(1, 2, 3, 4, 5, 6,NA)


  resultats <- sapply(1:nrow(data), function(i) {

  condition_1 <- data$Vigueur %in% valeurs_autorisees

  condition_2 <- ifelse(is.na(data$Vigueur), data$MSCR %in% c('M', 'S', 'C', 'R', 'MS', 'CR'), TRUE)

  condition_3 <- ifelse(data$Vigueur %in% c(1,2,3,4), data$Espece %in% c("BOG","BOJ","BOP", "CAC", "CAF", "CAR", "CEO", "CET", "CHB", "CHE","CHG",
                                                                         "CHR","ERA" , "ERG","ERN", "ERP","ERR","ERS",
                                                                         "FRA","FRN","FRP", "HEG", "JUV","MAS" ,"NOC","ORA",
                                                                         "ORR","ORT","OSV","PEB","PED","PEG","PEH","PET",
                                                                         "PRP","SAL","SOA","SOD","TIL" ,"AME"  ,"AUR","ERE"
                                                                         ,"TIA" ,"CEP"), TRUE)

  condition_4 <- ifelse(data$Vigueur %in% c(5,6), data$Espece %in% c("EPB", "EPN", "EPO","EPR",
                                                                     "MEJ" , "MEL", "MEU",
                                                                     "PIB","PID","PIG","PIR",
                                                                     "PIS","PRU","SAB","THO"), TRUE)

  condition_5 <- ifelse(data$Vigueur %in% c(3), data$DHPcm >23.1, TRUE)

  return(condition_1 & condition_2 & condition_3 & condition_4 & condition_5)

    })




  return(all(resultats))

}

valide_Sup_PE <- function(data){
  if(!"Sup_PE" %in% names(data)){
    return (FALSE)
  }

  resultat <- all(between(data$Sup_PE, 0.04, 1))

  return(resultat)

}

valide_Sup_PE_gaules <- function(data){
  if(!"Sup_PE" %in% names(data)){
    return (FALSE)
  }

  resultat <- all(between(data$Sup_PE, 0.004, 1))

  return(resultat)

}

valide_Annee_Coupe <- function(data){

  if(!"Annee_Coupe" %in% names(data)){
    return (FALSE)
  }

  condition <- ifelse(!is.na(data$Annee_Coupe), between(data$Annee_Coupe, 1900, 2100), TRUE)


  return(all(condition))

}


valide_Latitude <- function(data){

  if(!"Latitude" %in% names(data)){
    return (FALSE)
  }
  return(all(data$Latitude > 45 & data$Latitude < 48.5 ))

}

valide_Longitude <- function(data){

  if(!"Longitude" %in% names(data)){
    return (FALSE)
  }
  return(all(data$Longitude > -79.5 & data$Longitude < -64.0 ))

}


valide_Altitude <- function(data){

  if(!"Altitude" %in% names(data)){
    return (FALSE)
  }

  return(all(data$Altitude < 1000  ))

}

valide_Ptot <- function(data){
  if(!"Ptot" %in% names(data)){
    return (FALSE)
  }

  return(all(data$Ptot < 2000  ))

}


valide_Tmoy <- function(data){
  if(!"Tmoy" %in% names(data)){
    return (FALSE)
  }

  return(all(data$Tmoy > 0 & data$Tmoy < 10 ))

}


valide_Type_Eco <- function(data){

  if(!"Type_Eco" %in% names(data)){
    return (FALSE)
  }
  return(all(!is.na(data$Type_Eco )))

}


valide_Reg_Eco <- function(data){
  if(!"Reg_Eco" %in% names(data)){
    return (FALSE)
  }

  valeurs_autorisees<-c("1a", "2a", "2b", "2c", "3a", "3b", "3c", "3d", "4a", "4b", "4c", "4d", "4e", "4f", "4g",
                        "4h", "DU", "SV")

  return(all(data$Reg_Eco %in% valeurs_autorisees))

}

valide_MSCR <- function(data){

  if (!all(c("Vigueur", "MSCR") %in% names(data))) {
    return(FALSE)
  }
  valeurs_autorisees<-c('M', 'S', 'C', 'R','MS','CR')

  condition_1 <- data$MSCR %in% valeurs_autorisees | is.na(data$MSCR)
  condition_2 <- ifelse(is.na(data$MSCR), data$Vigueur %in% c(1,2,3,4,5,6), TRUE)

  return(all(condition_1 & condition_2))


}


valide_ABCD <- function(data){

  if(!"ABCD" %in% names(data)){
    return (FALSE)
  }

  Espece_specifiques <- c("BOG","BOJ","BOP", "CAC", "CAF", "CAR", "CEO", "CET", "CHB", "CHE","CHG",
                          "CHR","ERA" , "ERG","ERN", "ERP","ERR","ERS",
                          "FRA","FRN","FRP", "HEG", "JUV","MAS" ,"NOC","ORA",
                          "ORR","ORT","OSV","PEB","PED","PEG","PEH","PET",
                          "PRP","SAL","SOA","SOD","TIL" ,"AME"  ,"AUR","ERE"
                          ,"TIA" ,"CEP")
  valeurs_autorisees <- c("A","B","C","D",NA)


  resultats <- sapply(1:nrow(data), function(i) {

    GrEspece  <- data$Espece [i]
    ABCD  <- data$ABCD[i]
    DHP  <- data$DHPcm[i]

    condition0 <- ABCD %in% valeurs_autorisees

    condition1 <-  ifelse(!GrEspece  %in% Espece_specifiques,  is.na(ABCD),TRUE)

    condition2 <-  ifelse(DHP<23.1,  is.na(ABCD),TRUE)

    condition3 <-  ifelse(between(DHP, 23.1 , 33.0),  ABCD %in% c("C","D",NA),TRUE)

    condition4 <-  ifelse(between(DHP, 33.1 , 39.0),  ABCD %in% c("C","D", "B",NA),TRUE)

    return(condition0 & condition1 & condition2 & condition3 & condition4)
  })

  return(all(resultats))
}


valide_Pente <- function(data){
  if(!"Pente" %in% names(data)){
    return (FALSE)
  }

  return(all(data$Pente >=0 & data$Pente < 100 ))

}

valide_GrwDays <- function(data){
  if(!"GrwDays" %in% names(data)){
    return (FALSE)
  }

  return(all(data$GrwDays >=1 & data$GrwDays < 365 ))

}

valide_ntrt <- function(data){
  if(!"ntrt" %in% names(data)){
    return (FALSE)
  }


  return(all(data$ntrt %in% c(0,1,2)))
}


verifier_arbre_uniques_par_placette <- function(data) {

  data_diviser_par_placette <- split(data, data$Placette)


  arbre_uniques_par_placette <- lapply(data_diviser_par_placette, function(data_placette) {

    length(unique(data_placette$NoArbre)) == nrow(data_placette)
  })


    return(all(arbre_uniques_par_placette))

}


