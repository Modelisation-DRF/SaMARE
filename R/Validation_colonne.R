
valide_data <- function(data) {
  data <- renommer_les_colonnes(data)
  validations <- list(
    valide_espece = "Code d'essence non valide",
    valide_Etat = "Code d'etat non valide",
    valide_DHPcm = "Valeurs de DHP non permise",
    valide_Vigueur = "Code de vigueur non permis",
    valide_Sup_PE = "Superficie de la placette en ha non valide",
    valide_Nombre = "valeur de nombre null",
    valide_Annee_Coupe = "Annee coupe non valide",
    valide_Latitude = "Latitude non valide",
    valide_Longitude = "Longitude non valide",
    valide_Altitude = "Altitude non valide",
    verifier_arbre_uniques_par_placette = "plusieurs noarbre identiques pour la même placette ",
   # valide_Ptot = "Ptot non valide",
   # valide_Tmoy = "Tmoy non valide",
    valide_Type_Eco = "valeur Type_Eco null",
    valide_MSCR = "MSCR non valide",
    valide_Reg_Eco = "Valeur non permise pour Reg_Eco",
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
  if(any(is.na(data$Nombre)) ){
    return (FALSE)
  }

  return(all(data$Nombre>0))

}




valide_espece <- function(data){

  if(!"Espece" %in% names(data)){
    return (FALSE)
  }
  if(any(is.na(data$Espece)) ){
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
  if(any(is.na(data$Etat)) ){
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

  if(any(is.na(data$DHPcm)) ){
    return (FALSE)
  }
  return(all(data$DHPcm <= 160 ))

}

valide_DHPcm_gaules <- function(data){

  if(!"DHPcm" %in% names(data)){
    return (FALSE)
  }

  if(any(is.na(data$DHPcm)) ){
    return (FALSE)
  }
  return(all(between(data$DHPcm, 1.0, 9.0) ))

}

valide_Vigueur <- function(data) {

  if (!all(c("Vigueur", "MSCR", "DHPcm", "Espece") %in% names(data))) {
    return(FALSE)
  }


  valeurs_autorisees <- c(1, 2, 3, 4, 5, 6, NA)
  Espece_specifiques_1_4 <- c("BOG","BOJ","BOP", "CAC", "CAF", "CAR", "CEO", "CET", "CHB", "CHE","CHG",
                              "CHR","ERA" , "ERG","ERN", "ERP","ERR","ERS",
                              "FRA","FRN","FRP", "HEG", "JUV","MAS" ,"NOC","ORA",
                              "ORR","ORT","OSV","PEB","PED","PEG","PEH","PET",
                              "PRP","SAL","SOA","SOD","TIL" ,"AME"  ,"AUR","ERE",
                              "TIA" ,"CEP")
  Espece_specifiques_5_6 <- c("EPB", "EPN", "EPO","EPR",
                              "MEJ" , "MEL", "MEU",
                              "PIB","PID","PIG","PIR",
                              "PIS","PRU","SAB","THO")


  resultats <- data %>%
    mutate(
      condition_1 = Vigueur %in% valeurs_autorisees,
      condition_2 = if_else(is.na(Vigueur), MSCR %in% c('M', 'S', 'C', 'R', 'MS', 'CR'), TRUE),
      condition_3 = if_else(Vigueur %in% c(1,2,3,4), Espece %in% Espece_specifiques_1_4, TRUE),
      condition_4 = if_else(Vigueur %in% c(5,6), Espece %in% Espece_specifiques_5_6, TRUE),
      condition_5 = if_else(Vigueur == 3, DHPcm >= 23.1, TRUE)
    ) %>%
    summarize(all_conditions = all(condition_1 & condition_2 & condition_3 & condition_4 & condition_5))

  return(resultats$all_conditions)
}

valide_Sup_PE <- function(data){
  if(!"Sup_PE" %in% names(data)){
    return (FALSE)
  }
  if(any(is.na(data$Sup_PE)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique = n_distinct(Sup_PE) == 1 && all(Sup_PE >= 0.04 & Sup_PE <= 1)
    )
  return(all(resultats$valeur_unique))


}

valide_Sup_PE_gaules <- function(data){
  if(!"Sup_PE" %in% names(data)){
    return (FALSE)
  }
  if(any(is.na(data$Sup_PE)) ){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique = n_distinct(Sup_PE) == 1 && all(Sup_PE >= 0.004 & Sup_PE <= 1)
    )
  return(all(resultats$valeur_unique))

}

valide_Annee_Coupe <- function(data){

  if(!"Annee_Coupe" %in% names(data)){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique = if (all(is.na(Annee_Coupe))) {
        Data$ntrt == 0
      } else {
        n_distinct(Annee_Coupe) == 1 && all(Annee_Coupe >= 1900 & Annee_Coupe <= 2100)
      }
    )
  return(all(resultats$valeur_unique))
}


valide_Latitude <- function(data){

  if(!"Latitude" %in% names(data)){
    return (FALSE)
  }
  if(any(is.na(data$Latitude)) ){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique =  n_distinct(Latitude) == 1 && all(Latitude > 45 & Latitude < 48.5)

    )
  return(all(resultats$valeur_unique))

}

valide_Longitude <- function(data){

  if(!"Longitude" %in% names(data)){
    return (FALSE)
  }
  if(any(is.na(data$Longitude)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(Placette) %>%
    summarise(

      valeur_unique = n_distinct(Longitude) == 1 && all(Longitude > -79.5 & Longitude < -64.0)

    )
  return(all(resultats$valeur_unique))

}


valide_Altitude <- function(data){

  if(!"Altitude" %in% names(data)){
    return (FALSE)
  }

  if(any(is.na(data$Altitude)) ){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique = n_distinct(Altitude) == 1 && all(Altitude < 1000)
    )
  return(all(resultats$valeur_unique))

}

valide_Ptot <- function(data){
  if(!"Ptot" %in% names(data)|| any(is.na(data$Ptot))){
    return (FALSE)
  }
  if(any(is.na(data$Ptot)) ){
    return (FALSE)
  }

   resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique = n_distinct(Ptot) == 1 && all(Ptot < 2000)
    )
  return(all(resultats$valeur_unique))

}


valide_Tmoy <- function(data){
  if(!"Tmoy" %in% names(data)|| any(is.na(data$Tmoy))){
    return (FALSE)
  }

  if(any(is.na(data$Tmoy)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique = n_distinct(Tmoy) == 1 && all(Tmoy > 0 & Tmoy < 10 )

    )
  return(all(resultats$valeur_unique))

}


valide_Type_Eco <- function(data){

  if(!"Type_Eco" %in% names(data)){
    return (FALSE)
  }

  if(any(is.na(data$Type_Eco)) ){
    return (FALSE)
  }
  valeurs_autorisees<-c('FC10', 'FC11', 'FC12', 'FE10', 'FE11', 'FE12', 'FE13', 'FE15', 'FE16', 'FE20', 'FE21',
                        'FE22', 'FE23', 'FE24', 'FE25', 'FE26', 'FE28', 'FE30', 'FE31', 'FE32', 'FE32H', 'FE33',
                        'FE34', 'FE35', 'FE36', 'FE42', 'FE43', 'FE45', 'FE50', 'FE51', 'FE52', 'FE52P', 'FE53',
                        'FE60', 'FE61', 'FE62', 'FE62P', 'FE65', 'FE66', 'FO10', 'FO12', 'FO14', 'FO15', 'FO16',
                        'FO18', 'ME13', 'ME16', 'MF12', 'MF15', 'MF16', 'MF18', 'MJ10', 'MJ11', 'MJ12', 'MJ12P',
                        'MJ13', 'MJ14', 'MJ15', 'MJ15P', 'MJ16', 'MJ18', 'MJ20', 'MJ20P', 'MJ21', 'MJ22', 'MJ22P',
                        'MJ23', 'MJ24', 'MJ24P', 'MJ25', 'MJ25P', 'MJ26', 'MJ28', 'MS10', 'MS10P', 'MS11', 'MS12',
                        'MS13', 'MS14', 'MS15', 'MS16', 'MS18', 'MS20', 'MS20P', 'MS21', 'MS22', 'MS22F', 'MS22P',
                        'MS23', 'MS23F', 'MS24', 'MS25', 'MS25F', 'MS25P', 'MS25Q', 'MS25S', 'MS26', 'MS26F', 'MS40',
                        'MS42', 'MS43', 'MS60', 'MS61', 'MS62', 'MS62P', 'MS63', 'MS65', 'MS66', 'RB10', 'RB11', 'RB12',
                        'RB13', 'RB14', 'RB15', 'RB16', 'RB17', 'RB18', 'RB22', 'RB23', 'RB51', 'RB52', 'RB53', 'RB55',
                        'RB55Q', 'RB56', 'RC37', 'RC38', 'RC39', 'RE10', 'RE11', 'RE11V', 'RE12', 'RE12P', 'RE13', 'RE14',
                        'RE15', 'RE15P', 'RE15Q', 'RE15S', 'RE16', 'RE20', 'RE21', 'RE21P', 'RE21Q', 'RE21V', 'RE22', 'RE22M',
                        'RE22P', 'RE23', 'RE24', 'RE25', 'RE25M', 'RE25P', 'RE25Q', 'RE25S', 'RE26', 'RE26S', 'RE32', 'RE37',
                        'RE37P', 'RE38', 'RE39', 'RE40', 'RE42', 'RP10', 'RP10P', 'RP11', 'RP12', 'RP13', 'RP14', 'RP15',
                        'RS10', 'RS11', 'RS12', 'RS12P', 'RS13', 'RS14', 'RS15', 'RS15P', 'RS16', 'RS18', 'RS20', 'RS20M',
                        'RS20P', 'RS21', 'RS22', 'RS22M', 'RS22P', 'RS22S', 'RS23', 'RS23M', 'RS24', 'RS24V', 'RS25', 'RS25M',
                        'RS25P', 'RS25Q', 'RS25S', 'RS26', 'RS34', 'RS35', 'RS37', 'RS37P', 'RS38', 'RS39', 'RS40', 'RS42', 'RS50',
                        'RS51', 'RS52', 'RS53', 'RS54', 'RS55', 'RS56', 'RS75', 'RT10', 'RT11', 'RT12', 'RT12P', 'RT14', 'RT15', 'RT16')

  if(!all(data$Type_Eco %in% valeurs_autorisees)){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique = n_distinct(Type_Eco) == 1

)
  return(all(resultats$valeur_unique))

}


valide_Reg_Eco <- function(data){
  if(!"Reg_Eco" %in% names(data)){
    return (FALSE)
  }

  if(any(is.na(data$Reg_Eco)) ){
    return (FALSE)
  }

  valeurs_autorisees<-c("1a", "2a", "2b", "2c", "3a", "3b", "3c", "3d", "4a", "4b", "4c", "4d", "4e", "4f", "4g",
                        "4h", "DU", "SV")

  if(!all(data$Reg_Eco %in% valeurs_autorisees)){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique = n_distinct(Reg_Eco) == 1

    )
  return(all(resultats$valeur_unique))
}

valide_MSCR <- function(data){

  if (!all(c("Vigueur", "MSCR") %in% names(data))) {
    return(FALSE)
  }
  valeurs_autorisees<-c('M', 'S', 'C', 'R','MS','CR')


  resultats <- data %>%
    mutate(
      condition_respectee = (MSCR %in% valeurs_autorisees | (is.na(MSCR) & Vigueur %in% c(1,2,3,4,5,6)))
    )

  return(all(resultats$condition_respectee))
}


valide_ABCD <- function(data) {

  if (!all(c("ABCD", "Espece", "DHPcm") %in% names(data))) {
    return(FALSE)
  }


  Espece_specifiques <- c("BOG", "BOJ", "BOP", "CAC", "CAF", "CAR", "CEO", "CET", "CHB", "CHE", "CHG",
                          "CHR", "ERA", "ERG", "ERN", "ERP", "ERR", "ERS", "FRA", "FRN", "FRP", "HEG",
                          "JUV", "MAS", "NOC", "ORA", "ORR", "ORT", "OSV", "PEB", "PED", "PEG", "PEH",
                          "PET", "PRP", "SAL", "SOA", "SOD", "TIL", "AME", "AUR", "ERE", "TIA", "CEP")
  valeurs_autorisees <- c("A", "B", "C", "D", NA)


  resultats <- data %>%
    mutate(
      condition0 = ABCD %in% valeurs_autorisees,
      condition1 = if_else(!Espece %in% Espece_specifiques, is.na(ABCD), TRUE),
      condition2 = if_else(DHPcm < 23.1, is.na(ABCD), TRUE),
      condition3 = if_else(DHPcm >= 23.1 & DHPcm <= 33.0, ABCD %in% c("C", "D", NA), TRUE),
      condition4 = if_else(DHPcm > 33.0 & DHPcm <= 39.0, ABCD %in% c("B", "C", "D", NA), TRUE)
    ) %>%
    summarize(all_conditions = all(condition0 & condition1 & condition2 & condition3 & condition4))

  return(resultats$all_conditions)
}


valide_Pente <- function(data){
  if(!"Pente" %in% names(data)){
    return (FALSE)
  }
  if(any(is.na(data$Pente)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(Placette) %>%
    summarise(

      valeur_unique = n_distinct(Pente) == 1 && all(Pente >= 0 & Pente <= 100)
    )
  return(all(resultats$valeur_unique))
}

valide_GrwDays <- function(data){
  if(!"GrwDays" %in% names(data)|| any(is.na(data$GrwDays))){
    return (FALSE)
  }
  if(any(is.na(data$GrwDays)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique = n_distinct(GrwDays) == 1 && all(GrwDays >=1 & GrwDays <365 )

    )
  return(all(resultats$valeur_unique))

}

valide_ntrt <- function(data){
  if(!"ntrt" %in% names(data)){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      valeur_unique = n_distinct(ntrt) == 1 && all(ntrt %in% c(0,1,2))

    )
  return(all(resultats$valeur_unique))


}


verifier_arbre_uniques_par_placette <- function(data) {

  if(!"Placette" %in% names(data)){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    summarise(
      tous_uniques = n_distinct(NoArbre) == n(),
      .groups = 'drop'
    )

  return(all(resultats$tous_uniques))

}


