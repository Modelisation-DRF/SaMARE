#' Répertoire des erreurs dans le fichier des arbres
#'
#' La fonction \code{valide_data} vérifie la validité des données dans un dataframe représentant
#' le fichier des arbres et retourne une liste des erreurs trouvées.
#'
#' @param data Un dataframe représentant le fichier des arbres.
#'
#' @return Une liste des erreurs trouvées dans le fichier des arbres.
#'
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe `data` représentant le fichier des arbres
#'
#' # Appel de la fonction
#' erreurs <- valide_data(data)
#'
#' # La fonction retournera une liste des erreurs trouvées dans le dataframe `data`
#' }
#'
#' @export

valide_data <- function(data) {
  data <- renommer_les_colonnes(data)
  validations <- list(
    valide_espece = "Code d'essence non valide",
    valide_Etat = "Code d'état non valide",
    valide_DHPcm = "Valeur de DHP non permise",
    valide_Vigueur = "Code de vigueur non permis",
    valide_Sup_PE = "Superficie de la placette en ha non valide",
    valide_Nombre = "valeur de nombre null",
    valide_Annee_Coupe = "Année de coupe non valide",
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

#' Répertoire des erreurs dans le fichier des gaules
#'
#' La fonction \code{valide_data_gaules} vérifie la validité des données dans un dataframe représentant
#' le fichier des gaules et retourne une liste des erreurs trouvées.
#'
#' @param data Un dataframe représentant le fichier des gaules.
#'
#' @return Une liste des erreurs trouvées dans le fichier des gaules.
#'
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe `data` représentant le fichier des gaules
#'
#' # Appel de la fonction
#' erreurs <- valide_data_gaules(data)
#'
#' # La fonction retournera une liste des erreurs trouvées dans le dataframe `data`
#' }
#'
#' @export

valide_data_gaules <- function(data ) {

  data<- renommer_les_colonnes_gaules(data)

  validations <- list(
    valide_espece = "Code d'essence non valide",
    valide_DHPcm_gaules = "Valeur de DHP non permise",
    valide_Sup_PE_gaules = "Superficie de la placette en ha non valide",
    valide_Nombre_gaules = "valeur de nombre null"
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


#' Fonction pour vérifier que les valeurs saisies dans la colonne 'nombre' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'  # Exemple avec un dataframe valide
#' valide_Nombre(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Nombre' manquante)
#' valide_Nombre(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs négatives)
#' valide_Nombre(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Nombre(data) # Devrait retourner FALSE

valide_Nombre <- function(data){

  if(!"Nombre" %in% names(data)){
    return (FALSE)
  }
  if(length(data$Nombre) == 0){
    return(FALSE)
  }
  if(any(is.na(data$Nombre))){
    return (FALSE)
  }


  return(all(data$Nombre>0))

}


#' Cette fonction vérifie si la colonne 'Nombre' dans le fichier de données contient des valeurs valides pour chaque 'Placette'.
#' Elle s'assure que la colonne existe, qu'elle n'est pas vide, qu'elle ne contient pas de valeurs manquantes (NA), et que la somme des valeurs pour chaque 'Placette' est supérieure à zéro.
#'
#' @param data Un dataframe contenant les données des arbres avec les colonnes 'Nombre' et 'Placette'.
#' @return Retourne TRUE si les valeurs dans la colonne 'Nombre' sont valides pour chaque 'Placette', FALSE sinon.
#' @examples
#'
#' # Exemple avec un dataframe valide
#' valide_Nombre_gaules(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Nombre' manquante)
#' valide_Nombre_gaules(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs négatives)
#' valide_Nombre_gaules(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Nombre_gaules(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (somme de 'Nombre' par 'Placette' égale à zéro)
#' valide_Nombre_gaules(data) # Devrait retourner FALSE

valide_Nombre_gaules <- function(data){

  if(!"Nombre" %in% names(data)){
    return (FALSE)
  }
  if(length(data$Nombre) == 0){
    return(FALSE)
  }
  if(any(is.na(data$Nombre))){
    return (FALSE)
  }



  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      somme = sum(Nombre) > 0
    )

  return(all(resultats$somme))

}



#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Espece' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_espece(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Espece' manquante)
#' valide_espece(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées)
#' valide_espece(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_espece(data) # Devrait retourner FALSE

valide_espece <- function(data){

  if(!"Espece" %in% names(data)){
    return (FALSE)
  }
  if(length(data$Espece) == 0){
    return(FALSE)
  }
  if(any(is.na(data$Espece)) ){
    return (FALSE)
  }

  valeurs_autorisees <- c("AME", "AUR", "ERE", "ERG", "ERP", "MAS", "OSV", "PRP", "SAL",
                          "SOA", "SOD", "BOJ", "EPB", "EPN", "EPO", "EPR", "ERR", "ERA",
                          "ERN", "ERS", "CAC", "CAF", "CAR", "CEO", "CET", "CHB", "CHE",
                          "CHG", "CHR", "FRA", "FRN", "FRP", "NOC", "ORA", "ORR", "ORT",
                          "TIL", "AME", "AUR", "ERE", "ERG", "ERP", "MAS", "OSV", "PRP",
                          "SAL", "SOA", "SOD", "BOJ", "EPB", "EPN", "EPO", "EPR", "ERR",
                          "ERA", "ERN", "ERS", "CAC", "CAF", "CAR", "CEO", "CET", "CHB",
                          "CHE", "CHG", "CHR", "FRA", "FRN", "FRP", "NOC", "ORA", "ORR",
                          "ORT", "TIL", "BOG", "BOP", "PEB", "PED", "PEG", "PEH", "PET",
                          "HEG", "JUV", "MEJ", "MEL", "MEU", "PIB", "PID", "PIG", "PIR",
                          "PIS", "PRU", "THO", "SAB")


  return(all(data$Espece %in% valeurs_autorisees))
}


#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Etat' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#' # Exemple avec un dataframe valide
#' valide_Etat(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Etat' manquante)
#' valide_Etat(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées)
#' valide_Etat(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Etat(data) # Devrait retourner FALSE

valide_Etat <- function(data){

  if(!"Etat" %in% names(data)){
    return (FALSE)
  }
  if(length(data$Etat) == 0){
    return(FALSE)
  }
  if(any(is.na(data$Etat)) ){
    return (FALSE)
  }

  valeurs_autorisees<-c(10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25, 26, 27,
                        28, 29, 30, 32, 34, 36, 40, 42, 43, 44, 46, 50, 52, 54, 56 )

  return(all(data$Etat %in% valeurs_autorisees))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'DHPcm' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_DHPcm(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'DHPcm' manquante)
#' valide_DHPcm(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs supérieures à 160 cm)
#' valide_DHPcm(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_DHPcm(data) # Devrait retourner FALSE

valide_DHPcm <- function(data){

  if(!"DHPcm" %in% names(data)){
    return (FALSE)
  }
  if( length(data$DHPcm) == 0){
    return(FALSE)
  }

  if(any(is.na(data$DHPcm)) ){
    return (FALSE)
  }
  return(all(data$DHPcm <= 160 ))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'DHPcm' sont correctes.
#' @param data fichier des gaules
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_DHPcm_gaules(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'DHPcm' manquante)
#' valide_DHPcm_gaules(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs hors de l'intervalle)
#' valide_DHPcm_gaules(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_DHPcm_gaules(data) # Devrait retourner FALSE

valide_DHPcm_gaules <- function(data){

  if(!"DHPcm" %in% names(data)){
    return (FALSE)
  }

  if(length(data$DHPcm) == 0){
    return(FALSE)
  }
  if(any(is.na(data$DHPcm)) ){
    return (FALSE)
  }
  return(all(between(data$DHPcm, 1.0, 9.0) ))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Vigueur' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_Vigueur(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées dans 'Vigueur')
#' valide_Vigueur(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA sans MSCR correct)
#' valide_Vigueur(data) # Devrait retourner FALSE

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
    reframe(all_conditions = all(condition_1 & condition_2 & condition_3 & condition_4 & condition_5))

  return(resultats$all_conditions)
}


#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Sup_PE' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#' # Exemple avec un dataframe valide
#' valide_Sup_PE(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Sup_PE' manquante)
#' valide_Sup_PE(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Sup_PE(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Sup_PE(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Sup_PE' par 'Placette')
#' valide_Sup_PE(data) # Devrait retourner FALSE

valide_Sup_PE <- function(data){
  if(!all(c("Placette", "Sup_PE") %in% names(data))){
    return (FALSE)
  }

  if(length(data$Sup_PE) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Sup_PE)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      valeur_unique = n_distinct(Sup_PE) == 1 && all(Sup_PE >= 0.04 & Sup_PE <= 1)
    )
  return(all(resultats$valeur_unique))


}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Sup_PE' sont correctes.
#' @param data fichier des gaules
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_Sup_PE_gaules(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Sup_PE' manquante)
#' valide_Sup_PE_gaules(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Sup_PE_gaules(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Sup_PE_gaules(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Sup_PE' par 'Placette')
#' valide_Sup_PE_gaules(data) # Devrait retourner FALSE

valide_Sup_PE_gaules <- function(data){
  if(!all(c("Placette", "Sup_PE") %in% names(data))){
    return (FALSE)
  }
  if(length(data$Sup_PE) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Sup_PE)) ){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      valeur_unique = n_distinct(Sup_PE) == 1 && all(Sup_PE >= 0.004 & Sup_PE <= 1)
    )
  return(all(resultats$valeur_unique))

}


#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Annee_Coupe' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_Annee_Coupe(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Annee_Coupe' manquante)
#' valide_Annee_Coupe(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Annee_Coupe(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe valide (toutes les valeurs de 'Annee_Coupe' sont NA pour une 'Placette')
#' valide_Annee_Coupe(data) # Devrait retourner TRUE

valide_Annee_Coupe <- function(data){

  if(!all(c("Placette", "Annee_Coupe") %in% names(data))){
    return (FALSE)
  }


  if(length(data$Placette) == 0){
    return(FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      valeur_unique = if (all(is.na(Annee_Coupe))) {
        ntrt == 0
      } else {
        n_distinct(Annee_Coupe) == 1 && all(Annee_Coupe >= 1900 & Annee_Coupe <= 2100)
      }
    )
  return(all(resultats$valeur_unique))
}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Latitude' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_Latitude(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Latitude' manquante)
#' valide_Latitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Latitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Latitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Latitude' par 'Placette')
#' valide_Latitude(data) # Devrait retourner FALSE

valide_Latitude <- function(data){

  if(!all(c("Placette", "Latitude") %in% names(data))){
    return (FALSE)
  }

  if(length(data$Latitude) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Latitude)) ){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      valeur_unique =  n_distinct(Latitude) == 1 && all(Latitude > 45 & Latitude < 48.5)

    )
  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Longitude' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_Longitude(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Longitude' manquante)
#' valide_Longitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Longitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Longitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Longitude' par 'Placette')
#' valide_Longitude(data) # Devrait retourner FALSE

valide_Longitude <- function(data){

  if(!all(c("Placette", "Longitude") %in% names(data))){
    return (FALSE)
  }

  if(length(data$Longitude) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Longitude)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(Placette) %>%
    reframe(

      valeur_unique = n_distinct(Longitude) == 1 && all(Longitude > -79.5 & Longitude < -64.0)

    )
  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Altitude' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_Altitude(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Altitude' manquante)
#' valide_Altitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Altitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Altitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Altitude' par 'Placette')
#' valide_Altitude(data) # Devrait retourner FALSE

valide_Altitude <- function(data){

  if(!all(c("Placette", "Altitude") %in% names(data))){
    return (FALSE)
  }
  if(length(data$Altitude) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Altitude))){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      valeur_unique = n_distinct(Altitude) == 1 && all(Altitude < 1000)
    )
  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Ptot' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#' # Exemple avec un dataframe valide
#' valide_Ptot(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Ptot' manquante)
#' valide_Ptot(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Ptot(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Ptot(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Ptot' par 'Placette')
#' valide_Ptot(data) # Devrait retourner FALSE
#'
valide_Ptot <- function(data){
  if(!all(c("Placette", "Ptot") %in% names(data))|| any(is.na(data$Ptot))){
    return (FALSE)
  }


  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      valeur_unique = n_distinct(Ptot) == 1 && all(Ptot < 2000)
    )
  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Tmoy' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#' # Exemple avec un dataframe valide
#' valide_Tmoy(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Tmoy' manquante)
#' valide_Tmoy(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Tmoy(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Tmoy(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Tmoy' par 'Placette')
#' valide_Tmoy(data) # Devrait retourner FALSE
#'
valide_Tmoy <- function(data){
  if(!all(c("Placette", "Tmoy") %in% names(data))|| any(is.na(data$Tmoy))){
    return (FALSE)
  }

  if(any(is.na(data$Tmoy)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      valeur_unique = n_distinct(Tmoy) == 1 && all(Tmoy > 0 & Tmoy < 10 )

    )
  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Type_Eco' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'# Exemple avec un dataframe valide
#' valide_Type_Eco(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Type_Eco' manquante)
#' valide_Type_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées)
#' valide_Type_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Type_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Type_Eco' par 'Placette')
#' valide_Type_Eco(data) # Devrait retourner FALSE
#'
valide_Type_Eco <- function(data){

  if (!all(c("Placette", "Type_Eco") %in% names(data))) {
    return(FALSE)
  }
  if(length(data$Type_Eco) == 0){
    return(FALSE)
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
    reframe(
      valeur_unique = n_distinct(Type_Eco) == 1

    )
  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Reg_Eco' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'# Exemple avec un dataframe valide
#' valide_Reg_Eco(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Reg_Eco' manquante)
#' valide_Reg_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées)
#' valide_Reg_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Reg_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Reg_Eco' par 'Placette')
#' valide_Reg_Eco(data) # Devrait retourner FALSE
#'
valide_Reg_Eco <- function(data){
  if (!all(c("Placette", "Reg_Eco") %in% names(data))) {
    return(FALSE)
  }
  if(length(data$Reg_Eco) == 0){
    return(FALSE)
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
    reframe(
      valeur_unique = n_distinct(Reg_Eco) == 1

    )
  return(all(resultats$valeur_unique))
}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'MSCR' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#' # Exemple avec un dataframe valide
#' valide_MSCR(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'MSCR' manquante)
#' valide_MSCR(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées dans 'MSCR')
#' valide_MSCR(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA dans 'MSCR' sans 'Vigueur' appropriée)
#' valide_MSCR(data) # Devrait retourner FALSE
#'
valide_MSCR <- function(data){

  if (!all(c("Vigueur", "MSCR") %in% names(data))) {
    return(FALSE)
  }
  if(length(data$Vigueur) == 0){
    return(FALSE)
  }
  if(length(data$MSCR) == 0){
    return(FALSE)
  }
  valeurs_autorisees<-c('M', 'S', 'C', 'R','MS','CR')


  resultats <- data %>%
    mutate(
      condition_respectee = (MSCR %in% valeurs_autorisees | (is.na(MSCR) & Vigueur %in% c(1,2,3,4,5,6)))
    )

  return(all(resultats$condition_respectee))
}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'ABCD' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_ABCD(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'ABCD' manquante)
#' valide_ABCD(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées dans 'ABCD')
#' valide_ABCD(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA dans 'DHPcm' ou 'Espece')
#' valide_ABCD(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (conditions spécifiques non respectées)
#' valide_ABCD(data) # Devrait retourner FALSE
#'
valide_ABCD <- function(data) {

  if (!all(c("ABCD", "Espece", "DHPcm") %in% names(data))) {
    return(FALSE)
  }

  if(any(is.na(data$DHPcm)) ){
    return (FALSE)
  }
  if(any(is.na(data$Espece)) ){
    return (FALSE)
  }

  Espece_specifiques <- c("BOG", "BOJ", "BOP", "CAC", "CAF", "CAR", "CEO", "CET", "CHB", "CHE", "CHG",
                          "CHR", "ERA", "ERG", "ERN", "ERP", "ERR", "ERS", "FRA", "FRN", "FRP", "HEG",
                          "JUV", "MAS", "NOC", "ORA", "ORR", "ORT", "OSV", "PEB", "PED", "PEG", "PEH",
                          "PET", "PRP", "SAL", "SOA", "SOD", "TIL", "AME", "AUR", "ERE", "TIA", "CEP")
  valeurs_autorisees <- c("A", "B", "C", "D", NA,"")


  resultats <- data %>%
    mutate(
      condition0 = ABCD %in% valeurs_autorisees,
      condition1 = ifelse(!Espece %in% Espece_specifiques, is.na(ABCD) | ABCD == "", TRUE),
      condition2 = ifelse(DHPcm < 23.1, is.na(ABCD) | ABCD == "", TRUE),
      condition3 = ifelse(DHPcm >= 23.1 & DHPcm <= 33.0, ABCD %in% c("C", "D", NA, ""), TRUE),
      condition4 = ifelse(DHPcm > 33.0 & DHPcm <= 39.0, ABCD %in% c("B", "C", "D", NA, ""), TRUE)
    ) %>%
    reframe(all_conditions = all(condition0 & condition1 & condition2 & condition3 & condition4))

  return(resultats$all_conditions)
}


#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Pente' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'# Exemple avec un dataframe valide
#' valide_Pente(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Pente' manquante)
#' valide_Pente(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Pente(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Pente(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Pente' par 'Placette')
#' valide_Pente(data) # Devrait retourner FALSE
#'
valide_Pente <- function(data){
  if (!all(c("Placette", "Pente") %in% names(data))) {
    return(FALSE)
  }
  if(length(data$Pente) == 0){
    return(FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    reframe(

      valeur_unique = n_distinct(Pente) == 1 && (all(Pente >= 0 & Pente <= 100)|| is.na(Pente))
    )
  return(all(resultats$valeur_unique))
}


#' Fonction pour vérifier que les valeurs saisies dans la colonne 'GrwDays' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#' # Exemple avec un dataframe valide
#' valide_GrwDays(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'GrwDays' manquante)
#' valide_GrwDays(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_GrwDays(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_GrwDays(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'GrwDays' par 'Placette')
#' valide_GrwDays(data) # Devrait retourner FALSE
#'
valide_GrwDays <- function(data){
  if(!all(c("GrwDays","Placette") %in% names(data))|| any(is.na(data$GrwDays))){
    return (FALSE)
  }
  if(any(is.na(data$GrwDays)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      valeur_unique = n_distinct(GrwDays) == 1 && all(GrwDays >=1 & GrwDays <365 )

    )
  return(all(resultats$valeur_unique))

}


#' Fonction pour vérifier que les valeurs saisies dans la colonne 'ntrt' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#' # Exemple avec un dataframe valide
#' valide_ntrt(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'ntrt' manquante)
#' valide_ntrt(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées dans 'ntrt')
#' valide_ntrt(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'ntrt' par 'Placette')
#' valide_ntrt(data) # Devrait retourner FALSE
#'
valide_ntrt <- function(data){
  if (!all(c("Placette", "ntrt") %in% names(data))) {
    return(FALSE)
  }
  if(length(data$ntrt) == 0){
    return(FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      valeur_unique = n_distinct(ntrt) == 1 && all(ntrt %in% c(0,1,2))

    )
  return(all(resultats$valeur_unique))


}


#' Fonction pour vérifier que chaque arbres est unique dans chaque placette
#' @param data fichier des arbres
#' @return Retourne vrai ou faux s'il y a des arbres qui se répètent.
#' @examples
#'  # Exemple avec un dataframe valide
#' verifier_arbre_uniques_par_placette(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'NoArbre' manquante)
#' verifier_arbre_uniques_par_placette(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA dans 'Placette' ou 'NoArbre')
#' verifier_arbre_uniques_par_placette(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (arbres non uniques dans une placette)
#' verifier_arbre_uniques_par_placette(data) # Devrait retourner FALSE
#'
verifier_arbre_uniques_par_placette <- function(data) {


  if (!all(c("Placette", "NoArbre") %in% names(data))) {
    return(FALSE)
  }
if(any(is.na(data$Placette))){
  return(FALSE)
}
  if(any(is.na(data$NoArbre))){
    return(FALSE)
  }
  if(length(data$Placette) == 0){
    return(FALSE)
  }
  if(length(data$NoArbre) == 0){
    return(FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      tous_uniques = n_distinct(NoArbre) == n(),
      .groups = 'drop'
    )

  return(all(resultats$tous_uniques))

}


#' Valide la correspondance des placettes entre les arbres et les gaules
#'
#' Cette fonction vérifie que toutes les placettes présentes dans le fichier des arbres sont également présentes dans le fichier des gaules.
#'
#' @param data_arbre Un data.frame contenant les données des arbres.
#' @param data_gaules Un data.frame contenant les données des gaules.
#' @return Une liste de messages d'erreurs. Si aucune erreur n'est trouvée, une liste vide est retournée.
#' @examples
#' data_arbre <- data.frame(Placette = c("TEM23APC5001", "TEM23APC5002", "TEM23APC5003"))
#' data_gaules <- data.frame(Placette = c("TEM23APC5001", "TEM23APC5002"))
#' valide_placette_gaules(data_arbre, data_gaules)
#' # Les erreurs seront retournées comme ceci
#' # 'Les placette suivantes n'ont pas de gaules : TEM23APC5003'
valide_placette_gaules <-function (data_arbre , data_gaules){

  data_arbre <- renommer_les_colonnes(data_arbre)
  data_gaules<- renommer_les_colonnes_gaules(data_gaules)

  erreurs <-list()

  if(!"Placette" %in% names(data_arbre) ||!"Placette" %in% names(data_gaules) ){
    erreurs<-paste("La colonne 'Placette' est manquante dans le fichier des gaules ou dans le fichier des arbres.")

    return(erreurs)
  }else  if(length(data_arbre$Placette) == 0|| length(data_gaules$Placette) == 0){
    erreurs<-paste("La colonne 'Placette' est vide dans le fichier des gaules ou dans le fichier des arbres.")
    return(erreurs)
  }


  pacette_arbre <- unique(data_arbre$Placette)
  placette_gaules <- unique(data_gaules$Placette)

  diff_placette <- setdiff(pacette_arbre, placette_gaules)

  erreurs <-list()

  if (!length(diff_placette) == 0) {

    erreurs<-paste("Les placettes suivantes n'ont pas de gaules : ", paste(diff_placette, collapse = ", "))
  }


  return(erreurs)
}
