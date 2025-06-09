#' Vérifier la présence des colonnes obligatoires dans le fichier d'arbres
#'
#' La fonction \code{trouver_noms_absents} vérifie si toutes les colonnes obligatoires
#' sont présentes dans un dataframe représentant le fichier d'arbres. Elle retourne une liste
#' des noms des colonnes manquantes, le cas échéant.
#'
#' @param Data Un dataframe représentant le fichier d'arbres.
#'
#' @return Une liste des noms des colonnes manquantes.
#'
#' @export
#'
trouver_noms_absents <- function(Data) {

  ColOrdre<-c("Placette","NoArbre","Espece","Etat","DHPcm","Vigueur","Nombre",
              "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Reg_Eco","Type_Eco", "MSCR","ntrt","ABCD")

  names(Data) <- tolower(names(Data))

  colone_minuscule <- lapply(ColOrdre, tolower)

  noms_absents <- setdiff(colone_minuscule, names(Data))

  return(noms_absents)

}


#' Fonction qui vérifie si les variables climatiques sont présentes. Si elle sont absentes, elles
#' sont estimée à l'aide du package extract_map.
#'
#'@param data Un dataframe contenant la liste d'arbres à simuler
#'
#'@return Retourne la liste d'arbre initiale avec les données météo ajoutée si
#'        elles sont absentes
#'
#'@export
verifier_variable_meteo <- function(data){

  select=dplyr::select

  data <- data %>%
    rename(id_pe = Placette, latitude = Latitude, longitude = Longitude)

  mes_variables <- c('GrwDays', 'Ptot', 'Tmoy')
  #fonctions_validation <- list(valide_GrwDays, valide_Ptot, valide_Tmoy)
  noms_remplacement <- c("growingseasonlength", "totalprecipitation", "tmean")

  # for (i in seq_along(mes_variables)) {
  #   if (mes_variables[i] %in% names(data) && !fonctions_validation[[i]](data)) {
  #     data <- select(data, -!!rlang::sym(mes_variables[i])) # on enlève la variable i si elle est présente et contient des manquants ou des valeurs hors normes
  #   }
  # }
  # variables_presentes <- intersect(mes_variables, names(data))
  # for (col_names in variables_presentes) {
  #   if (!length(unique(data[[col_names]])) == 1) {
  #     data <- select(data, -!!rlang::sym(col_names))
  #   }
  #
  # }

  map_noms_variables <- c(GrwDays = "growingseasonlength",
                          Ptot = "totalprecipitation",
                          Tmoy = "tmean")

  variables_non_trouvees <- setdiff(mes_variables, names(data))

  if(!is_empty(variables_non_trouvees)){
    variables_a_extraire <- map_noms_variables[variables_non_trouvees]

    data <- ExtractMap::extract_map_plot(file=data, liste_raster="cartes_climat", variable=variables_a_extraire)

    if('tmean' %in% variables_a_extraire) {
      data <- rename(data, Tmoy = tmean)
    }

    if('totalprecipitation' %in% variables_a_extraire) {
      data <- rename(data, Ptot = totalprecipitation)
    }

    if('growingseasonlength' %in% variables_a_extraire) {
      data <- rename(data, GrwDays = growingseasonlength)
    }
  }

  data <- data %>% rename(Placette=id_pe, Latitude = latitude, Longitude = longitude )

  return (data)
}



#' Fonction qui vérifie si la pente est dans le fichier.
#' Si elle est absente, elle est estimée à l'aide du package ExtractMap
#'
#'@param data Un dataframe contenant la liste d'arbres à simuler.
#'
#'@return Retourne la liste d'arbres initiale avec les données de station ajoutées si
#'        elles sont absentes.
#'
#'@export
verifier_variable_station <- function(data){

  select=dplyr::select


  mes_variables <- c("Pente")
  #fonctions_validation <- list(valide_Pente, valide_Exposition)

  data <- data %>%
    rename(id_pe = Placette, latitude = Latitude, longitude = Longitude)

  # for (i in seq_along(mes_variables)) {
  #   if (mes_variables[i] %in% names(data) && !fonctions_validation[[i]](data)) {
  #     data <- select(data, -!!rlang::sym(mes_variables[i]))
  #   }
  # }
  #
  # variables_presentes <- intersect(mes_variables, names(data))
  # for (col_names in variables_presentes) {
  #   if (!length(unique(data[[col_names]])) == 1) {
  #     data <- select(data, -!!rlang::sym(col_names))
  #   }
  # }

  map_noms_variables <- c(Pente = "pente")

  variables_non_trouvees <- setdiff(mes_variables, names(data))


  if(!is_empty(variables_non_trouvees)){
    variables_a_extraire <- map_noms_variables[variables_non_trouvees]

    data <- ExtractMap::extract_map_plot(file=data, liste_raster="cartes_station", variable=variables_a_extraire)


    if('pente' %in% variables_a_extraire) {
      data <- rename(data, Pente = pente)
    }

  }
  data <- data %>% rename(Placette = id_pe, Latitude = latitude, Longitude = longitude )

  return (data)

}




#' Vérifier la présence des colonnes obligatoires dans le fichier des gaules
#'
#' La fonction \code{trouver_noms_absents_gaules} vérifie si toutes les colonnes obligatoires
#' sont présentes dans un dataframe représentant le fichier des gaules. Elle retourne une liste
#' des noms des colonnes manquantes, le cas échéant.
#'
#' @param Data Un dataframe représentant le fichier des gaules.
#'
#' @return Une liste des noms des colonnes manquantes.
#'
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe data avec les colonnes suivantes :
#' # "Placette", "Espece", "DHPcm", "Nombre"
#'
#' # Appel de la fonction
#' # noms_absents <- trouver_noms_absents_gaules(data)
#'
#' # Si la colonne "Sup_PE" est absente, la fonction retournera :
#' # [1] "sup_pe"
#' }
#'
#' @export
trouver_noms_absents_gaules <- function(Data) {

  ColOrdre<-c("Placette","Espece","DHPcm","Nombre","Sup_PE")

  names(Data) <- tolower(names(Data))

  colone_minuscule <- lapply(ColOrdre, tolower)

  noms_absents <- setdiff(colone_minuscule, names(Data))

  return(noms_absents)

}



#' Renommer les colonnes  du fichier des arbres
#'
#' La fonction \code{renommer_les_colonnes} renomme les colonnes d'un dataframe.
#' Les noms doivent être ceux attendus avec seulement des différences au niveau des minuscules et majuscules.
#'
#' @param data Un dataframe, représentant le fichier des arbres, dont les colonnes doivent être renommées
#'
#'
#' @details
#' La fonction suit les étapes suivantes :
#' \itemize{
#'   \item Définir un vecteur \code{ColOrdre} contenant les noms des colonnes souhaitées dans l'ordre désiré.
#'   \item Convertir les noms des colonnes existantes du dataframe en minuscules pour faciliter la correspondance.
#'   \item Convertir les noms des colonnes souhaitées en minuscules.
#'   \item Pour chaque nom de colonne souhaité, chercher sa correspondance parmi les noms des colonnes existantes.
#'   \item Si une correspondance est trouvée, renommer la colonne existante avec le nom souhaité.
#'   \item Retourner le dataframe avec les noms de colonnes mis à jour.
#' }
#'
#' @export
renommer_les_colonnes <- function(data){

  ColOrdre<-c("Placette","NoArbre","Espece","Etat","DHPcm","Vigueur","Nombre",
              "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Pente","Ptot","Tmoy",
              "GrwDays","Reg_Eco","Type_Eco", "MSCR","ntrt","ABCD")

  noms_colonnes_existants <- tolower(names(data))
  noms_colonnes_desires <- tolower(ColOrdre)

  for (i in seq_along(noms_colonnes_desires)) {
    index_colonne <- match(noms_colonnes_desires[i], noms_colonnes_existants)
    if (!is.na(index_colonne)) {
      names(data)[index_colonne] <- ColOrdre[i]
    }
  }

  return(data)
}



#' Renommer les colonnes  du fichier des gaules
#'
#' La fonction \code{renommer_les_colonnes} renomme les colonnes d'un dataframe
#'
#' @param data Un dataframe, représentant le fichier des gaules, dont les colonnes doivent être renommées
#'
#' @details
#' La fonction suit les étapes suivantes :
#' \itemize{
#'   \item Définir un vecteur \code{ColOrdre} contenant les noms des colonnes souhaitées dans l'ordre désiré.
#'   \item Convertir les noms des colonnes existantes du dataframe en minuscules pour faciliter la correspondance.
#'   \item Convertir les noms des colonnes souhaitées en minuscules.
#'   \item Pour chaque nom de colonne souhaité, chercher sa correspondance parmi les noms des colonnes existantes.
#'   \item Si une correspondance est trouvée, renommer la colonne existante avec le nom souhaité.
#'   \item Retourner le dataframe avec les noms de colonnes mis à jour.
#' }
#'
#' @export
renommer_les_colonnes_gaules <- function(data){

  ColOrdre<-c("Placette","Espece","DHPcm","Nombre","Sup_PE")

  noms_colonnes_existants <- tolower(names(data))
  noms_colonnes_desires <- tolower(ColOrdre)

  for (i in seq_along(noms_colonnes_desires)) {
    index_colonne <- match(noms_colonnes_desires[i], noms_colonnes_existants)
    if (!is.na(index_colonne)) {
      names(data)[index_colonne] <- ColOrdre[i]
    }
  }

  return(data)
}


