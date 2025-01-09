
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
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe data avec les colonnes suivantes :
#' # "Placette", "NoArbre", "Espece", "Etat", "DHPcm", "Vigueur", "Nombre"
#'
#' # Appel de la fonction
#' # noms_absents <- trouver_noms_absents(data)
#'
#' # Si les colonnes "Sup_PE", "Annee_Coupe", "Latitude", "Longitude", "Altitude", "Pente",
#' # "Reg_Eco", "Type_Eco", "MSCR", "ntrt", "ABCD" sont absentes, la fonction retournera :
#' # [1] "sup_pe" "annee_coupe" "latitude" "longitude" "altitude" "pente"
#' #     "reg_eco" "type_eco" "mscr" "ntrt" "abcd"
#' }
#'
#' @export
#'
trouver_noms_absents <- function(Data) {

  ColOrdre<-c("Placette","NoArbre","Espece","Etat","DHPcm","Vigueur","Nombre",
              "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Pente","Reg_Eco","Type_Eco", "MSCR","ntrt","ABCD")

  names(Data) <- tolower(names(Data))

  colone_minuscule <- lapply(ColOrdre, tolower)

  noms_absents <- setdiff(colone_minuscule, names(Data))

  return(noms_absents)

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
#' La fonction \code{renommer_les_colonnes} renomme les colonnes d'un dataframe
#'
#' @param data Un dataframe, représentant le fichier des arbres, dont les colonnes doivent être renommées et réorganisées.
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
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe data avec les colonnes suivantes :
#' # "placette", "noarbre", "espece", "etat", "dhpcm", "vigueur", "nombre",
#' # "sup_pe", "annee_coupe", "latitude", "longitude", "altitude", "pente",
#' # "ptot", "tmoy", "grw_days", "reg_eco", "type_eco", "mscr", "ntrt", "abcd"
#'
#' # Appel de la fonction
#' # data_renomme <- renommer_les_colonnes(data)
#'
#' # Le dataframe data_renomme aura les colonnes renommées et réorganisées selon ColOrdre.
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
#' @param data Un dataframe, représentant le fichier des gaules, dont les colonnes doivent être renommées et réorganisées.
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
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe data avec les colonnes suivantes :
#' # "placette", "espece", "grespece", "dhpcm", "nombre", "sup_pe"
#'
#' # Appel de la fonction
#' # data_renomme <- renommer_les_colonnes_gaules(data)
#'
#' # Le dataframe data_renomme aura les colonnes renommées et réorganisées selon ColOrdre.
#' }
#' @export


renommer_les_colonnes_gaules <- function(data){

  ColOrdre<-c("Placette","Espece","GrEspece","DHPcm","Nombre","Sup_PE")

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


