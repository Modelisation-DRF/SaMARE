#' Fusion des sorties Sybille et Petro
#'
#' Cette fonction combine les résultats des fonctions SortieSybille et SortieBillonnage
#' pour produire une table fusionnée des données avec les volumes par grade.
#'
#' @param Data Un data.frame ou data.table contenant les données d'inventaire forestier avec les colonnes suivantes:
#'   \itemize{
#'     \item Veg_Pot - Code de végétation potentielle, ex: "MS2"
#'     \item PlacetteID - Identifiant de la placette
#'     \item DHPcm - Diamètre à hauteur de poitrine en cm
#'     \item Altitude - Altitude en mètres
#'     \item hauteur_pred - Hauteur prédite de l'arbre en mètres
#'     \item origTreeID - Numéro de l'arbre
#'     \item Espece - Code d'essence de l'arbre, ex: "SAB"
#'     \item Cl_Drai - Classe de drainage ex: "2"
#'     \item Etat - État de l'arbre ("vivant", "mort", "recrue")
#'     \item Nombre - Nombre d'arbres représentés par cette observation dans une placette de 400 m2
#'     \item sdom_bio - Sous-domaine bioclimatique. ex: "1", "2E", "4O"
#'   }
#' @param Type Le type de billonage à utiliser pour la fonction SortieBillonnage
#' @param dhs Hauteur de souche standard en mètres (point de départ des mesures), initialisée à 0.15(15 cm)
#' @param nom_grade1 Nom du premier type de bille.
#' @param nom_grade2 Nom du deuxième type de bille.(si besoin)
#' @param nom_grade3 Nom du troisième type de bille.(si besoin)
#' @param long_grade1 Longueur de la première bille en pieds(multiple de 2 pieds).
#' @param long_grade2 Longueur de la deuxième bille en pieds.(multiple de 2 pieds)(si besoin)
#' @param long_grade3 Longueur de la troisième bille en pieds.(multiple de 2 pieds)(si besoin)
#' @param diam_grade1 Diamètre minimal au fin bout de la première bille en cm.
#' @param diam_grade2 Diamètre minimal au fin bout de la deuxième bille en cm.(si besoin)
#' @param diam_grade3 Diamètre minimal au fin bout de la troisième bille en cm.(si besoin)
#' @param Simplifier Paramètre qui permet de décider si on prend toutes les années de simulation ou seulement la première et la dernière(False = Tout, True = 1er et dernier)
#'
#' @return Un data.table fusionné contenant toutes les colonnes de Samare et les colonnes pour le billonage (grade_type et vol_bille_dm3)
#'
#' @details
#' La fonction effectue les opérations suivantes:
#' \itemize{
#'   \item Appelle SortieSybille avec les paramètres de grades spécifiés
#'   \item Standardise les noms de colonnes de Sybille
#'   \item Appelle SortieBillonnage avec le type spécifié
#'   \item Transpose les données Petro pour avoir une ligne par coupe
#'   \item Fusionne les deux tables avec rbind
#'   \item Supprime les colonnes non nécessaires
#'   \item Remplace les valeurs NA de volume par 0.0
#'   \item Trie les données par PlacetteID, Annee, et origTreeID
#' }
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation basique
#' resultat <- SortieBillesFusion(mes_donnees, "TypeA", nom_grade1 = "Sciage long",  long_grade1 = 4, diam_grade1 = 8)
#'
#' # Avec paramètres de grades personnalisés
#' resultat <- SortieBillesFusion(
#'   mes_donnees,
#'   "TypeB",
#'   dhs = 0.20,
#'   nom_grade1 = "Sciage long",
#'   long_grade1 = 4,
#'   diam_grade1 = 8
#' )
#' }
#'
#' @seealso \code{\link{SortieSybille}}, \code{\link{SortieBillonnage}}
#' @export

#library(OutilsDRF)
SortieBillesFusion <- function(Data, Type, dhs = 0.15, nom_grade1 = NA, long_grade1 = NA, diam_grade1 = NA,
                               nom_grade2 = NA, long_grade2 = NA, diam_grade2 = NA,
                               nom_grade3 = NA, long_grade3 = NA, diam_grade3 = NA, Simplifier = FALSE) {

  setDT(Data)

  Data_Arbre <- SortieArbreSamare(Data, simplifier = Simplifier)
  setDT(Data_Arbre)

  # On obtient Petro et Sybille
  Petro <- SortieBillonnage(Data, Type)

  Sybille <- SortieSybille(Data, dhs, nom_grade1, long_grade1, diam_grade1, nom_grade2, long_grade2, diam_grade2,
                           nom_grade3, long_grade3, diam_grade3)

  Sybille <- Sybille[!is.na(grade_bille)]

  # On fusionne Petro et Sybille
  Fusion <- rbind(Petro, Sybille, fill = TRUE)
  setDT(Fusion)
  setorder(Fusion, PlacetteID, Annee, ArbreID)

  # Détecter les noms de colonnes
  colonnes_data_arbre <- colnames(Data_Arbre)

  col_placette <- if("PlacetteID" %in% colonnes_data_arbre) {
    "PlacetteID"
  } else if("id_pe" %in% colonnes_data_arbre) {
    "id_pe"
  } else {
    stop("Impossible de trouver la colonne PlacetteID/id_pe")
  }

  col_arbre <- if("ArbreID" %in% colonnes_data_arbre) {
    "ArbreID"
  } else if("no_arbre" %in% colonnes_data_arbre) {
    "no_arbre"
  } else {
    stop("Impossible de trouver la colonne ArbreID/no_arbre")
  }

  col_dhp <- if("DHPcm" %in% colonnes_data_arbre) {
    "DHPcm"
  } else if("DHP_Ae" %in% colonnes_data_arbre) {
    "DHP_Ae"
  } else {
    stop("Impossible de trouver la colonne DHPcm/DHP_Ae")
  }

  # SOLUTION 1: Sélectionner uniquement les colonnes nécessaires de Fusion
  # pour éviter les conflits de noms
  colonnes_fusion <- c("PlacetteID", "Annee", "ArbreID", "grade_bille", "vol_bille_dm3")
  Fusion_reduite <- Fusion[, ..colonnes_fusion]

  # MERGE avec all.x = TRUE pour garder tous les arbres de Data_Arbre
  Fusion_complete <- merge(Data_Arbre, Fusion_reduite,
                           by.x = c(col_placette, "Annee", col_arbre),
                           by.y = c("PlacetteID", "Annee", "ArbreID"),
                           all.x = TRUE)

  setDT(Fusion_complete)

  # Remplacer les NA par 0 pour les volumes
  Fusion_complete[is.na(vol_bille_dm3), vol_bille_dm3 := 0.0]

  # Remettre les valeurs de DHP en cm (si nécessaire)
  if(col_dhp == "DHP_Ae") {
    Fusion_complete[, (col_dhp) := get(col_dhp) / 10]
  }

  # Nettoyage final selon le paramètre Simplifier
  if(Simplifier == FALSE) {
    cols_to_remove <- c("cl_drai", "sdom_bio", "veg_pot", "ALTITUDE", "HT_REELLE_M", "nbTi_ha", "st_ha")
    cols_to_remove <- cols_to_remove[cols_to_remove %in% names(Fusion_complete)]
    if(length(cols_to_remove) > 0) {
      Fusion_complete[, (cols_to_remove) := NULL]
    }

    # Renommer les colonnes pour SaMARE
    if("DHP_Ae" %in% names(Fusion_complete)) {
      setnames(Fusion_complete, "DHP_Ae", "DHPcm")
    }
    if("HAUTEUR_M" %in% names(Fusion_complete)) {
      setnames(Fusion_complete, "HAUTEUR_M", "Hautm")
    }
  }

  setorderv(Fusion_complete, c(col_placette, "Annee", col_arbre))

  return(Fusion_complete)
}

#result <- SimulSaMARE(NbIter = 10, Horizon = 5, Data = data1)
#result2 <- SimulSaMARE(NbIter = 10, Horizon = 2, RecruesGaules = 1, Data = Test2500m2, Gaules=GaulesTest2500m2)
#result55 <- SortieBillesFusion(result, Type = "DHP2015", dhs = 0.15, nom_grade1 = "sciage long", long_grade1 = 4, diam_grade1 = 8, Simplifier = F)
