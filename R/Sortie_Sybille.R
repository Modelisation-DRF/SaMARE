#' Prépare les données pour le module Sybille et calcule les volumes de billes
#'
#' Cette fonction prend un jeu de données forestières, le transforme selon les exigences de Sybille et applique l'algorithme.
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
#'
#' @return Un data.table contenant les résultats du calcul des volumes de billes avec les colonnes:
#'   \itemize{
#'     \item id_pe - Identifiant de la placette
#'     \item no_arbre - Numéro de l'arbre
#'     \item vol_bille_dm3 - Volume de la bille en dm³
#'     \item grade_bille - Type de la bille
#'     \item Annee - Annee de la simulation pour l'arbre
#'   }
#'
#' @details
#' La fonction effectue les opérations suivantes:
#' \enumerate{
#'   \item Calcule la densité d'arbres (nbTi_ha) et la surface terrière (st_ha) par placette et année
#'   \item Change les noms de colonnes pour compatibilité avec calcul_vol_bille
#'   \item Transforme les codes de sous-domaine ('E' en 'EST', 'O' en 'OUEST')
#'   \item Extrait le premier caractère de la classe de drainage
#'   \item Applique calcul_vol_bille avec ses paramètres
#' }
#'
#' @examples
#' \dontrun{
#'   donnees <- data.frame(
#'     Veg_Pot = c("MS2", "RS2"),
#'     PlacetteID = c(1, 1),
#'     DHPcm = c(15, 25),
#'     Altitude = c(450, 450),
#'     hauteur_pred = c(12, 18),
#'     origTreeID = c(1, 2),
#'     Espece = c("SAB", "EPN"),
#'     Cl_Drai = c("2", "3"),
#'     Etat = c("vivant", "vivant"),
#'     Nombre = c(1, 1),
#'     sdom_bio = c("4E", "4O"),
#'     Annee = c(2020, 2020)
#'   )
#'
#'   resultats <- SortieSybille(donnees, nom_grade1 = "pate", nom_grade1 = 8, diam_grade1 = 4)
#'   head(resultats)
#' }
#'
#' @export
SortieSybille <- function(Data, dhs = 0.15, nom_grade1 = NA, long_grade1 = NA, diam_grade1 = NA, nom_grade2 = NA, long_grade2 = NA, diam_grade2 = NA,
                          nom_grade3 = NA, long_grade3 = NA, diam_grade3 = NA) {

  setDT(Data)

  # Calculer nbTi_ha et st_ha et inclure dans Data
  Data[Etat != 'mort', `:=`(
    nbTi_ha = sum(Nombre/0.04),
    st_ha = sum(pi*(DHPcm/200)^2 * Nombre/0.04)
  ), by = .(PlacetteID, Annee)]

  ## Renommer les colonnes pour préparer le Data dans Sybille
  setnames(Data, c("PlacetteID", "DHPcm", "Hautm", "ArbreID", "Espece"),
           c("id_pe", "DHP_Ae", "HAUTEUR_M", "no_arbre", "essence"))

  # Ajouter les colonnes manquantes et effectuer les traitements de préparation de données
  # Puisque SaMARE n'utilise pas ces colonnes, on les initialise à NA pour Sybille
  Data[, `:=`(
    cl_drai = NA_character_,
    veg_pot = NA_character_,
    sdom_bio = NA_character_,
    ALTITUDE = NA_real_
  )]

  # temporaire pour résultats
  # Data[, essence := "BOP"]

  # Keep
  Data[, HT_REELLE_M := 0]

  # Multiplier par 10 pour satisfaire le calcul avec DHP_Ae
  Data[, DHP_Ae := DHP_Ae * 10]

  # On prend que les lignes où hauteur existe
  Data <- Data[is.finite(HAUTEUR_M)]

  # Garder que les colonnes nécessaires pour utiliser Sybille
  Data_treated <- Data[, .(essence, id_pe, no_arbre, sdom_bio, cl_drai, veg_pot, DHP_Ae, HT_REELLE_M, HAUTEUR_M, nbTi_ha, st_ha, ALTITUDE, Annee)]

  # Application de Sybille sur les données
  Data_calculated <- OutilsDRF::calcul_vol_bille(Data_treated, dhs, nom_grade1, long_grade1, diam_grade1, nom_grade2, long_grade2, diam_grade2,
                                                 nom_grade3, long_grade3, diam_grade3)

  #On renomme les colonnes pour matcher avec SaMARE
  setnames(Data_calculated, c("id_pe", "no_arbre"),
           c("PlacetteID", "ArbreID"))

  #On garde que les colonnes nécessaires
  Data_calculated <- Data_calculated[, .(PlacetteID, ArbreID, Annee, grade_bille, vol_bille_dm3)]

  return(Data_calculated)

}

###
#result <- SimulSaMARE(NbIter = 10, Horizon = 2, Data = Test2500m2)
#result2 <- SimulSaMARE(NbIter = 10, Horizon = 2, RecruesGaules = 1, Data = Test2500m2, Gaules=GaulesTest2500m2)
#result44 <- SortieSybille(result, dhs = 0.15, nom_grade1 = "sciage long", long_grade1 = 4, diam_grade1 = 8)
