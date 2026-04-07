#' Prépare les données pour le module Sybille et calcule les volumes de billes
#'
#' Cette fonction prend un jeu de données forestières, le transforme selon les exigences de Sybille et applique l'algorithme.
#'
#' @param Data Un data.frame ou data.table contenant les données d'inventaire forestier avec les colonnes suivantes:
#'   \itemize{
#'     \item Iter - Identifiant de l'itération
#'     \item Placette - Identifiant de la placette/annee/iter
#'     \item Annee - Annee de la simulation pour l'arbre
#'     \item ArbreID - Numéro de l'arbre
#'     \item DHPcm - Diamètre à hauteur de poitrine en cm
#'     \item Hautm - Hauteur de l'arbre en mètres
#'     \item Espece - Code d'essence de l'arbre, ex: "SAB"
#'     \item Etat - État de l'arbre ("vivant", "mort", "recrue")
#'     \item Nombre - Nombre d'arbres représentés par cette observation dans une placette de superficie Sup_PE
#'     \item Sup_PE - Superficie de la placette en ha
#'
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
#'     \item Iter - Itération
#'     \item Placette - Identifiant de la placette
#'     \item ArbreID - Numéro de l'arbre
#'     \item Annee - Annee de la simulation pour l'arbre
#'     \item vol_bille_dm3 - Volume de la bille en dm³
#'     \item grade_bille - Type de la bille
#'   }
#'
#' @details
#' La fonction effectue les opérations suivantes:
#' \enumerate{
#'   \item Calcule la densité d'arbres (nbTi_ha) et la surface terrière (st_ha) par iteration, placette et année
#'   \item Change les noms de colonnes pour compatibilité avec calcul_vol_bille
#'   \item Applique calcul_vol_bille
#' }
#'
#' @export
SortieSybille <- function(Data, dhs = 0.15, nom_grade1 = NA, long_grade1 = NA, diam_grade1 = NA, nom_grade2 = NA, long_grade2 = NA, diam_grade2 = NA,
                          nom_grade3 = NA, long_grade3 = NA, diam_grade3 = NA) {

  Data1 <- Data
  setDT(Data1)

  # créer un numéro de placette temporaire qui concatène Iter/placette/annee
  Data1[, id_pe := paste(Iter, Placette, Annee, sep='_')]

  # Enlever les morts
  Data1 <- Data1[Etat != 'mort']

  # Calculer nbTi_ha et st_ha et inclure dans Data
  Data1[, `:=`(
    nbTi_ha = sum(Nombre)/Sup_PE,
    st_ha = sum(pi*(DHPcm/200)^2 * Nombre)/Sup_PE
  ), by = .(id_pe)]  # calculer par Iter/placette/annee

  ## Renommer les colonnes pour préparer le Data dans Sybille
  setnames(Data1, c("DHPcm", "Hautm", "ArbreID", "Espece"),
                  c("DHP_Ae", "HAUTEUR_M", "no_arbre", "essence"))

  # Ajouter les colonnes manquantes et effectuer les traitements de préparation de données
  # Puisque SaMARE n'utilise pas ces colonnes, on les initialise à NA pour Sybille
  Data1[, `:=`(
    cl_drai = NA_character_,
    veg_pot = NA_character_,
    sdom_bio = NA_character_,
    ALTITUDE = NA_real_
  )]

  # temporaire pour résultats
  #Data1[, essence := "BOP"]

  # Keep
  Data1[, HT_REELLE_M := 0]

  # Multiplier par 10 pour satisfaire le calcul avec DHP_Ae
  Data1[, DHP_Ae := DHP_Ae * 10]

  # On prend que les lignes où hauteur existe
  Data1 <- Data1[is.finite(HAUTEUR_M)]

  # Garder que les colonnes nécessaires pour utiliser Sybille
  # Annee est optionnel dans la fct calcul_vol_bille()
  Data_treated <- Data1[, .(essence, id_pe, no_arbre, sdom_bio, cl_drai, veg_pot, DHP_Ae, HT_REELLE_M, HAUTEUR_M, nbTi_ha, st_ha, ALTITUDE)]

  # Application de Sybille sur les données
  Data_calculated <- OutilsDRF::calcul_vol_bille(Data_treated, dhs, nom_grade1, long_grade1, diam_grade1, nom_grade2, long_grade2, diam_grade2,
                                                 nom_grade3, long_grade3, diam_grade3)

  # on remet Iter/PlacetteID/Annee dans le data
  data_placette <- Data1[, .(Iter, Placette, Annee, id_pe, no_arbre)]
  Data_calculated <- Data_calculated[data_placette, on= .(id_pe, no_arbre)]

  #On renomme les colonnes pour matcher avec SaMARE
  setnames(Data_calculated, c("no_arbre"),
                            c("ArbreID"))


  #On garde que les colonnes nécessaires
  Data_calculated <- Data_calculated[, .(Iter, Placette, ArbreID, Annee, grade_bille, vol_bille_dm3)]

  return(Data_calculated)

}

###
#result <- SimulSaMARE(NbIter = 10, Horizon = 2, Data = Test2500m2)
#result2 <- SimulSaMARE(NbIter = 10, Horizon = 2, RecruesGaules = 1, Data = Test2500m2, Gaules=GaulesTest2500m2)
#result44 <- SortieSybille(result, dhs = 0.15, nom_grade1 = "sciage long", long_grade1 = 4, diam_grade1 = 8)
