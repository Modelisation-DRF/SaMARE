#' Fonction qui structure un dataframe de sortie dont chaque ligne correspond
#' à chacun des arbres par placette, par classe Petro, par itération et par année.
#'
#' @param Data Un dataframe qui contient en ligne les arbres dont on veut prévoir
#'             les rendements en produit à l'aide du module de billonnage Petro
#'             régionalisé. sera applier sur "ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"
#' @param Type "DHP" pour utiliser les équations régionalisées basées seulement sur le DHP
#'             "ABCD" pour utiliser les équations régionalisées basées sur ABCD
#'             "1234" pour utiliser les équations de 2015 basées sur 1234
#'             "MSCR" pour utiliser les équations de 2015 basées sur MSCR
#'             "DHP2015" pour utiliser les équations de 2015 basées seulement sur le DHP
#'             "ABCD2015" pour utiliser les équations de 2015 basées sur ABCD
#' @return Retourne un dataframe avec l'estimation du volume par classe de produit
#'          pour chacun des arbres feuillus de plus de 23 cm.
#' @export

#' Fonction qui structure un dataframe de sortie dont chaque ligne correspond
#' à chacun des arbres par placette, par classe Petro, par itération et par année.
#'
#' @param Data Un dataframe qui contient en ligne les arbres dont on veut prévoir
#'             les rendements en produit à l'aide du module de billonnage Petro
#'             régionalisé. sera applier sur "ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"
#' @param Type "DHP" pour utiliser les équations régionalisées basées seulement sur le DHP
#'             "ABCD" pour utiliser les équations régionalisées basées sur ABCD
#'             "1234" pour utiliser les équations de 2015 basées sur 1234
#'             "MSCR" pour utiliser les équations de 2015 basées sur MSCR
#'             "DHP2015" pour utiliser les équations de 2015 basées seulement sur le DHP
#'             "ABCD2015" pour utiliser les équations de 2015 basées sur ABCD
#' @return Retourne un dataframe avec l'estimation du volume par classe de produit
#'          pour chacun des arbres feuillus de plus de 23 cm.
#' @export

SortieBillonnage <- function(Data, Type){

  # Data=fic; Type="DHP2015"
  select <- dplyr::select
  Data_ori <- Data

  # Petro a une équation pour CHR, attribuer CHR aux 3 autres especes de chenes
  # Si recrue de plus de 23 cm, elle n'a rien dans Espece, utiliser GrEspece (donc ça sera seulement ERS, ERR, BOJ, HEG qui passeront dans Petro, car les autres sont des groupes ou des ess pas de Petro)
  Data <- Data %>%
    lazy_dt() %>%
    filter(DHPcm > 23) %>%
    mutate(Espece_original = Espece,
           Espece = ifelse(Espece %in% c("CHR","CHG","CHB","CHE"),"CHR", Espece),
           Espece = ifelse(is.na(Espece), GrEspece, Espece)) %>%
    filter(Espece %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR")) %>%  # j'ai change CHX pour CHR car cette ligne ne fonctionnera jamais pour CHX, car le CHR est dans le Grespce FEN pour les recrues, et si on n'a l'espece original (pour les arbres qui sont là dès le départ, ça sera CHR, CHE, CHG ou CHB)
    as.data.frame()

  # essences billonnage:         BOJ         ERR   ERS               HEG               CHR  BOP
  # essences samare:      "AUT" "BOJ" "EPX" "ERR" "ERS" "FEN" "FIN" "HEG" "RES" "SAB"

  # data <- Data %>%
  #   lazy_dt() %>%
  #   filter(Espece %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR") ) %>%  # j'ai change CHX pour CHR car cette ligne ne fonctionnera jamais pour CHX, car le CHR est dans le Grespce FEN pour les recrues, et si on n'a l'espece original (pour les arbres qui sont là dès le départ, ça sera CHR, CHE, CHG ou CHB)
  #   as.data.frame()

  #Vérifier s'il y a des données valides après filtrage et retourner immédiatement si vide
  if (nrow(Data) == 0) {
    # Copier la structure des colonnes nécessaires depuis Data
    cols_needed <- c("PlacetteID", "Annee", "origTreeID")
    final_transpo <- as.data.table(Data)[0, ..cols_needed]

    # Ajouter les colonnes supplémentaires avec les types forcés
    final_transpo[, grade_bille := character(0)]
    final_transpo[, vol_bille_dm3 := numeric(0)]
    return(final_transpo)
  }

  # Traiter les données si on a des lignes valides
  data1 <- Data %>%
    lazy_dt() %>%
    mutate(bilonID = seq_len(nrow(Data))) %>% # numéroter les arbres
    rename(vigu0=Vig, prod0=Prod) %>% # nécessaire pour type=1234
    as.data.frame()

  billo <- Billonage::SIMBillonnageABCD_DHP(data1, Type)

  # utiliser Data_ori?
  final <- data1 %>% lazy_dt() %>%
    left_join(billo, by = "bilonID") %>%
    # ceci est fait directewment dans la fct SimulSaMARE
    # mutate(Stm2ha = pi*(DHPcm/200)^2,  # ici st n'est pas à l'ha, Nombre et SUP_PE n'est pas pris en compte
    #        Vigueur = case_when(
    #           vigu0 == "ViG" & prod0 == "sciage" ~ 1,
    #           vigu0 == "ViG" & prod0 == "pate" ~ 2,
    #           vigu0 == "NONVIG" & prod0 == "sciage" & DHPcm>=23.1 ~ 3,
    #           vigu0 == "NONVIG" & prod0 == "sciage" & DHPcm<23.1 ~ 4,
    #           vigu0 == "NONVIG" & prod0 == "pate" ~ 4,
    #           vigu0 == "ViG" & prod0 == "resineux" ~ 5,
    #           vigu0 == "NONVIG" & prod0 == "resineux" ~ 6,
    #           TRUE ~ NA_integer_)
    #           ) %>%
    dplyr::select(-Espece, -bilonID) %>%
    rename(Espece = Espece_original) %>%
    # Laisser toutes les variable de Data
    #select(Annee, Residuel, ArbreID, Iter, NoArbre, Placette, Nombre, GrEspece, Espece,
    #       Etat, DHPcm, MSCR, hauteur_pred, vol_dm3, Stm2, Sup_PE, ABCD, Vigueur, DER, F1, F2, F3, F4, P, type) %>%
    as.data.frame()

  #Multiplication des valeurs par 1000 pour avoir les bonnes valeurs
  final <- final %>%
    mutate(across(c(DER, F1, F2, F3, F4, P), ~ .x * 1000))

  #On transpose les colonnes de Petro pour avoir la colonne grade_bille avec les valeurs
  billonage_cols <- c("DER", "F1", "F2", "F3", "F4", "P")
  existing_cols <- intersect(billonage_cols, colnames(final))

  #Vérifier nrow(final)et aussi vérifier les colonnes existantes
  if (nrow(final) == 0 || length(existing_cols) == 0) {
    # Copier la structure des colonnes nécessaires depuis Data
    cols_needed <- c("PlacetteID", "Annee", "origTreeID")
    final_transpo <- as.data.table(Data)[0, ..cols_needed]

    # Ajouter les colonnes supplémentaires avec les types forcés
    final_transpo[, grade_bille := character(0)]
    final_transpo[, vol_bille_dm3 := numeric(0)]
    return(final_transpo)
  } else {
    #On donne les valeurs des colonnes respectives de billonage à vol_bille
    final_transpo <- final %>%
      pivot_longer(cols = all_of(existing_cols),
                   names_to = "grade_bille",
                   values_to = "vol_bille_dm3") #%>%
      #select(PlacetteID, Annee, ArbreID, grade_bille, vol_bille_dm3)

    #On enleve les possibles erreurs de fichiers en mettant le fichier en data.table
    final_transpo <- suppressMessages(setDT(final_transpo))
  }

  return(final_transpo)
}

#data1 <- fread("C:/Users/boini5/Downloads/Donnees_test_billon_shiny - 1 placette.csv", sep = ";", header = TRUE)
#result <- SimulSaMARE(NbIter = 10, Horizon = 5, Data = data1)
#result6 <- SortieBillonnage(result, "DHP2015")
