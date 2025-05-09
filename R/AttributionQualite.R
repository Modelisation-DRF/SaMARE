#' Fonction qui attribut la qualité ABCD aux tiges feuillues qui n'ont pas de valeurs de qualité.
#' La qualité est attribuée soit aux arbres dont la qualité n'a pas été évaluée ou
#' aux arbres qui en croissant atteignent le seuil de 23.1cm.
#'
#' @param PlacSansQual Un dataframe contenant une ligne par arbre avec au minimum les colonnes Placette, ArbreID, GrEspece, ABCD, DHPcm1, rid1
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#' @return Retourne PlacSansQual avec la colonne ABCD et PredQual
#' @export
#'
AttribQualFct<-function(PlacSansQual, seed_value=NULL){

  # PlacSansQual <- AttribQual

  PlacSansQual1 <- PlacSansQual # faire une copie de l'original
  #verif <- names(PlacSansQual)
  #verif2 <- unique(verif)

  # sélectionner les lignes dont il faut attribuer la qualité
  PlacSansQual <- PlacSansQual %>%
    filter(GrEspece %in% c("BOJ","ERR","ERS","FEN","FIN","HEG") & (is.na(ABCD)==TRUE | is.null(ABCD)==TRUE | ABCD=="") & DHPcm1>=23.05 & DHPcm<23.05)

  if (nrow(PlacSansQual)>0){

    if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

    # ajouter les paramètres
    PlacSansQualForm <- left_join(PlacSansQual, AttribQual, by = c('GrEspece', 'rid1'))

    # attribuer la qualité
    PlacSansQualForm <- PlacSansQualForm %>%
      mutate(Alea = runif(n()),
             PredQual = ifelse(Alea<=ProbQualC, "C", "D")) %>%
      select(Placette, ArbreID, PredQual)


    # remettre les arbres avec le fichier complet
    setDT(PlacSansQualForm)
    PlacSansQualForm <- merge(PlacSansQual1, PlacSansQualForm, by = c("Placette", "ArbreID"), all.x = TRUE)
    PlacSansQualForm[,`:=`(
      ABCD = fifelse(is.na(PredQual)==FALSE, PredQual, ABCD)
    )
    ]
    #PlacSansQualForm[, c("PredQual") := NULL]

  } else { # si pas d'arbres dont la qualité est à attribuer, on retourne le fichier d'intrant

    PlacSansQualForm <-  PlacSansQual1


  }


  return ( PlacSansQualForm)

}




