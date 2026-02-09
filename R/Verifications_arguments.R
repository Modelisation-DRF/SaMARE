#' Vérification des arguments de la fonction principale du simulateur SaMARE
#'
#' @description Vérification des arguments de la fonction principale \code{SimulSaMARE()} du simulateur Natura
#'
#' @inheritParams SimulSaMARE
#'
#' @return Une chaine de caractères contant "ok" s'il n'y a pas d'erreur, sinon, contient un message d'erreur.
# #' @export
#'
# @examples
verifArguments <- function(NbIter, Horizon, RecruesGaules, Data, Gaules, MCH, cubage, Coupe_ON, Coupe_modif) {

  # on doit spécifier au moins un 1 fichier
  if (missing(Data)) {
    erreur <- "Data doit etre specifie"
  }
  # RecruesGaules doit être 0 ou 1
  else if (!RecruesGaules %in% c(0, 1)) {
    erreur <- c("RecruesGaules doit etre 0 ou 1")
  }
  # MCH doit être 0 ou 1
  else if (!MCH %in% c(0, 1)) {
    erreur <- c("MCH doit etre 0 ou 1")
  }
  # cubage doit etre T ou F
  else if (!cubage %in% c(TRUE, FALSE)) {
    erreur <- c("cubage doit etre TRUE ou FALSE")
  }
  # l'horizon doit etre entre 1 et 15
  else if (Horizon>12 | Horizon<1) {
    erreur <- c("Horizon doit etre de 1 a 12")
  }
  # l'NbIter doit > 1
  else if (NbIter<=1) {
    erreur <- c("NbIter doit etre > 1")
  }
  # l'NbIter doit est pair
  else if (NbIter %% 2 > 0) {
    erreur <- c("NbIter doit etre un nombre pair")
  }
  # Si on spécifie RecruesGaules=1, Gaules doit être specifie
  else if (RecruesGaules==1 & missing(Gaules)){
    erreur <- "Avec RecruesGaules==1 Gaules doit etre specifie"
  }
  # # multi_session doit etre TRUE or FALSE
  # else if (!multi_session %in% c(TRUE, FALSE)) {
  #   erreur <- c("multi_session doit etre TRUE ou FALSE")
  # }
  #
  else erreur <- c("ok")
  #
  # validation coupe_on
  if (!is.null(Coupe_ON)) {

    if (!is.vector(Coupe_ON) || !is.numeric(Coupe_ON)) {
      erreur <- "L'argument Coupe_ON doit etre un vecteur numerique"
    }
    if (length(Coupe_ON) != Horizon) {
      erreur <- paste("La longueur de Coupe_ON doit etre egale a Horizon (", Horizon, ")", sep="")
    }
    # Vérifier que les valeurs sont valides (0-18 ou NA)
    valid_values <- c(0:19, NA)
    if (!all(Coupe_ON %in% valid_values)) {
      erreur <- "Les valeurs de Coupe_ON doivent etre entre 0 et 19 ou NA"
    }
  }
  #
  # Validation du paramètre Coupe_modif
  if (!is.null(Coupe_modif)) {
    if (!is.list(Coupe_modif)) {
      erreur <- "L'argument Coupe_modif doit etre une liste"
    }
    if (length(Coupe_modif) != Horizon) {
      erreur <- paste("La longueur de Coupe_modif doit etre egale a Horizon (", Horizon, ")", sep="")
    }

    # Valider chaque élément de la liste
    for (i in 1:length(Coupe_modif)) {
      elem <- Coupe_modif[[i]]

      # Skip NULL elements
      if (is.null(elem)) {
        next
      }

      # Handle single values (numbers or NA)
      if (length(elem) == 1) {
        if (is.na(elem)) {
          next  # Skip NA values
        } else if (is.numeric(elem)) {
          if (elem < -80 || elem > 160) {
            erreur <- paste("Le modificateur a la position", i, "doit etre entre -80 et 160")
          }
        } else {
          erreur <- paste("L'element a la position", i,
                     "doit etre un nombre, un data.frame, NA ou NULL")
        }
      }
      # Handle data frames or lists with multiple elements
      else if (is.data.frame(elem)) {
        if (!all(c("ess_ind", "modifier") %in% names(elem))) {
          erreur <- paste("Le data.frame a la position", i,
                     "doit contenir les colonnes 'ess_ind' et 'modifier'")
        }
        if (any(elem$modifier < -80 | elem$modifier > 160)) {
          erreur <- paste("Tous les modificateurs dans le data.frame a la position", i,
                     "doivent etre entre -80 et 160")
        }
      }
      # Handle other multi-element objects
      else {
        erreur <- paste("L'element a la position", i,
                   "doit etre un nombre, un data.frame, NA ou NULL")
      }
    }
  }

  return(erreur)
}
