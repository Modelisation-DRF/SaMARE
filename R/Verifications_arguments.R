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

  erreur <- list()

  # on doit spécifier au moins un 1 fichier
  if (missing(Data)) {
    erreur[[1]] <- "Data doit etre specifie"
  }
  # RecruesGaules doit être 0 ou 1
  if (!RecruesGaules %in% c(0, 1)) {
    erreur[[2]] <- c("RecruesGaules doit etre 0 ou 1")
  }
  # MCH doit être 0 ou 1
  if (!MCH %in% c(0, 1)) {
    erreur[[3]] <- c("MCH doit etre 0 ou 1")
  }
  # cubage doit etre T ou F
  if (!cubage %in% c(TRUE, FALSE)) {
    erreur[[4]] <- c("cubage doit etre TRUE ou FALSE")
  }
  # l'horizon doit etre entre 1 et 15
  if (Horizon>20 | Horizon<1) {
    erreur[[5]] <- c("Horizon doit etre de 1 a 20")
  }
  # l'NbIter doit > 1
  if (NbIter<=1) {
    erreur[[6]] <- c("NbIter doit etre > 1")
  }
  # l'NbIter doit est pair
  if (NbIter %% 2 > 0) {
    erreur[[7]] <- c("NbIter doit etre un nombre pair")
  }
  # Si on spécifie RecruesGaules=1, Gaules doit être specifie
  if (RecruesGaules==1 & missing(Gaules)){
    erreur[[8]] <- "Avec RecruesGaules==1 Gaules doit etre specifie"
  }
  # # multi_session doit etre TRUE or FALSE
  # else if (!multi_session %in% c(TRUE, FALSE)) {
  #   erreur <- c("multi_session doit etre TRUE ou FALSE")
  # }
  #
  #else erreur <- c("ok")
  #
  # validation coupe_on
  if (!is.null(Coupe_ON)) {

    if (!is.vector(Coupe_ON) || !is.numeric(Coupe_ON)) {
      erreur[[9]] <- "L'argument Coupe_ON doit etre un vecteur numerique"
    }
    if (length(Coupe_ON) != Horizon) {
      erreur[[10]] <- paste("La longueur de Coupe_ON doit etre egale a Horizon (", Horizon, ")", sep="")
    }
    # Vérifier que les valeurs sont valides (0-18 ou NA)
    valid_values <- c(0:19, NA)
    if (!all(Coupe_ON %in% valid_values)) {
      erreur[[11]] <- "Les valeurs de Coupe_ON doivent etre entre 0 et 19 ou NA"
    }
  }
  #
  # Validation du paramètre Coupe_modif
  if (!is.null(Coupe_modif)) {
    if (!is.list(Coupe_modif)) {
      erreur[[12]] <- "L'argument Coupe_modif doit etre une liste"
    }
    if (length(Coupe_modif) != Horizon) {
      erreur[[13]] <- paste("La longueur de Coupe_modif doit etre egale a Horizon (", Horizon, ")", sep="")
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
            erreur[[14]] <- paste("Le modificateur a la position", i, "doit etre entre -80 et 160")
          }
        } else {
          erreur[[15]] <- paste("L'element a la position", i,
                     "doit etre un nombre, un data.frame, NA ou NULL")
        }
      }
      # Handle data frames or lists with multiple elements
      else if (is.data.frame(elem)) {
        if (!all(c("ess_ind", "modifier") %in% names(elem))) {
          erreur[[16]] <- paste("Le data.frame a la position", i,
                     "doit contenir les colonnes 'ess_ind' et 'modifier'")
        }
        if (any(elem$modifier < -80 | elem$modifier > 160)) {
          erreur[[17]] <- paste("Tous les modificateurs dans le data.frame a la position", i,
                     "doivent etre entre -80 et 160")
        }
      }
      # Handle other multi-element objects
      else {
        erreur[[18]] <- paste("L'element a la position", i,
                   "doit etre un nombre, un data.frame, NA ou NULL")
      }
    }
  }

  erreur <- erreur[!sapply(erreur, is.null)]

  return(erreur)
}
