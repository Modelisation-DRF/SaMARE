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
verifArguments <- function(NbIter, Horizon, RecruesGaules, Data, Gaules, MCH, cubage) {

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
  else erreur <- c("ok")
  return(erreur)
}
