
#'Fonction qui lit les paramètres nécessaire au simulateur SaMARE

#' @return Retourne une liste de dataframes qui contiennent les paramètres
#'         nécessaires à la simulation
#' @examples
#'

ParametresSimulSaMARE <- function() {

  CovParms<-MatchModuleCovparms
  EfCovParms<-EffetCovparms

  CovParmsGaules<-CovparmGaules

  ####### Fichier des parametres
  Para<-MatchModuleParameters

  ParaGaules<-ParametresGaules


  # Fichier des especes
  Sp<-Species.csv
  SpGroups<-SpeciesGroups
  # Fichier des especes dans chacun des groupes d'especes
  MatchSpGroups<-MatchSpeciesGroups

  #Omega
  Omega<-MatchModuleOmega

  OmegaGaules<-OmegaGaulesFormat

  List_Para <- list(CovParms,EfCovParms, CovParmsGaules, Para, ParaGaules, Sp, SpGroups, MatchSpGroups, Omega, OmegaGaules )
  return(List_Para)
}
