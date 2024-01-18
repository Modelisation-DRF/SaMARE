
#'list de ^paramettre de base pour la simulation
#' @return
#' @examples
#'

ParametresSimulSaMARE <- function() {

  CovParms<-read_delim("Parametres/0_MatchModuleCovparms.csv", delim=";")
  EfCovParms<-read_delim("Parametres/EffetCovparms.csv", delim=";")

  CovParmsGaules<-read_delim("Parametres/CovparmGaules.csv", delim=";")

  ####### Fichier des parametres
  Para<-read_delim("Parametres/0_MatchModuleParameters.csv", delim=";")

  ParaGaules<-read_delim("Parametres/ParametresGaules.csv", delim=";")


  # Fichier des especes
  Sp<-read_delim("Parametres/0_Species.csv", delim=";")
  SpGroups<-read_delim("Parametres/0_SpeciesGroups.csv", delim=";")
  # Fichier des especes dans chacun des groupes d'especes
  MatchSpGroups<-read_delim("Parametres/0_MatchSpeciesGroups.csv", delim=";")

  #Omega
  Omega<-read_delim("Parametres/0_MatchModuleOmega.csv", delim=";")

  OmegaGaules<-read_delim("Parametres/OmegaGaulesFormat.csv", delim=",")

  List_Para <- list(CovParms,EfCovParms, CovParmsGaules, Para, ParaGaules, Sp, SpGroups, MatchSpGroups, Omega, OmegaGaules )
  return(List_Para)
}
