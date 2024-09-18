#' Fonction qui attribut la qualité aux tiges feuillues qui n'ont pas de valeurs de qualité.
#' La qualité est attribuée soit aux arbres dont la qualité n'a pas été évaluée ou
#' aux arbres qui en croissant atteignent le seuil de 23.1cm. La qualité est
#' attribuée sur la base du document AttribQual qui documente la proportion de tiges
#' C et D dans les tiges feuillues de 23.1 à 33.0 cm
#'
#' @param PlacSansQual Un dataframe contenant une ligne par arbre pour lequel
#'                      seulement les arbres qui ont atteint le seuil de 23.1 cm
#'                      durant la période de simulation en cours sont présents
#' @param rid1 Variable contenant le code de la région écologique
#' @return Retourne un dataframe avec le no de l'arbre et sa classe de qualité prédite
#' @export
#'
AttribQualFct<-function(PlacSansQual,rid1){

  PlacSansQual$rid1<-rid1
  Alea_n=runif(n=nrow(PlacSansQual))
  suppressMessages(
  PlacSansQualForm<-PlacSansQual %>%
                    inner_join(AttribQual,relationship = "many-to-many"))

  PlacSansQualForm$Alea<-Alea_n

  PlacSansQualForm<-PlacSansQualForm %>%
                    mutate(PredQual=ifelse(Alea<=ProbQualC,"C","D")) %>%
                    select(ArbreID,PredQual)

  return ( PlacSansQualForm)

}
