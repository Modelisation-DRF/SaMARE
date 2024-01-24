#'Fonction qui modifie la région écologique en vue de son utilisation par
#'le module de billonnage Petro régionalisé
#'
#'
#' @param data Un dataframe qui contient en ligne les arbres dont on veut prévoir
#'             les rendements en produit à l'aide du module de billonnage Petro
#'             régionalisé.
#'
#'  @return Retourne un  dataframe qui contient en ligne les arbres dont on veut
#'          prévoir les rendements en produits à l'aide de Petro régionalisé avec le
#'          champ "eco" qui corespond à un groupement de régions écologiques
#'  @examples



ConvertisseurEco<- function (data){

  data$eco <- ifelse(data$Espece == "BOJ",
                     ifelse(data$reg_eco %in% c("1a", "2a", "2b", "2c", "3a", "3b", "SV"), "3O",
                            ifelse(data$reg_eco %in% c("3c", "3d"), "3E",
                                   ifelse(data$reg_eco %in% c("4a", "4b"), "4O_b",
                                          ifelse(data$reg_eco %in% c("4c"), "4O_c", "4E")
                                   )
                            )
                     ),
                     ifelse(data$reg_eco %in% c("1a", "2a", "2b", "2c"), "2O",
                            ifelse(data$reg_eco %in% c("3a"), "3O_a",
                                   ifelse(data$reg_eco %in% c("3b", "3c", "SV"), "3O_b",
                                          ifelse(data$reg_eco %in% c("4a", "4b"), "4O_b",
                                                 ifelse(data$reg_eco %in% c("4c"), "4O_c", "4E")
                                          )
                                   )
                            )
                     )
  )

 return(data)
}







