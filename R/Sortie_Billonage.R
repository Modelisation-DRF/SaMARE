#' Fonction qui structure un dataframe de sortie dont chaque ligne correspond
#' à chacun des arbres par placette, par classe Petro, par itération et par année.
#'
#' @param Data Un dataframe qui contient en ligne les arbres dont on veut prévoir
#'             les rendements en produit à l'aide du module de billonnage Petro
#'             régionalisé.
#' @param Type "DHP" pour utiliser les équations régionalisées basées seulement sur le DHP
#'             "ABCD" pour utiliser les équations régionalisées basées sur ABCD
#'             "1234" pour utiliser les équations de 2015 basées sur 1234
#'             "MSCR" pour utiliser les équations de 2015 basées sur MSCR
#'             "DHP2015" pour utiliser les équations de 2015 basées seulement sur le DHP
#'             "ABCD2015" pour utiliser les équations de 2015 basées sur ABCD
#' @return Retourne un dataframe avec l'estimation du volume par classe de produit
#'          pour chacun des arbres feuillus.
#' @export

SortieBillonage <- function(Data, Type ){

  select=dplyr::select

  Data<- Data %>% filter(DHPcm >23)

  data<- Data %>% filter(Espece %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHX") )

  if (nrow(data) == 0) {

    Data<- Data %>% mutate(erreur = "Code d'essence a l'exterieur de la plage de valeurs possibles pour billonage ")

    return(Data)
  }

  data1 <- data %>% mutate(bilonID = seq_len(nrow(data)))


  billo <- Billonage::SIMBillonnageABCD_DHP(data1, Type)

  final <- left_join(data1, billo, by = "bilonID") %>%  mutate (Stm2ha=pi*(DHPcm/200)^2,
                                                                Vigueur = case_when(
                                                                  vigu0 == "ViG" & prod0 == "sciage" ~ 1,
                                                                  vigu0 == "ViG" & prod0 == "pate" ~ 2,
                                                                  vigu0 == "NONVIG" & prod0 == "sciage" & DHPcm>=23.1 ~ 3,
                                                                  vigu0 == "NONVIG" & prod0 == "sciage" & DHPcm<23.1 ~ 4,
                                                                  vigu0 == "NONVIG" & prod0 == "pate" ~ 4,
                                                                  vigu0 == "ViG" & prod0 == "resineux" ~ 5,
                                                                  vigu0 == "NONVIG" & prod0 == "resineux" ~ 6,
                                                                  TRUE ~ NA_integer_
                                                                ))

  final <- final %>% select(Annee,Residuel,ArbreID,Iter,NoArbre,Placette,Nombre,GrEspece,Espece,
                            Etat,DHPcm,MSCR,hauteur_pred,vol_dm3,Stm2ha,Sup_PE,ABCD,Vigueur,DER,F1,F2,F3,F4,P,type)


  return(final)

  }
