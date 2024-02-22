#' Fonction prévoit la répartition par produits des arbres feuillus à l'aide des nouvelles
#' équation de Petro régionalisés issu des travaux du CFFM de Filip Havreljuk.
#'
#' @param Data UUn dataframe qui contient en ligne les arbres dont on veut prévoir
#'             les rendements en produit à l'aide du module de billonnage Petro
#'             régionalisé.
#' @param ligne une valeur binaire égale à 1 lorsque l'on désir avoir la sortie
#'              qui contient une ligne par produit ou 0 lorsque l'on désir avoir la sortie
#'              qui contient une colone par produit
#' @return Retourne un dataframe avec l'estimation du volume par classe de produit
#'          pour chacun des arbres feuillus.
#' @examples
#'

SIMBillonnageABCD_DHP<- function (data , type){

  select=dplyr::select

  data<- data %>% filter(DHPcm >23)
                          ##### ABCD#####
  if(!"eco" %in% colnames(data)){
    data <-ConvertisseurEco(data)
  }

  if(type == "ABCD" && all(is.na(data$ABCD))){
    type="DHP"
  }


  if(type %in% c("ABCD","DHP")){

    if(type == "ABCD"){
      # Séparer les arbres possédant la qualité ABCD des autres
     data_ABCD <- data %>% filter(!is.na(ABCD))
     data_pas_ABCD <-data %>% filter(is.na(ABCD))

     regional_ABCD <- data_ABCD %>% filter(Espece %in% c("ERS", "BOJ"))

     non_regional_2015_ABCD <- data_ABCD %>% filter(!Espece %in% c("ERS", "BOJ"))

     # Billonnage régionalisé pour les arbres possédant la qualité ABCD
     regional_result_ABCD <-ABCD_DHP_regio(data=regional_ABCD, type =type )

     #Billonnage non régionalisé pour les arbres possédant la qualité ABCD
     non_regional_2015_result_ABCD <- ABCD_DHP215(data=non_regional_2015_ABCD, type =type)


     regional_pas_ABCD <- data_pas_ABCD %>% filter(Espece %in% c("ERS", "BOJ"))
     non_regional_2015_pas_ABCD <- data_pas_ABCD %>% filter(!Espece %in% c("ERS", "BOJ"))

     # Billonnage régionalisé pour les arbres ne  possédant pas la qualité ABCD
     # donc Billonage éffectuer avec DHP
     regional_result_pas_ABCD <-ABCD_DHP_regio(data=regional_pas_ABCD, type ="DHP" )

     # Billonnage non régionalisé pour les arbres ne  possédant pas la qualité ABCD
     # donc Billonage éffectuer avec DHP

     non_regional_2015_result_pas_ABCD <- ABCD_DHP215(data=non_regional_2015_pas_ABCD, type ="DHP")

     finl1 <-rbind(regional_result_ABCD,non_regional_2015_result_ABCD)
     finl2 <-rbind(regional_result_pas_ABCD,non_regional_2015_result_pas_ABCD)
     final <-rbind(finl2,finl1)

    }else{

    regional <- data %>% filter(Espece %in% c("ERS", "BOJ"))
    regional_result <-ABCD_DHP_regio(data=regional, type =type )

    non_regional_2015 <- data %>% filter(!Espece %in% c("ERS", "BOJ"))
    non_regional_2015_result <- ABCD_DHP215(data=non_regional_2015, type =type)

    final <-merge(regional_result,regional_result)
   }
  }else{

    final <- ABCD_DHP215(data=data, type =type)
  }



  # filtrer <23 cm DHP
  # data<- data %>% filter(DHPcm >23)
  #
  # if(!"eco" %in% colnames(data)){
  #   data <-ConvertisseurEco(data)
  # }
  #
  # registerDoFuture()
  #
  # batch_size <- 10  # Taille de chaque lot
  # n <- nrow(data)
  # batch_indices <- split(1:n, ceiling(seq_along(1:n) / batch_size))
  #
  # sim_ABCD_DHPt <- future_sapply(batch_indices, function(indices) {
  #   batch_df <- data[indices, , drop = FALSE]
  #   ABCD_DHP(data = batch_df)
  # }, simplify = FALSE)
  #
  # resultbionage <- do.call(rbind, sim_ABCD_DHPt)





  return (final )


}
