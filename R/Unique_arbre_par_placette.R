



verifier_arbre_uniques_par_placette <- function(data) {

  data_diviser_par_placette <- split(data, data$Placette)


  arbre_uniques_par_placette <- lapply(data_diviser_par_placette, function(data_placette) {

    length(unique(data_placette$NoArbre)) == nrow(data_placette)
  })

  if(all(arbre_uniques_par_placette) == TRUE){

    return(TRUE)
  }else{

    return(FALSE)
  }
}
