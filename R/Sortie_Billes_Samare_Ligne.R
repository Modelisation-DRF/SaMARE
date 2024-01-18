


#' cette function prend en parametre un dafra qui a déja simuller et billoner

#' @param SimulHtVolBillon
#' @return
#' @examples
#'
SortieBillesSamareLigne <- function(SimulHtVolBillon){

  BillesSamareLigne <- SimulHtVolBillon %>%  mutate (Stm2ha=pi*(DHPcm/200)^2/Sup_PE,vigu0 =NULL, prod0=NULL, Nb_Gaules_Ha = NULL,
                                                      Nb_Gaules_ERS = NULL,
                                                      Nb_Gaules_BOJ=NULL,Nb_Gaules_SAB =NULL, Nb_Gaules_68_ERS=NULL,
                                                      Nb_Gaules_68_HEG=NULL,Nb_Gaules_68_BOJ=NULL,Nb_Gaules_68_SAB=NULL,
                                                      Nb_Gaules_68_Ha =NULL, Nb_Gaules_HEG=NULL, Sup_PE =NULL,
                                                      reg_eco=NULL, Type_Eco=NULL, Altitude =NULL, Ptot=NULL, Tmoy=NULL,
                                                      veg_pot=NULL, milieu=NULL, nb_tige=NULL)  %>%
                          relocate(Annee, PlacetteID,Residuel,ArbreID,NoArbre)



    return (BillesSamareLigne)

}
