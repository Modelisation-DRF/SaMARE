
trouver_noms_absents <- function(Data) {

  ColOrdre<-c("Placette","NoArbre","Espece","Etat","DHPcm","Vigueur","Nombre",
              "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Pente","Reg_Eco","Type_Eco", "MSCR","ntrt","ABCD")

  names(Data) <- tolower(names(Data))

  colone_minuscule <- lapply(ColOrdre, tolower)

  noms_absents <- setdiff(colone_minuscule, names(Data))

  return(noms_absents)

}

trouver_noms_absents_gaules <- function(Data) {

  ColOrdre<-c("Placette","Espece","DHPcm","Nombre","Sup_PE")

  names(Data) <- tolower(names(Data))

  colone_minuscule <- lapply(ColOrdre, tolower)

  noms_absents <- setdiff(colone_minuscule, names(Data))

  return(noms_absents)

}






renommer_les_colonnes <- function(data){

  ColOrdre<-c("Placette","NoArbre","Espece","Etat","DHPcm","Vigueur","Nombre",
              "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Pente","Ptot","Tmoy",
              "GrwDays","Reg_Eco","Type_Eco", "MSCR","ntrt","ABCD")

  noms_colonnes_existants <- tolower(names(data))
  noms_colonnes_desires <- tolower(ColOrdre)

  for (i in seq_along(noms_colonnes_desires)) {
    index_colonne <- match(noms_colonnes_desires[i], noms_colonnes_existants)
    if (!is.na(index_colonne)) {
      names(data)[index_colonne] <- ColOrdre[i]
    }
  }

  return(data)
}



renommer_les_colonnes_gaules <- function(data){

  ColOrdre<-c("Placette","Espece","GrEspece","DHPcm","Nombre","Sup_PE")

  noms_colonnes_existants <- tolower(names(data))
  noms_colonnes_desires <- tolower(ColOrdre)

  for (i in seq_along(noms_colonnes_desires)) {
    index_colonne <- match(noms_colonnes_desires[i], noms_colonnes_existants)
    if (!is.na(index_colonne)) {
      names(data)[index_colonne] <- ColOrdre[i]
    }
  }

  return(data)
}








cppFunction('
#include <Rcpp.h>
#include <string>
#include <cctype>
#include <algorithm>

using namespace Rcpp;

// Helper function to convert a string to lowercase
std::string to_lower(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(),
                   [](unsigned char c){ return std::tolower(c); });
    return str;
}

// Main function to find missing column names
// [[Rcpp::export]]
CharacterVector trouver_noms_absents1(DataFrame Data) {
    CharacterVector ColOrdre = CharacterVector::create("Placette","NoArbre","Espece","Etat","DHPcm","Vigueur","Nombre",
                                                      "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Pente",
                                                      "Reg_Eco","Type_Eco", "MSCR","ntrt","ABCD");

    // Convert ColOrdre to lowercase
    for (int i = 0; i < ColOrdre.size(); ++i) {
        ColOrdre[i] = to_lower(as<std::string>(ColOrdre[i]));
    }

    // Extract names from Data and convert to lowercase
    CharacterVector dataNames = Data.names();
    for (int i = 0; i < dataNames.size(); ++i) {
        dataNames[i] = to_lower(as<std::string>(dataNames[i]));
    }

    // Find missing names
    CharacterVector noms_absents = setdiff(ColOrdre, dataNames);

    return noms_absents;
}
')


