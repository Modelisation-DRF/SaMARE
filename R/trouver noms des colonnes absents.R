
trouver_noms_absents <- function(Data) {

  ColOrdre<-c("Placette","NoArbre","Espece","Etat","DHPcm","Vigueur","Nombre",
              "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Pente","Ptot","Tmoy",
              "GrwDays","Reg_Eco","Type_Eco", "MSCR","ntrt")

  names(Data) <- tolower(names(Data))

  colone_minuscule <- lapply(ColOrdre, tolower)

  noms_absents <- setdiff(colone_minuscule, names(Data))

  return(noms_absents)
}


renommer_les_colonnes <- function(data){

  ColOrdre<-c("Placette","NoArbre","Espece","Etat","DHPcm","Vigueur","Nombre",
              "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Pente","Ptot","Tmoy",
              "GrwDays","Reg_Eco","Type_Eco", "MSCR","ntrt")

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
