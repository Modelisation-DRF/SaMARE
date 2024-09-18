# Créer les fichiers d'exemple


# fichier de 400 m2
Test400m2 <- read_delim("data-raw/Fichiers_exemple/Test400m2.csv", delim=';')
usethis::use_data(Test400m2,
                  internal=FALSE, overwrite = TRUE)

# fichier de 400 m2 coupe
Test400m2Coupe <- read_delim("data-raw/Fichiers_exemple/Test400m2Coupe.csv", delim=';')
usethis::use_data(Test400m2Coupe,
                  internal=FALSE, overwrite = TRUE)


# fichier gaules 2500m2
GaulesTest2500m2 <- read_delim("data-raw/Fichiers_exemple/GaulesTest2500m2.csv", delim=';')
usethis::use_data(GaulesTest2500m2,
                  internal=FALSE, overwrite = TRUE)


# fichier de 2500 m2
Test2500m2 <- read_delim("data-raw/Fichiers_exemple/Test2500m2.csv", delim=';')
usethis::use_data(Test2500m2,
                  internal=FALSE, overwrite = TRUE)
