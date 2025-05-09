# Créer les fichiers d'exemple


# fichier de 400 m2
#Test400m2 <- read_delim("data-raw/Fichiers_exemple/Test400m2.csv", delim=';')
load("data-raw/Fichiers_exemple/Test400m2.rda")
view(Test400m2)
Test400m2Temoin <- Test400m2 %>% mutate(Nombre=1)
usethis::use_data(Test400m2Temoin,
                  internal=FALSE, overwrite = TRUE)

# Mettre Test400m2 dans sysdata pour qu'il soit accessible pour les tests

# fichier de 400 m2 coupe
#Test400m2Coupe <- read_delim("data-raw/Fichiers_exemple/Test400m2Coupe.csv", delim=';') %>%
#  rename(Placette=PlacettE, NoArbre=NoArBre, Espece=EsPecE, Vigueur=VigueuR, Nombre=NombrE)
load("data-raw/Fichiers_exemple/Test400m2Coupe.rda")
Test400m2CP <- Test400m2Coupe %>% mutate(Nombre=1)
usethis::use_data(Test400m2CP,
                  internal=FALSE, overwrite = TRUE)

# Mettre Test400m2Coupe dans sysdata pour qu'il soit accessible pour les tests

# fichier gaules 2500m2
GaulesTest2500m2 <- read_delim("data-raw/Fichiers_exemple/GaulesTest2500m2.csv", delim=';')
usethis::use_data(GaulesTest2500m2,
                  internal=FALSE, overwrite = TRUE)


# fichier de 2500 m2
Test2500m2 <- read_delim("data-raw/Fichiers_exemple/Test2500m2.csv", delim=';')
usethis::use_data(Test2500m2,
                  internal=FALSE, overwrite = TRUE)
