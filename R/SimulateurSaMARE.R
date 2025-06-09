#' Fonction principale pour une simulation de la croissance d'un peuplement avec le simulateur SaMARE
#'
#' @description \code{SimulSaMARE()} est la fonction principale pour exécuter une simulation de la croissance d'un peuplement avec le simulateur SaMARE
#'
#' @param NbIter Nombre d'iterations à effectuer (>=2 et un nombre pair).
#' @param Horizon Nombre de périodes de 5 ans sur à simuler, valeur de 1 à 12 (ex: 6 pour 30 ans de simulation).
#' @param RecruesGaules Identification du module de recrutement à utiliser
#' \itemize{
#'    \item 0 pour utiliser le module de recrutement basé sur les arbres de dimension marchande (par défaut).
#'    \item 1 pour utiliser le module de recrutement basé sur l'inventaire des gaules. Le paramètre \code{Gaules} doit être aussi spécifié.
#' }
#' @param Data Un dataframe contenant une liste d'arbres de dimension marchande regroupés en placette à simuler.
#'             Les colonnes suivantes doivent être dans le dataframe:
#' \itemize{
#'    \item Placette: identifiant de la placette
#'    \item NoArbre: identifiant de l'arbre dans la placette
#'    \item Espece: code à 3 caractères de l'essence de l'arbre  (ex: ERS)
#'    \item Etat: code de l'état de l'arbre, numérique (ex: 10 pour vivant, 11 pour martelé et 14 pour mort)
#'    \item DHPcm: DHP de l'arbre en cm (>9 cm)
#'    \item Vigueur: code de vigueur de l'arbre, un entier de 1 à 6. Facultatif, mettre NA. Si non fourni, la colonne MSCR doit être fournie.
#'    \item Nombre: nombre d'arbres que représente cet arbre dans la placette de superficie \code{Sup_PE}
#'    \item Sup_PE: superficie de la placette en ha
#'    \item Annee_Coupe: si la placette est dans un peuplement qui a subi une coupe partielle, l'année de la coupe, sinon NA
#'    \item Annee_Inventaire: l'année d'inventaire des arbres, si la colonne est absente, l'année courante sera utilisée
#'    \item ntrt: si la placette est dans un peuplement qui a subi une coupe partielle, nombre de coupes, sinon 0 ou NA
#'    \item MSCR: code MSCR. Facultatif, mettre NA non utilisé. Si non fourni, la colonne Vigueur doit être fournie.
#'    \item Latitude, Longitude: coordonnées de la placette en degrés décimaux
#'    \item Altitude: altitude de la placette en mètres
#'    \item Pente: pourcentage de pente de la placette (ex: 10), si la colonne est absente, la valeur sera estimée à partir de la localisation
#'    \item Reg_Eco: code de la région écologique de la placette (ex: 3a)
#'    \item Type_Eco: code du type écologique de la placette (ex: FE23)
#'    \item Ptot: précipitations totales annuelles de la placette en mm, si la colonne est absente, la valeur sera estimée à partir de la localisation
#'    \item Tmoy: température moyenne annuelle de la placette en degrés celcius, si la colonne est absente, la valeur sera estimée à partir de la localisation
#'    \item GrwDays: Nombre de jours de la saison de croissance de la placette, si la colonne est absente, la valeur sera estimée à partir de la localisation
#' }
#' @param Gaules Un dataframe contenant la liste des gaules par essence et par classe de diamètre.
#'               Cette information doit être fournie si le paramètre \code{RecruesGaules}=1.
#'               Les colonnes suivantes doivent être présentes:
#' \itemize{
#'    \item Placette: identifiant de la placette, doivent être les mêmes que ceux de \code{Data}
#'    \item Espece: code à 3 caractères de l'essence de l'arbre  (ex: ERS)
#'    \item DHPcm: classe de DHP de 2 cm de l'arbre (2, 4, 6 ou 8 cm)
#'    \item Nombre: nombre de gaules dans la classe de DHP pour cette essence dans la placette de superficie \code{Sup_PE}
#'    \item Sup_PE: superficie de la placette de gaules en ha
#' }
#' @param MCH Binaire. 1 = indique la présence de la maladie corticale du hêtre durant tout l'horizon de simulation.
#'                     0 = absence de la maladie (par défaut).
#' @param cubage Booléen
#' \itemize{
#'    \item \code{TRUE}: pour estimer la hauteur et le volume des arbres à la fin de la simulation (par défaut)
#'    \item \code{FALSE} pour ne pas les calculer.
#'    }
#'
#' @return Un dataframe contenant la liste des arbres, leur état, leur DHP, pour chaque pas de simulation
#'         et chaque iteration.
#'
#' @details
#' Lorsque \code{MCH}=1, la probabilité de mortalié des hêtres est estimée avec l'équation de l'avis technique
#' AT-SSRF 20 de la Direction de la recherche forestière.
#'
#' Les code de vigueurs sont
#' \itemize{
#'    \item 1: feuillu vigoureux avec potentiel de sciage
#'    \item 2: feuillu vigoureux avec potentiel de pâte
#'    \item 3: feuillu non-vigoureux avec potentiel de sciage
#'    \item 4: feuillu non-vigoureux avec potentiel de pâte
#'    \item 5: résineux vigoureux
#'    \item 6: résineux non-vigoureux
#' }
#'
#' @examples
#' \dontrun{
#' # Simulation sur 10 ans, recrutement de base
#' result <- SimulSaMARE(NbIter = 10, Horizon = 2, Data = Test2500m2)
#' # Simulation sur 10 ans, recrutement avec les gaules
#' result <- SimulSaMARE(NbIter = 10, Horizon = 2, RecruesGaules = 1,
#'                       Data = Test2500m2, Gaules=GaulesTest2500m2)
#' }
#'
#' @import data.table
#' @export


SimulSaMARE<-function(NbIter, Horizon, RecruesGaules=0, Data, Gaules, MCH=0, cubage=TRUE){

  select=dplyr::select

  # NbIter=2; Horizon=1; Data=Test400m2; RecruesGaules=0; Gaules = NULL; MCH = 0; cubage=TRUE
  # Data = Test400m2; NbIter = 20; Horizon = 3; multi_session = FALSE; RecruesGaules=0; Gaules = NULL; MCH = 0; cubage=TRUE
  # NbIter=2; Horizon=1; Data=Test2500m2; RecruesGaules=1; Gaules = GaulesTest2500m2; MCH = 0; cubage=TRUE; multi_session = FALSE

  # Vérification des arguments de la fonction principale
  Erreur <- verifArguments(NbIter=NbIter, Horizon=Horizon, RecruesGaules=RecruesGaules, Data=Data, Gaules=Gaules, MCH=MCH, cubage=cubage)
  if (Erreur != "ok") {stop(Erreur)}



   #### 0. Vérifier les colonnes des fichiers en entrée ####

  # Vérifier s'il manque des colonnes obligatoires dans le fichier en entrées
  col_abs <- trouver_noms_absents(Data)
  if (length(col_abs)>0) {stop(paste("Les colonnes suivantes ne sont pas dans le fichier en entr\u00E9e:", paste(unlist(col_abs), collapse=', ') ))}

  if (!missing(Gaules)){
    col_abs <- trouver_noms_absents_gaules(Gaules)
    if (length(col_abs)>0) {stop(paste("Les colonnes suivantes ne sont pas dans le fichier des gaules:", paste(unlist(col_abs), collapse=', ')))}
  }

  Data <- as.data.frame(Data)
  Data <- renommer_les_colonnes(Data)
  Gaules <- if (!missing(Gaules)) renommer_les_colonnes_gaules(Gaules) else NA

  # Vérifier si les colonnes Ptot Tmoy GrwDays sont présentes, sinon les extraire avec ExtractMap
  Data <- verifier_variable_meteo(Data)

  # Vérifier si les colonnes Pente, sinon les extraire avec ExtractMap
  Data <- verifier_variable_station(Data)


  #### 1. Validation/création de la variable Annee_Inventaire ####

  Data <- Data %>% filter(DHPcm>=9)
  Data <- valide_Annee_depart(Data)


  #### 2. Optimiser le nombre d'itérations vs nombre de placettes ####
  diviseur = 2
  # if (multi_session == TRUE) {
  #   if (NbIter<10) multi_session == FALSE else diviseur=10 # si moins de 10 iter, on ne pourra pas diviser par 10, donc ne pas faire de multisession et laisser diviseur à 2
  # }
  # Pour que la boucle Samare soit moins grosse (en ce moment elle est du nombre d'itérations), générer les itérations ici en dupliquant les placettes
  NbIter_orig <- NbIter
  test <- Data
  Data <- NULL
  for (i in 1:(NbIter/diviseur)){
    temp <- test %>% rename(Placette_orig=Placette) %>% mutate(Placette = paste(Placette_orig,i,sep = '.'))
    Data <- bind_rows(Data,temp)
  }
  if (RecruesGaules==1){
    test <- Gaules
    Gaules <- NULL
    for (i in 1:(NbIter/diviseur)){
      temp <- test %>% rename(Placette_orig=Placette) %>% mutate(Placette = paste(Placette_orig,i,sep = '.'))
      Gaules <- bind_rows(Gaules,temp)
    }
  }
  NbIter=diviseur # il faut au moins 2 iter pour le relation hd et cubage
  # on a copié les placettes nbiter/2, il reste donc seulement 2 itérations à faire


  #### 3. Préparation des paramètres ####

  # EfCovParms <- EffetCovParms # je ne vois pas à quoi sert ce fichier

  # Fichier des parametres des effets fixes
  Para <- MatchModuleParameters %>%
    mutate(Effect = str_to_lower(Effect)) %>%
    rename(GrEspece=Ess_groupe) %>%
    select(-VegPotID,-Veg_Pot)

  # Fichier des paramètres des effets fixes du module de gaules
  ParaGaules <- ParametresGaules %>%
    rename(GrEspece=Ess_groupe)

  # Merge des fichers des especes et des groupes d'especes, et ensuite merge avec les Essence,
  # pour obtenir la liste des essences dans chaque groupe d'essences
  ListeSp <- merge(MatchSpeciesGroups, SpeciesGroups, by="SpeciesGroupID") %>%
    merge(Species, by="SpeciesID") %>%
    rename(Espece=SpeciesName,GrEspece=SpeciesGroupName) %>%
    select(GrEspece,Espece)


  #### 4. Préparation des fichiers pour la simulation ####

  # Ordre des colonnes du fichier des arbres
  ColOrdre <- c("Placette","NoArbre","Espece","GrEspece","Etat","DHPcm","Vigueur","Nombre",
                "Sup_PE","Annee_Coupe","Latitude","Longitude","Altitude","Pente","Ptot","Tmoy",
                "GrwDays","Reg_Eco","Type_Eco", "MSCR","ntrt","ABCD", "Annee_Inventaire")

  Data <- Data %>% left_join(ListeSp, by="Espece")
  Data <- Data[ColOrdre]

   # S'il y a des gaules, préparer le fichier
  if (RecruesGaules==1){

    Gaules <- as.data.frame(Gaules)
    ColOrdre <- c("Placette","Espece","GrEspece","DHPcm","Nombre","Sup_PE")
    Gaules <- Gaules %>% inner_join(ListeSp, by="Espece")
    Gaules <- Gaules[ColOrdre]


    # Sélection des placettes avec Gaules
    IndexGaules <- Gaules %>%
      group_by(Placette) %>%
      summarise()
    Data <- Data %>% inner_join(IndexGaules, by="Placette")
  }



  #### 5. Génération des effets aléatoires ####

  # une ligne par module/placette/iter/step
  # 8 modules
  # ex: 250 placettes, 50 itération, 8 steps = 100 000 x 8 modules = 800 000 lignes
  RandPlacStep <- RandomPlacStep(CovParms=MatchModuleCovparms, Data=Data, NbIter=NbIter, NbPeriodes=Horizon)


  # S'il y a des gaules, générer leur effets aléatoires
  RandPlacStepGaules <- NULL
  if (RecruesGaules==1){
    RandPlacStepGaules <- RandomPlacStepGaules(CovParms=CovparmGaules ,Data=Gaules, NbIter=NbIter)
  }


  #### 6. Appel de la fonction SaMARE ####

  matchModuleOmega <- MatchModuleOmega
  omegaGaulesFormat <- OmegaGaulesFormat
  paraGaules <- ParaGaules
  para <- Para
  covparmGaules <- CovparmGaules
  matchModuleCovparms <- MatchModuleCovparms


  # doFuture::registerDoFuture()
  # options(future.globals.maxSize= 891289600)      # Monte la tolérence à 850 megs pour les éléments passés dans dofuture
  # future::plan(sequential) #temporaire
  # if (multi_session==TRUE) {
  #    future::plan(multisession, workers=parallelly::availableCores()/2)  # Limite le nombre de coeurs utilisé pour éviter de planter l'ordi
  # }
  # Simul <- bind_rows(
  #   foreach(x = 1:NbIter, .packages = c("mvtnorm"))  %dorng%
  #     {SaMARE(Random=RandPlacStep, RandomGaules=RandPlacStepGaules, Data=Data, Gaules=Gaules,
  #             Iteration=x, Horizon=Horizon, RecruesGaules=RecruesGaules, MCH=MCH,
  #             CovParms=matchModuleCovparms, CovParmsGaules=covparmGaules,
  #             Para=para, ParaGaules=paraGaules, Omega=matchModuleOmega, OmegaGaules=omegaGaulesFormat)}
  # )
  #
  # future::plan(sequential)
  # setDT(Simul)
  # le parallèle n'augmente pas vraiment le temps de simulation, même que ça plante si le nombre d'itérations est trop grand

 Simul <- list(NbIter)
  for(x in 1:NbIter){
    Simul[[x]] <- SaMARE(Random=RandPlacStep, RandomGaules=RandPlacStepGaules, Data=Data, Gaules=Gaules,
            Iteration=x, Horizon=Horizon, RecruesGaules=RecruesGaules, MCH=MCH,
            CovParms=matchModuleCovparms, CovParmsGaules=covparmGaules,
            Para=para, ParaGaules=paraGaules, Omega=matchModuleOmega, OmegaGaules=omegaGaulesFormat)
  }
 Simul <- rbindlist(Simul)



  #### 6. Estimer la hauteur et le volume des arbres ####

  if (isTRUE(cubage)) {
    Simul <- cubage_arbres(data=Data, simul_data=Simul, NbIter=NbIter, Horizon=Horizon)
    setnames(Simul,
             old = c("vol_dm3", "hauteur_pred"),
             new = c("Vol_dm3", "Hautm"))
  } else {
    Simul[, `:=`(
      Hautm = NA,
      Vol_dm3 = NA
    )]
  }


  #### 7. Préparer fichier final ####

  Simul[, `:=`(
    Temps = Annee - Annee_Inventaire,
    Temps_depuis_coupe = Annee - t0,
    t0_aj_ = NULL,  # servait à vérifier sa valeur dans la fct SamaRE
    ntrt = NULL,  # servait à vérifier sa valeur dans la fct SamaRE, elle peut être modifiée dans la boucle samare
    trt = NULL  # servait à vérifier sa valeur dans la fct SamaRE, elle peut être modifiée dans la boucle samare vs ntrt original du fichier d'intrant
  )]

  # ajouter certaines variables échelle placette du fichier intrants
  info <- Data %>% group_by(Placette) %>% slice(1) %>% select(Placette, Latitude, Longitude, Sup_PE, ntrt)
  setDT(info)
  Simul <- merge(Simul, info, by = "Placette", all.x = T, sort = FALSE)

  # recréer le numéro de placette original et le numéro d'itération original
  Simul[,  `:=`(
    Iter_temp =     as.numeric(sub(".*\\.", "", Placette)), # extrait ce qui suit le dernier '.'
    Placette = sub("\\..*", "", Placette))
    ][, Iter := .GRP, by = .(Iter, Iter_temp)] # numéroter les combinaisons uniques de iter et iter_temp pour créer les numéro d'iter de 1 à NbIter_orig
  Simul[, `:=`(Iter_temp = NULL)]
  Simul[, `:=`(
      PlacetteID = paste0(Placette,"_", Iter),
      ST_m2 = pi*(DHPcm/200)^2,  # on en tient pas compte de Nombre
      Vigueur = case_when(
        vigu0 == "ViG" & prod0 == "sciage" ~ as.integer(1),
        vigu0 == "ViG" & prod0 == "pate" ~ as.integer(2),
        vigu0 == "NONVIG" & prod0 == "sciage" & DHPcm>=23.1 ~ as.integer(3),
        vigu0 == "NONVIG" & prod0 == "sciage" & DHPcm<23.1 ~ as.integer(4),
        vigu0 == "NONVIG" & prod0 == "pate" ~ as.integer(4),
        vigu0 == "ViG" & prod0 == "resineux" ~ as.integer(5),
        vigu0 == "NONVIG" & prod0 == "resineux" ~ as.integer(6),
        TRUE ~ NA_integer_
      )
    )]
  setnames(Simul,
           old = c("t0", "mch", 'prod0', 'vigu0', 'NoArbre'),   #PlacetteID=Placette: je ne renomme pas la variable
           new = c("Annee_Coupe", "MCH", "Prod", "Vig", 'origTreeID'))
  setorder(Simul, Iter, Placette, Annee, Residuel, ArbreID)

  # mettre les colonnes en ordre
  Simul <- Simul %>% lazy_dt() %>%
    relocate(Iter, Placette, PlacetteID, Latitude, Longitude, Annee_Inventaire, Sup_PE, MCH, Residuel, Annee, Annee_Coupe, Temps, Temps_depuis_coupe, ntrt,
             origTreeID, ArbreID, Espece, GrEspece, Etat, Nombre, DHPcm, ST_m2, Hautm, Vol_dm3, Prod, Vig, Vigueur, ABCD, MSCR) %>%
    as.data.frame()
  # les colonnes de gaules seront par défaut à la fin du fichier car pas nommées dans le relocate

  return(Simul)

}

