#' Fonction qui estime la hauteur et le volume des arbres d'un fichier de résultats de simulation avec SaMARE
#'
#' @param data Dataframe d'intrant à la simulation avec les variables Placette, ArbreID, DHPcm, Sup_PE, Reg_Eco, Type_Eco, Altitude, Ptot, Tmoy
#'             (dataframe préparé dans la fonction SimulSaMARE)
#' @param simul_data Dataframe des résultats de simulation avec SaMARE
#' @inheritParams SimulSaMARE
#'
#' @return Retourne le dataframe Data avec les colonnes hauteur_pred et vol_dm3
#' @export
#'
cubage_arbres <- function(data, simul_data, NbIter, Horizon){

  # data=Data; simul_data=Simul; NbIter=3; Horizon=3

  #### 1. Préparation des variables pour modèles de hauteur et volume des arbres ####

  # Variable à l'échelle de la placette fixes dans le temps
  VarEco <- data %>%
    group_by(Placette) %>%
    slice(1) %>%
    select(Placette, Sup_PE, Reg_Eco, Type_Eco, Altitude, Ptot, Tmoy) %>%
    mutate(veg_pot = substr(Type_Eco,1,3), milieu = as.character(substr(Type_Eco,4,4)))

  # Ajouter les variables au fichier des simulations
  setDT(simul_data); setDT(VarEco)
  simul <- merge(simul_data, VarEco, by = "Placette", all.x = T) # je veux un vrai left_join
  simul[, nb_tige := Nombre / Sup_PE / 25]  # le nombre d'arbre que représente une ligne doit être le nombre dans 400 m2
  simul[, step := (Annee - min(Annee)) / 5 + 1, by = Placette] # numéroter les années de 1 à n
  setnames(simul,
           old = c("Placette", "DHPcm", "ArbreID", "Altitude", "Ptot", "Tmoy", "Reg_Eco", "Iter"),
           new = c("id_pe", "dhpcm", "no_arbre", "altitude", "p_tot", "t_ma", "reg_eco", "iter"))


  #### 2. Faire l'association d'essences pour l'équation de hauteur et de volume ####

  # si Espece est non manquant, l'utiliser pour cuber, sinon prendre GrEspece

  # Commencer par l'association avec Espece
  ass_ess_ht_vol1 <- ass_ess_ht_vol[,-2]
  Simul_esp <- simul[!is.na(Espece)]
  Simul_esp <- merge(Simul_esp, ass_ess_ht_vol1, by = "Espece", all.x = T)

  # Association par GrEspece pour les Espece à NA
  Simul_esp_na <- simul[is.na(Espece)]
  ass_ess_ht_vol2 <- ass_ess_ht_vol %>% group_by(GrEspece) %>% slice(1) %>% dplyr::select(-Espece)
  Simul_esp_na <- merge(Simul_esp_na, ass_ess_ht_vol2, by = "GrEspece", all.x = T)

  # remettre les 2 fichiers ensemble
  simul <- rbind(Simul_esp, Simul_esp_na)
  rm(Simul_esp_na, Simul_esp)

  # enlever les arbres martelés, on ne veut pas calculer leur volume
  SimulHtVol1 <- simul[which(simul$Residuel==0),]
  setnames(SimulHtVol1, old='essence_hauteur', new='essence')

  # calcul de st et dq pour la relation hd
  SimulHtVol1[, `:=`(
    sum_st_ha = sum(pi * (dhpcm/2/ 100)^2 * nb_tige * 25, na.rm = TRUE),
    dens = sum(nb_tige * 25, na.rm = TRUE)
  ), by = c('id_pe', 'step', 'iter')][, `:=`(
    dhp_moy = sqrt((sum_st_ha*40000)/(dens*pi))
  )]

  # ne garder que les variables nécessaires à la relation hd
  SimulHtVol1 <- SimulHtVol1[, .(iter, id_pe, no_arbre, step, Annee, essence, dhpcm, reg_eco, altitude, p_tot, veg_pot, t_ma, milieu, sum_st_ha, dhp_moy, essence_volume)]


  #### 3. Estimation de la hauteur et du volume des arbres ####


  resultats <- OutilsDRF::relation_h_d(fic_arbres=SimulHtVol1, mode_simul='STO', nb_iter=NbIter, nb_step=Horizon+1, reg_eco = TRUE, dt =5, use_ass_ess = FALSE)


  SimulHtVol1 <- resultats[, `:=` (essence=NULL)]
  setnames(SimulHtVol1, old='essence_volume', new='essence')
  resultats <- OutilsDRF::cubage(fic_arbres=SimulHtVol1, mode_simul='STO', nb_iter=NbIter, nb_step=Horizon+1, use_ass_ess = FALSE)

  # il ne faut pas séparer les arbres d'une placette/iter/step, car la fct relation_h_d calcule la st de la placette à partir des arbres
  # ok, ça car st de dq maintenant calculés avant
  # les residus du modèles sont corrélés dans le temps pour une meme placette/iter, donc si on sépare les step d'une placette, ça ne fonctionnera plus
  # les effets fixes sont générés pour être appliqués à tous les arbre/placette/step d'une même itération, donc si les placettes d'une même iter ne sont pas dans le même groupe, ça ne marchera pas
  # effet aléatoire de placettes, il ne faut pas séparer les arbres/step d'une même placette
  # donc il faut regrouper des itérations
  # même chose pour le cubage


  #### 4. Préparation du fichier final ####

  # Garder juste les variables de hauteur et volume
  resultats <- resultats[, .(id_pe, Annee, iter, no_arbre, hauteur_pred, vol_dm3)]
  setnames(resultats,
           old = c("id_pe", "no_arbre", "iter"),
           new = c("Placette", "ArbreID", "Iter"))

  # Joindre avec Simul, et ça va aussi remettre les arbres martelés
  SimulHtVol <- merge(simul_data, resultats, by = c("Iter","Placette","Annee","ArbreID"), all.x = T)

  # # s'il y avait des Residuel=1, refaire le tri, sinon pas nécessaire
  # martele <- simul_data[which(simul$Residuel==1),]
  # if (nrow(martele>0)) {
  #   setorder(SimulHtVol, Iter, Placette, Annee, Residuel, ArbreID)
  # }

  # metre à NA les volumes des non commerciaux
  SimulHtVol[, `:=`(
    vol_dm3 = fifelse(Espece %in% c("AME","AUR","ERE","ERG","ERP","MAS","PRP","SAL","SOA","SOD"), NA, vol_dm3)
  )]

  return(SimulHtVol)

}
