#' nbha
#' Calcul le nombre d'arbres à l'ha que représente une ligne d'un data
#' @param nombre Nombre d'arbres dans la placette de superficie \code{superficie} que représente une ligne du data
#' @param superficie Superficie de la placette
#'
#' @return Nombre d'arbres à l'ha que représente une ligne d'un data
#'
#' @export
nbha <- function(nombre,superficie) {
  nombre/superficie
}

#' stm2ha
#' Calcul la surface terrière en m2/ha que représente une ligne d'un data
#' @param dhpcm DHP de l'arbre en cm
#' @param nombre Nombre d'arbres dans la placette de superficie \code{superficie} que représente une ligne du data
#' @param superficie Superficie de la placette
#'
#' @return surface terrière en m2/ha que représente une ligne d'un data
#'
#' @export
stm2ha <- function(dhpcm, nombre, superficie) {
  #pi*(dhpcm/200)^2 * nbha(nombre, superficie)
  pi*(dhpcm/200)^2 *  nombre/superficie
}


#' volm3ha
#' Calcul le volume en m3/ha que représente une ligne d'un data
#' @param voldm3 volume de l'arbre en dm3
#' @param nombre Nombre d'arbres dans la placette de superficie \code{superficie} que représente une ligne du data
#' @param superficie Superficie de la placette
#'
#' @return surface terrière en m2/ha que représente une ligne d'un data
#'
#' @export
volm3ha <- function(voldm3, nombre, superficie) {
  voldm3/1000 * nbha(nombre, superficie)
}

#' dqm
#' Calcul le diamètre quadratique moyen à partir de la la surface terrière et du nombre de tiges
#' @param stm2ha surface terrière en m2/ha
#' @param nbha Nombre d'arbres à l'ha
#'
#' @return diamètre quadratique moyen en cm
#'
#' @export
dqm <- function(stm2ha, nbha) {
  (stm2ha/nbha/pi)^0.5*200
}



#' Calculer les variables dendrométriques de placettes
#'
#' @param data Un dataframe dont les lignes sont des arbres individuels ou un groupe d'arbres, avec les colonnes dhp, voldm3, nombre, superficie
#' @param dhp Nom de la colonne dans \code{data} qui contient le DHP de l'arbre en cm
#' @param voldm3 Nom de la colonne dans \code{data} qui contient le volume de l'arbre en dm3. Mettre des NA dans la colonne si le volume n'est pas disponible
#' @param nombre Nom de la colonne dans \code{data} qui contient le nombre d'arbres dans la placette que représente une ligne
#' @param superficie Nom de la colonne dans \code{data} qui contient la superficie de la placette en ha
#' @param group_by_vars Vecteur contenant le nom des colonnes dans \code{data} à utiliser pour grouper les lignes pour le calcul des variables dendrométriques. ex: c('Placette','Annee','GrEspece'). Facultatif.
#'
#' @return Un dataframe avec une ligne par combinaison des variables de \code{group_by_vars}, et les colonnes ST_m2ha (surface terrière m2/ha),
#'         Vol_m3ha (volume marchand brut m3/ha), Ti_HA (nombre d'arbres à l'ha), DQM_cm (diamètre moyen quadratique en cm).
#' @export
calcul_var_dendro <- function(data, dhp, voldm3, nombre, superficie, group_by_vars=NULL){

  # data=fic; dhp=dhpcm; voldm3=vol; nombre=nb; superficie=sup; group_by_vars=c('placette','essence')

  # convertir les noms en symboles
  group_syms <- syms(group_by_vars)

  data1 <- data %>%
    lazy_dt() %>%
    {
      if (!is.null(group_by_vars) && length(group_by_vars) > 0) {
        group_by(., group_syms)
      } else {
        .
      }
    } %>%
    summarise(Ti_ha = sum(nbha({{nombre}},{{superficie}}), na.rm = F),
              ST_m2ha = sum(stm2ha({{dhp}}, {{nombre}}, {{superficie}}), na.rm = F),
              Vol_m3ha = sum(volm3ha({{voldm3}}, {{nombre}}, {{superficie}}), na.rm = F),
              .groups="drop") %>%
    mutate(DQM_cm = dqm(ST_m2ha, Ti_ha)) %>%
    as.data.frame()

  return(data1)

}


#' Calculer la hauteur dominante de placettes
#'
#' @param data Un dataframe dont les lignes sont des arbres individuels ou un groupe d'arbres, avec les colonnes hautm, nombre, superficie
#' @param ht Nom de la colonne dans \code{data} qui contient la hauteur de l'arbre en m.
#' @param nombre Nom de la colonne dans \code{data} qui contient le nombre d'arbres dans la placette que représente une ligne
#' @param superficie Nom de la colonne dans \code{data} qui contient la superficie de la placette en ha
#' @param group_by_vars Vecteur contenant le nom des colonnes dans \code{data} à utiliser pour grouper les lignes pour le calcul de la hauteur dominante. ex: c('Placette','Annee','GrEspece'). Facultatif.
#'
#' @return Un dataframe contenant, pour chaque placette, groupe d'espèces, année et itération,
#' la surface terrière, le volume marchand brut, le diamètre moyen quadratique et la hauteur dominante à l'ha.
#' @export
calcul_hdom <- function(data, ht, nombre, superficie, group_by_vars=NULL){

  # convertir les noms en symboles
  group_syms <- syms(group_by_vars)

  # pour le arrange
  arrange_exprs <- c(group_syms, expr(desc({{ht}})))

  data1 <- data %>%
    lazy_dt() %>%
    mutate(nb = nbha({{nombre}},{{superficie}})) %>%
    arrange(!!!arrange_exprs)%>%
    {
      if (!is.null(group_by_vars) && length(group_by_vars) > 0) {
        group_by(., group_syms)
      } else {
        .
      }
    } %>%
    mutate(NbCum = cumsum(nb),
           NbCum_prec = lag(NbCum),
           NbCum_prec = ifelse(is.na(NbCum_prec), 0, NbCum_prec),
           select_id = ifelse(NbCum_prec>=100 & NbCum>100, 0, 1)) %>%
    filter(select_id==1) %>%
    mutate(poids = ifelse(NbCum<=100, nb, 100-NbCum_prec)) %>%
    summarise(nb_tige = round(as.numeric(sum(poids)), 0), # il y avait un problème d'arrondi lorsqu'il y avait des fractions d'arbres, même si la somme des poids etait 4, la selection >4 ne fonctionnait pas
              HDom_m = sum({{ht}}*poids)/sum(poids),
              .groups="drop") %>%
    mutate(HDom_m = ifelse(nb_tige<100, NA, HDom_m)) %>%
    select(-nb_tige) %>%
    as.data.frame()

  return(data1)

}




