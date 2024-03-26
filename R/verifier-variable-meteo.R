#
#
#
# vevifier_variable_meteo <- function(data){
#
#   select=dplyr::select
#
#   data <- data %>%
#     rename(id_pe = Placette, latitude = Latitude, longitude = Longitude)
#
#
#   mes_variables <- c('GrwDays', 'Ptot', 'Tmoy')
#   fonctions_validation <- list(valide_GrwDays, valide_Ptot, valide_Tmoy)
#   noms_remplacement <- c("growingseasonlength", "totalprecipitation", "tmean")
#
#
#   for (i in seq_along(mes_variables)) {
#     if (mes_variables[i] %in% names(data) && !fonctions_validation[[i]](data)) {
#       data <- select(data, -!!rlang::sym(mes_variables[i]))
#     }
#   }
#
#
#   variables_presentes <- intersect(mes_variables, names(data))
#   for (col_names in variables_presentes) {
#     if (!length(unique(data[[col_names]])) == 1) {
#       data <- select(data, -!!rlang::sym(col_names))
#     }
#   }
#
#
#   map_noms_variables <- c(GrwDays = "growingseasonlength",
#                           Ptot = "totalprecipitation",
#                           Tmoy = "tmean")
#
#   variables_non_trouvees <- setdiff(mes_variables, names(data))
#
#   if(!is_empty(variables_non_trouvees)){
#   variables_a_extraire <- map_noms_variables[variables_non_trouvees]
#
#     data <- extract_map_plot(file=data, liste_raster="cartes_climat", variable=variables_a_extraire)
#
#     if('tmean' %in% variables_a_extraire) {
#       data <- rename(data, Tmoy = tmean)
#     }
#
#     if('totalprecipitation' %in% variables_a_extraire) {
#       data <- rename(data, Ptot = totalprecipitation)
#     }
#
#     if('growingseasonlength' %in% variables_a_extraire) {
#       data <- rename(data, GrwDays = growingseasonlength)
#     }
# }
#
# data<-data %>% rename(Placette=id_pe)
#
#   return (data)
# }
