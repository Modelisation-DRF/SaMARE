#'Fonction qui prévoit le prédicteur linéaire de l'équation du DHP des recrues
#'
#'
#' @param RecSelect Dataframe qui contient les préviions du nomobre de recrues par
#'                  groupe d'espèces.
#' @param dens_tot0 Densitée totale de la placette en nb de tiges marchandes
#'                   (>9.0cm) par hectare au début du pas de simulation.
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param ntrt  Nombre de traitements de coupes partielles précédement
#'               effectuées sur la placette.
#' @param Iterj  Itération en cours.
#' @param st_tot0  La surface terrière marchande de la placette en m2/ha au
#'                 début du pas de simulation.
#' @param Para.rec_dhp  Un dataframe  contenant les paramettres du module
#'                      de prévision du DHP des recrues.
#' @return  Retourne le prédicteur linéaire de l'équation du DHP des recrues
#' @export
rec_dhp<-function(RecSelect,st_tot0,dens_tot0,t,ntrt,Iterj,Para.rec_dhp){
  select=dplyr::select
  n<-nrow(RecSelect)

  #Liste des effets

  listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
              rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))

  # Construction matrice X
  Xrec_dhp<-matrix(0,ncol=15,nrow=n)
  Xrec_dhp[,1]<-1
  Xrec_dhp[,2:11]<-(RecSelect$GrEspece==listeEss)*1
  Xrec_dhp[,12]<-log(st_tot0)
  Xrec_dhp[,13]<-log(dens_tot0)
  Xrec_dhp[,14]<-log(t)
  Xrec_dhp[,15]<-ntrt




  # selectionner les parametres d'accroissement de la vp et du groupe d'essences de l'arbre
  ParaRec_dhp<-Para.rec_dhp %>%
    filter(Iter==Iterj)
  # Construction matrice beta
  BetaMat<-matrix(ParaRec_dhp$ParameterEstimate,ncol=1)

  # Calcul accroissement
  Xbeta <-Xrec_dhp %*% BetaMat

  return(Xbeta)

}
