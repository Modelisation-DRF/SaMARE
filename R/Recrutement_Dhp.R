#'Fonction qui estime le DHP des recrues
#'
#' @param RecSelect Dataframe qui contient les prévisions du nombre de recrues par groupe d'espèces, avec au minimum les
#'                  colonnes Placette, GrEspece, st_tot0, dens_tot0, ntrt
#' @param t La longueur du pas de simulation en annees (en annees).
#' @param Para.rec_dhp  Un dataframe  contenant les paramettres du module de prévision du DHP des recrues.
#' @param RandomRecDhp Effet aléatoires du modèle de dhp des recrues
#' @param varRecDhp Variance résiduelle du modèle de dhp des recrues
#' @param theta Paramètre de la fonction de variance résiduelle du modèle de dhp des recrues
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return  Retourne RecSelect avec la colonne DHPcm1 et pred_dhp
#' @export
rec_dhp <- function(RecSelect, t, Para.rec_dhp, RandomRecDhp, varRecDhp, theta, seed_value=NULL){

  if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

  select=dplyr::select
  n<-nrow(RecSelect)

  #Liste des effets

  listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
              rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))

  # Construction matrice X
  Xrec_dhp<-matrix(0,ncol=15,nrow=n)
  Xrec_dhp[,1]<-1
  Xrec_dhp[,2:11]<-(RecSelect$GrEspece==listeEss)*1
  Xrec_dhp[,12]<-log(RecSelect$st_tot0)
  Xrec_dhp[,13]<-log(RecSelect$dens_tot0)
  Xrec_dhp[,14]<-log(t)
  Xrec_dhp[,15]<-RecSelect$ntrt


  # selectionner les parametres
  ParaRec_dhp<-Para.rec_dhp

  # Construction matrice beta
  BetaMat<-matrix(ParaRec_dhp$ParameterEstimate,ncol=1)

  # Calcul accroissement
  RecSelect$xb_dhp_rec <- as.numeric(Xrec_dhp %*% BetaMat)

  # ajouter les effets aléatoire de placettes
  setDT(RandomRecDhp)
  setDT(RecSelect)
  RecSelect <- RandomRecDhp[, .(Placette, RandomPlac, RandomStep)][RecSelect, on = .(Placette)]

  RecSelect <- RecSelect %>%
    mutate(pred_dhp = xb_dhp_rec,
           eijk = rnorm(n(), mean=0, sd=sqrt(varRecDhp*pred_dhp^theta)),
           DHPcm1 = ((pred_dhp + RandomPlac + RandomStep + eijk)^2+90)/10,
           DHPcm1 = ifelse(DHPcm1>=9.1,DHPcm1,9.1),
           DHPcm1 = round(DHPcm1 * 10) * 0.1) %>%
    select(-xb_dhp_rec,-eijk,-RandomStep, -RandomPlac)



  return(RecSelect)

}
