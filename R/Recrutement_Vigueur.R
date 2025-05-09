#'Fonction qui prevoit si une recrue est vigoureuse ou non
#'
#' @param RecSelect  Un dataframe qui contient une liste d'arbres avec les colonnes Placette, GrEspece, latitude .
#' @param Para.rec_vig  Paramètres de l'équation de la probabilité que les recrues
#'                     soit vigoureuses.
#' @param RandomRecVig Effets aléatoire du modèle de vigueur des recrues
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return  Retourne RecSelect avec les colonnes prob_vig_rec et vigu1.
#' @export

rec_vig <- function(RecSelect, Para.rec_vig, RandomRecVig, seed_value=NULL){

  if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

  select=dplyr::select
  n<-nrow(RecSelect)

  #Liste des effets

  listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
              rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))

  # Construction matrice X
  Xrec_vig<-matrix(0,ncol=12,nrow=n)
  Xrec_vig[,1]<-1
  Xrec_vig[,2:11]<-(RecSelect$GrEspece==listeEss)*1
  Xrec_vig[,12]<-RecSelect$latitude

  # selectionner les parametres
  ParaRec_vig<-Para.rec_vig

  # Construction matrice beta
  BetaMat<-matrix(ParaRec_vig$ParameterEstimate,ncol=1)

  # logit
  RecSelect$xb_rec_vig <- as.numeric(Xrec_vig %*% BetaMat)


  # ajouter les effets aléatoires
  setDT(RandomRecVig)
  setDT(RecSelect)
  RecSelect <- RandomRecVig[, .(Placette, RandomPlac, RandomStep)][RecSelect, on = .(Placette)]

  # calcul probabilité de vigueur et transfo en vig/non-vig
  RecSelect <- RecSelect %>%
    mutate(pred_vig = xb_rec_vig + RandomPlac + RandomStep,
           AleaVig = runif(n()),
           prob_vig_rec = exp(pred_vig)/(1+exp(pred_vig)),
           vigu1 = ifelse(AleaVig <= prob_vig_rec, "ViG", "NONVIG")) %>%
    select(-pred_vig, -AleaVig, -RandomPlac, -RandomStep)

  return(RecSelect)

}
