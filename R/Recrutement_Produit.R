#'Fonction qui prevoit si une recrue a un potentiel de sciage ou de pate.
#'
#' @param RecSelect Un dataframe qui contient une liste d'arbres avec les colonnes Placette, GrEspece, type_pe_Plac, rid1
#'                  par groupe d'espèce.
#' @param Para.rec_prod  Paramètres de l'équation de la probabilité de présence
#'                      de potentiel sciage des recrues de groupe d'essence feuillus.
#' @param RandomRecProd Effets aléatoires du modèle de produit des recrues
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return Retourne RecSelect avec les colonnes prob_prod_rec et prod1.
#' @export
rec_prod <- function(RecSelect, Para.rec_prod, RandomRecProd, seed_value=NULL){

  if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

  select=dplyr::select
  n<-nrow(RecSelect)

  #Liste des effets

  listeEss<-c(rep("AUT",n),rep("BOJ",n),rep("EPX",n),rep("ERR",n),rep("ERS",n),
              rep("FEN",n),rep("FIN",n),rep("HEG",n),rep("RES",n),rep("SAB",n))
  listeTypePe<-c(rep("type0",n),rep("type1",n),rep("type2",n))
  listeRid1<-c(rep("2o",n),rep("3a",n),rep("3b",n),rep("3c",n),rep("3d",n),
               rep("4e",n),rep("4o",n),rep("DU",n),rep("SV",n))

  # Construction matrice X
  Xrec_prod<-matrix(0,ncol=23,nrow=n)
  Xrec_prod[,1]<-1
  Xrec_prod[,2:11]<-(RecSelect$GrEspece==listeEss)*1
  Xrec_prod[,12:14]<-(RecSelect$type_pe_Plac==listeTypePe)*1
  Xrec_prod[,15:23]<-(RecSelect$rid1==listeRid1)*1


  # selectionner les parametres
  ParaRec_prod<-Para.rec_prod

  # Construction matrice beta
  BetaMat<-matrix(ParaRec_prod$ParameterEstimate,ncol=1)

  # Calcul accroissement
  RecSelect$xb_rec_prod <- as.numeric(Xrec_prod %*% BetaMat)

  # ajouter les effets aléatoires
  setDT(RandomRecProd)
  setDT(RecSelect)
  RecSelect <- RandomRecProd[, .(Placette, RandomPlac, RandomStep)][RecSelect, on = .(Placette)]

  RecSelect <- RecSelect %>%
    mutate(pred_prod = xb_rec_prod + RandomPlac + RandomStep,
           AleaProd = runif(n()),
           prob_prod_rec = exp(pred_prod)/(1+exp(pred_prod)),
           prod1 = ifelse(GrEspece %in% c("SAB","RES","EPX"), "resineux",
                          ifelse(GrEspece=="AUT"|(vigu1=="NONVIG"), "pate",
                                 ifelse(AleaProd<=prob_prod_rec, "sciage", "pate")))) %>%
    select(-pred_prod, -AleaProd, -RandomPlac, -RandomStep)

  return(RecSelect)

}
