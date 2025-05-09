#' Fonction qui attribue la classe MSCR d'un arbre à partir de son code de vigueur.
#'
#' @param Data Un dataframe contenant une ligne par arbre pour lesquels on
#'              veut attribuer le code MSCR (au minimum les variables Placette, ArbreID, Residuel, GrEspece, vigu0, prod0, DHPcm)
#' @param Para.ConvVigMSCR Un dataframe contenant les paramètres des équations
#'                         de conversion de vigueur en MSCR.
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#' @return Retourne Data avec les colonnes MSCR, probM, probS, probC.
#' @details
#' L'équation pour les 6 feuillus ("BOJ","ERR","ERS","FEN","FIN","HEG") est celle calibrée par François Guillemette en 2013 :
#' Voir fichier Modèles de conversion de format ABCD MSCR 1234_20130814.xls, feuille "MSCR selon vigueur - Feuillu.
#' Pour les résineux (SAB, RES, EPX), c'est aussi une équation par FG, dans la feuille "MSCR selon vigueur - Rés-EPX"
#'
#' @import data.table
#' @export
AttribMSCR<-function(Data,Para.ConvVigMSCR, seed_value=NULL){
  # Data = data; Para.ConvVigMSCR=  Para;
  # Data=data_test; Para.ConvVigMSCR=Para.mscr; seed_value=3;

  # les 10 groupes d'essences de samare-2018: BOJ ERR ERS FIN FEN HEG    RES SAB EPX    AUT
  # les groupes avec equations MSCR:  #       BOJ ERR ERS FIN FEN HEG    RES SAB EPX    avec FIN

  if (length(seed_value)>0) {set.seed(seed_value)} # on a besoin d'un seed pour les test, car cette fonction génère des nombres aléatoires

  select=dplyr::select

  # Faire un fichier avec les essence feuillus
  # ajouter AUT car il sera traité comme FIN
  Data_feu <- Data[Data$GrEspece %in% c("BOJ", "ERR", "ERS", "FEN", "FIN", "HEG", "AUT"),]

  # faire un fichier avec les autres essences résineuses
  Data_res <- Data[Data$GrEspece %in% c("SAB", "RES", "EPX"),]


  if (nrow(Data_feu)>0){

    n <- nrow(Data_feu)
    Data_feu <- Data_feu %>% mutate(GrEspece_copie = GrEspece)
    Data_feu$GrEspece_copie <- ifelse(Data_feu$GrEspece_copie=="AUT", "FIN", Data_feu$GrEspece_copie) # AUT principalement du PRP, donc FIN
    listeEss <- c(rep("ERS",n),rep("BOJ",n),rep("ERR",n),rep("HEG",n),rep("FIN",n)) # le paramètre de FEN est à 0

  # Construction matrice X
  XConvVigMSCR<-matrix(0,ncol=9,nrow=n)
  XConvVigMSCR[,1]<-1
  XConvVigMSCR[,2]<-(Data_feu$vigu0=="ViG")*1
  XConvVigMSCR[,3]<-(Data_feu$prod0=="sciage")*1
  XConvVigMSCR[,4]<-(Data_feu$DHPcm*10)
  XConvVigMSCR[,5:9]<-(Data_feu$GrEspece_copie==listeEss)*1

  # selectionner les parametres de conversion en vigeur (pas stochastique)
  Para.ConvVigMSCR1 <- Para.ConvVigMSCR %>% filter(SubModuleID==20 & response==1) # BOJ ERR ERS FIN HEG (intercept FEN)
  Para.ConvVigMSCR2 <- Para.ConvVigMSCR %>% filter(SubModuleID==20 & response==2)
  Para.ConvVigMSCR3 <- Para.ConvVigMSCR %>% filter(SubModuleID==20 & response==3)

  # Construction matrice beta
  BetaMat1<-matrix(Para.ConvVigMSCR1$ParameterEstimate,ncol=1)
  BetaMat2<-matrix(Para.ConvVigMSCR2$ParameterEstimate,ncol=1)
  BetaMat3<-matrix(Para.ConvVigMSCR3$ParameterEstimate,ncol=1)

  # Calcul
  Data_feu$pred1 <- exp(as.vector(XConvVigMSCR %*% BetaMat1))
  Data_feu$pred2 <- exp(as.vector(XConvVigMSCR %*% BetaMat2))
  Data_feu$pred3 <- exp(as.vector(XConvVigMSCR %*% BetaMat3))

  Data_feu <- Data_feu %>% select(-GrEspece_copie)

  }

  # traitement des résineux
  if (nrow(Data_res)>0) {

            n<-nrow(Data_res)

            listeEss<-c(rep("SAB",n),rep("RES",n)) # EPX est l'intercept

            # Construction matrice X
            XConvVigMSCR<-matrix(0,ncol=5,nrow=n)
            XConvVigMSCR[,1]<-1
            XConvVigMSCR[,2]<-(Data_res$vigu0=="ViG")*1
            XConvVigMSCR[,3:4]<-(Data_res$GrEspece==listeEss)*1
            XConvVigMSCR[,5]<-Data_res$DHPcm

            # selectionner les parametres de conversion en vigeur
            Para.ConvVigMSCR1<-Para.ConvVigMSCR %>% filter(SubModuleID==21 & response==1) # RES SAB (intercept EPX)
            Para.ConvVigMSCR2<-Para.ConvVigMSCR %>% filter(SubModuleID==21 & response==2)
            Para.ConvVigMSCR3<-Para.ConvVigMSCR %>% filter(SubModuleID==21 & response==3)

            # Construction matrice beta
            BetaMat1<-matrix(Para.ConvVigMSCR1$ParameterEstimate,ncol=1)
            BetaMat2<-matrix(Para.ConvVigMSCR2$ParameterEstimate,ncol=1)
            BetaMat3<-matrix(Para.ConvVigMSCR3$ParameterEstimate,ncol=1)

            # Calcul
            Data_res$pred1 <-exp(as.vector(XConvVigMSCR %*% BetaMat1))
            Data_res$pred2 <-exp(as.vector(XConvVigMSCR %*% BetaMat2))
            Data_res$pred3 <-exp(as.vector(XConvVigMSCR %*% BetaMat3))



    }

  #}

  # remettre les 2 fichiers ensemble
  #result <- rbind(Data_feu,Data_res, fill=TRUE)
  result <- bind_rows(Data_feu,Data_res)
  setDT(result)
  #setorder(result, Placette, Annee, Residuel, ArbreID, Iter)
  setorder(result, Placette,Annee,Residuel,ArbreID,Iter)

  # Calculer le code MSCR
  result[, probM := pred1 / (1 + pred1 + pred2 + pred3)]
  result[, probS := probM + pred2 / (1 + pred1 + pred2 + pred3)]
  result[, probC := probS + pred3 / (1 + pred1 + pred2 + pred3)]
  result[, Alea := runif(.N)]
  result[, `:=`(
    MSCR = fifelse(Alea < probM, "M",
                   fifelse(Alea <probS, "S",
                           fifelse(Alea < probC, "C", "R")))
  )]


  return(result)
}
