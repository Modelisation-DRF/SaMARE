#' Fonction qui extrait la vigueur à partir du code de vigueur au début de la simulation.
#' Si pas de code de vigueur, utilise les équations de conversion MSCR à vigueur
#'
#' @param Data Un dataframe contenant une ligne par arbre avec au minimum les colonnes Vigueur, GrEspece, MSCR, DHPcm
#' @param Para.ConvMSCRVig Un dataframe  contenant les paramettres des équations
#'                         de conversion du classement MSCR en vigueur.
#' @return Retourne Data avec la colonne vigu0 ("ViG" ou "NONVIG").
#'
#' @export
#'

AttribVigu0<-function(Data,Para.ConvMSCRVig){
  #Data = Plac

  select=dplyr::select

  Data_vig <- Data %>% filter(!is.na(Vigueur)) # arbres avec vigueur
  Data_sans_vig <- Data %>% filter(is.na(Vigueur)) # arbres sans vigueur

  #if (is.na(Data$Vigueur)==FALSE){ # s'il n'y a pas de données manquantes dans la colonne Vigueur, on peut créer directement la colonne vigu0
  if (nrow(Data_vig)>0) { # s'il n'y a pas de données manquantes dans la colonne Vigueur, on peut créer directement la colonne vigu0

    #vigu0<-ifelse(Data$Vigueur %in% c(1,2,5),"ViG","NONVIG")
    #Data_vig[, vigu0 := fifelse(Vigueur %in% c(1,2,5), "ViG", "NONVIG")]
    Data_vig <- Data_vig %>% mutate(vigu0 = ifelse(Vigueur %in% c(1,2,5), "ViG", "NONVIG"))

  }

  if (nrow(Data_sans_vig)>0) { # s'il y a des données manquantes dans Vigueur

    #if (is.na(Data$MSCR)==FALSE){ # s'il y a une valeur MSCR, on s'en sert pour calculer vigu0
    Data_sans_vig_mscr <- Data_sans_vig %>% filter(!is.na(MSCR)) # avec MSCR
    Data_sans_vig_sans_mscr <- Data_sans_vig %>% filter(is.na(MSCR)) # sans MSCR

    #if (is.na(Data_sans_vig$MSCR)==FALSE){ # s'il y a une valeur MSCR, on s'en sert pour calculer vigu0
    if (nrow(Data_sans_vig_mscr)>0){ # s'il y a une valeur MSCR, on s'en sert pour calculer vigu0

      n <- nrow(Data_sans_vig_mscr)

      Data_sans_vig_mscr$GrEspeceMSCR <- ifelse(Data_sans_vig_mscr$GrEspece=="AUT", "FIN",
                                                ifelse(Data_sans_vig_mscr$GrEspece=="EPX", "RES",
                                                       Data_sans_vig_mscr$GrEspece))

      listeMSCR<-c(rep("R",n),rep("C",n),rep("S",n),rep("M",n),rep("CR",n),rep("MS",n))
      listeEss<-c(rep("BOJ",n),rep("ERR",n),rep("ERS",n),rep("FEN",n),rep("FIN",n),
                  rep("HEG",n),rep("RES",n),rep("SAB",n))

      # Construction matrice X
      XConvMSCRVig<-matrix(0,ncol=16,nrow=n)
      XConvMSCRVig[,1]<-1
      XConvMSCRVig[,2:7]<-(Data_sans_vig_mscr$MSCR==listeMSCR)*1
      XConvMSCRVig[,8:15]<-(Data_sans_vig_mscr$GrEspeceMSCR==listeEss)*1
      XConvMSCRVig[,16]<-Data_sans_vig_mscr$DHPcm

      # selectionner les parametres de conversion en vigeur
      Para.ConvMSCRVig<-Para.ConvMSCRVig

      # Construction matrice beta
      BetaMat <- matrix(Para.ConvMSCRVig$ParameterEstimate,ncol=1)

      # Calcul
      logit <- as.vector(XConvMSCRVig %*% BetaMat)
      pred <- exp(logit)/(1+exp(logit))
      vigu0 <- ifelse(pred>=runif(n=1),"ViG","NONVIG")
      Data_sans_vig_mscr$vigu0 = vigu0

    }

  if (nrow(Data_sans_vig_sans_mscr)>0){ # s'il n'y a pas de valeur MSCR, on met à vigu0="ViG"
        #Data_sans_vig_sans_mscr[, vigu0 := "ViG"]
        Data_sans_vig_sans_mscr <- Data_sans_vig_sans_mscr %>% mutate(vigu0 = "ViG")
  }
    # on remet les sans vigueurs ensemble
    Data_sans_vig <- bind_rows(Data_sans_vig_mscr, Data_sans_vig_sans_mscr)
  }

  # remettre les fichiers ensemble
  result <- bind_rows(Data_vig, Data_sans_vig)
  setorder(result, Placette, NoArbre)

  return(result)
}
