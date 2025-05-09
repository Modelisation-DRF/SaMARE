#' Fonction qui extrait le produit à partir du code de vigueur au début de la simulation.
#' Si pas de code de vigueur, utilise les équations de conversion MSCR à produit
#'
#' @param Data Un dataframe contenant une ligne par arbre avec au minimum les colonnes Placette, ArbreID, Vigueur, GrEspece, MSCR, DHPcm
#' @param Para.ConvMSCRProd1024 Un dataframe  contenant les paramettres des équations
#'                         de conversion du classement MSCR en produits pour les arbres de moins de 23.1 cm.
#' @param Para.ConvMSCRProd24 Un dataframe  contenant les paramettres des équations
#'                         de conversion du classement MSCR en produits pour les arbres de plus de 23 cm.
#' @return Retourne Data avec la colonne prod0 ("sciage" ou "pate" ou "resineux").
#'
#' @export
#'

AttribProd0<-function(Data,Para.ConvMSCRProd1024,Para.ConvMSCRProd24){
  # Data=Plac
  select=dplyr::select

  # traiter les résineux séparément des feuillus
  Data_res <- Data %>% filter(GrEspece %in% c("EPX","RES","SAB"))
  Data_feu <- Data %>% filter(!(GrEspece %in% c("EPX","RES","SAB")))

  #if(Data$GrEspece %in% c("EPX","RES","SAB")){
  if (nrow(Data_res)>0){ # s'il y a des résineux dans le data

    Data_res$prod0 <- "resineux"

  }#else{

  if (nrow(Data_feu)>0){ # s'il y a des feuillus dans le data

    # traiter avec et sans vigueur séparément
    Data_feu_vig <- Data_feu %>% filter(!is.na(Vigueur))
    Data_feu_sans_vig <- Data_feu %>% filter(is.na(Vigueur))

        #if (is.na(Data$Vigueur)==FALSE){
    if (nrow(Data_feu_vig)>0){ # si la vigueur est présente

      Data_feu_vig$prod0 = ifelse(Data_feu_vig$Vigueur %in% c(1,3), "sciage",
                                  ifelse(Data_feu_vig$Vigueur %in% c(2,4), "pate", "resineux"))

    }

    #else{
    if (nrow(Data_feu_sans_vig)>0){ # si la vigueur n'est pas présente

      # traiter avec et sans mscr séparément et selon le dhp
      Data_feu_sans_vig_mscr_lt23 <- Data_feu_sans_vig %>% filter(!is.na(MSCR) & DHPcm<23.1)
      Data_feu_sans_vig_mscr_gt23 <- Data_feu_sans_vig %>% filter(!is.na(MSCR) & DHPcm>=23.1)
      Data_feu_sans_vig_sans_mscr <- Data_feu_sans_vig %>% filter(is.na(MSCR))

      #if (is.na(Data$MSCR)==FALSE & Data$DHPcm<23.1){
      if (nrow(Data_feu_sans_vig_mscr_lt23)>0) { # si mscr et , 23.1 cm

                  n<-nrow(Data_feu_sans_vig_mscr_lt23)

                  Data_feu_sans_vig_mscr_lt23$GrEspeceMSCR<-ifelse(Data_feu_sans_vig_mscr_lt23$GrEspece=="AUT","FIN",
                                                                   ifelse(Data_feu_sans_vig_mscr_lt23$GrEspece=="EPX","RES",
                                                                          ifelse(Data_feu_sans_vig_mscr_lt23$GrEspece=="ERR","FIN",Data_feu_sans_vig_mscr_lt23$GrEspece)))

                  Data_feu_sans_vig_mscr_lt23$GrMSCR<-ifelse(Data_feu_sans_vig_mscr_lt23$GrEspeceMSCR %in% c("ERR","FEN","FIN","HEG","RES","SAB") & Data_feu_sans_vig_mscr_lt23$MSCR %in% c("M","S","MS"),"MS",
                                                             ifelse(Data_feu_sans_vig_mscr_lt23$GrEspeceMSCR %in% c("ERR","FEN","FIN","HEG","RES","SAB") & Data_feu_sans_vig_mscr_lt23$MSCR %in% c("C","R","CR"),"CR",
                                                                    ifelse(Data_feu_sans_vig_mscr_lt23$GrEspeceMSCR %in% c("ERS","BOJ") & Data_feu_sans_vig_mscr_lt23$MSCR %in% c("M","S","MS"),"MS",Data_feu_sans_vig_mscr_lt23$MSCR)))

                listeGrMSCR<-c(rep("R",n),rep("C",n),rep("CR",n),rep("MS",n))
                listeEss<-c(rep("BOJ",n),rep("ERS",n),rep("FEN",n),rep("FIN",n), rep("HEG",n))
                listeInterMSCR1<-c(rep("R",n*2),rep("C",n*2))
                listeInterEss1<-c(rep("BOJ",n),rep("ERS",n))
                listeInterMSCR2<-c(rep("CR",n*5))
                listeInterMSCR3<-c(rep("MS",n*4))
                listeInterEss1<-c(rep("BOJ",n),rep("ERS",n))
                listeInterEss2<-c(rep("BOJ",n),rep("ERS",n),rep("FEN",n),rep("FIN",n),rep("HEG",n))
                listeInterEss3<-c(rep("BOJ",n),rep("ERS",n),rep("FIN",n),rep("HEG",n))

              # Construction matrice X
                XConvMSCRProd1024<-matrix(0,ncol=30,nrow=n)
                XConvMSCRProd1024[,1]<-1
                XConvMSCRProd1024[,2]<-Data_feu_sans_vig_mscr_lt23$DHPcm
                XConvMSCRProd1024[,3:6]<-(Data_feu_sans_vig_mscr_lt23$GrMSCR==listeGrMSCR)*1
                XConvMSCRProd1024[,7:11]<-(Data_feu_sans_vig_mscr_lt23$GrEspeceMSCR==listeEss)*1
                XConvMSCRProd1024[,12:16]<-(Data_feu_sans_vig_mscr_lt23$GrEspeceMSCR==listeEss)*Data_feu_sans_vig_mscr_lt23$DHPcm
                XConvMSCRProd1024[,17:20]<-(Data_feu_sans_vig_mscr_lt23$GrMSCR==listeInterMSCR1 & Data_feu_sans_vig_mscr_lt23$GrEspeceMSCR==listeInterEss1)*1
                XConvMSCRProd1024[,21:25]<-(Data_feu_sans_vig_mscr_lt23$GrMSCR==listeInterMSCR2 & Data_feu_sans_vig_mscr_lt23$GrEspeceMSCR==listeInterEss2)*1
                XConvMSCRProd1024[,26:29]<-(Data_feu_sans_vig_mscr_lt23$GrMSCR==listeInterMSCR3 & Data_feu_sans_vig_mscr_lt23$GrEspeceMSCR==listeInterEss3)*1
                XConvMSCRProd1024[,30]<-Data_feu_sans_vig_mscr_lt23$DHPcm^2

               # selectionner les parametres de conversion en vigeur
               #Para.ConvMSCRProd1024<-Para.ConvMSCRProd1024

               # Construction matrice beta
                BetaMat<-matrix(Para.ConvMSCRProd1024$ParameterEstimate,ncol=1)

              # Calcul
              logit <- as.vector(XConvMSCRProd1024 %*% BetaMat)

              pred <- exp(logit)/(1+exp(logit))

              prod0 <- ifelse(pred>=runif(n=1),"sciage","pate")
              Date_feu_sans_vig_mscr_lt23$prod0 <- prod0

              }

      #else{
      if (nrow(Data_feu_sans_vig_mscr_gt23)) { # si msrc et dhp>23.1

          #if (is.na(Data$MSCR)==FALSE & Data$DHPcm>=23.1){

            n<-nrow(Data_feu_sans_vig_mscr_gt23)

            Data_feu_sans_vig_mscr_gt23$GrEspeceMSCR<-ifelse(Data_feu_sans_vig_mscr_gt23$GrEspece=="AUT","FIN",
                                      ifelse(Data_feu_sans_vig_mscr_gt23$GrEspece=="EPX","RES",
                                             ifelse(Data_feu_sans_vig_mscr_gt23$GrEspece=="ERR","FIN",Data_feu_sans_vig_mscr_gt23$GrEspece)))

            Data_feu_sans_vig_mscr_gt23$GrMSCR<-ifelse(Data_feu_sans_vig_mscr_gt23$GrEspeceMSCR %in% c("ERR","FEN","FIN","HEG","RES","SAB") & Data_feu_sans_vig_mscr_gt23$MSCR %in% c("M","S","MS"),"MS",
                                                       ifelse(Data_feu_sans_vig_mscr_gt23$GrEspeceMSCR %in% c("ERR","FEN","FIN","HEG","RES","SAB") & Data_feu_sans_vig_mscr_gt23$MSCR %in% c("C","R","CR"),"CR",
                                                              ifelse(Data_feu_sans_vig_mscr_gt23$GrEspeceMSCR %in% c("ERS","BOJ") & Data_feu_sans_vig_mscr_gt23$MSCR %in% c("M","S","MS"),"MS",Data_feu_sans_vig_mscr_gt23$MSCR)))

            listeGrMSCR<-c(rep("R",n),rep("C",n),rep("CR",n),rep("MS",n))
            listeEss<-c(rep("BOJ",n),rep("ERS",n),rep("FIN",n),rep("HEG",n))

            # Construction matrice X
            XConvMSCRProd24<-matrix(0,ncol=11,nrow=n)
            XConvMSCRProd24[,1]<-1
            XConvMSCRProd24[,2:5]<-(Data_feu_sans_vig_mscr_gt23$GrMSCR==listeGrMSCR)*1
            XConvMSCRProd24[,10]<-Data_feu_sans_vig_mscr_gt23$DHPcm
            XConvMSCRProd24[,11]<-Data_feu_sans_vig_mscr_gt23$DHPcm^2

            # selectionner les parametres de conversion en vigeur
            #Para.ConvMSCRProd24<-Para.ConvMSCRProd24

            # Construction matrice beta
            BetaMat<-matrix(Para.ConvMSCRProd24$ParameterEstimate,ncol=1)

            # Calcul
            logit <-as.vector(XConvMSCRProd24 %*% BetaMat)

            pred<-exp(logit)/(1+exp(logit))

            prod0<-ifelse(pred>=runif(n=1),"sciage","pate")
            Data_feu_sans_vig_mscr_gt23$prod0 <- prod0

      }

      #else{
      if (nrow(Data_feu_sans_vig_sans_mscr)>0) { # feuillus sans vigueur et sans mscr

        Data_feu_sans_vig_sans_mscr$prod0 = "pate"

      }
      # remettre les feuillus sans vigueur ensemble
      Data_feu_sans_vig <- bind_rows(Data_feu_sans_vig_mscr_lt23, Data_feu_sans_vig_mscr_gt23, Data_feu_sans_vig_sans_mscr)

    }
    # remettre les feuillus sans vigueurs avec les feuillus avec vigueurs
    Data_feu <- bind_rows(Data_feu_sans_vig, Data_feu_vig)

  }
  # remettre les res et feu ensemble
  result <- bind_rows(Data_feu, Data_res)
  setorder(result, Placette, NoArbre)


  return(result)

}
