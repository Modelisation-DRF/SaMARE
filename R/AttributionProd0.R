#' Fonction qui attribut les produits au début de la simulation. La fonction
#' cherche dabord si la tige est résineuse ensuite si le classement
#' vigueur 1,2,3,4,5,6 est fourni sinon elle cherche le classement MSCR pour
#' attribuer le produit sinon il classe l'arbre comme pate.
#'
#' @param Data Un dataframe contenant une ligne par arbre pour lesquels on
#'              veut attribuer le vigueur.
#' @param Para.ConvMSCRProd1024 Un dataframe  contenant les paramettres des équations
#'                         de conversion du classement MSCR en produits pour les arbres de moins de 23.1 cm.
#'@param Para.ConvMSCRProd24 Un dataframe  contenant les paramettres des équations
#'                         de conversion du classement MSCR en produits pour les arbres de plus de 23 cm.
#' @return Retourne la classe de produit de l'arbre.
#'
#' @export
#'

AttribProd0<-function(Data,Para.ConvMSCRProd1024,Para.ConvMSCRProd24){

  select=dplyr::select

  if(Data$GrEspece %in% c("EPX","RES","SAB")){

  prod0<-"resineux"

  }else{

        if (is.na(Data$Vigueur)==FALSE){

            prod0=ifelse(Data$Vigueur %in% c(1,3),"sciage",
            ifelse(Data$Vigueur %in% c(2,4),"pate","resineux"))

        }else{

              if (is.na(Data$MSCR)==FALSE & Data$DHPcm<23.1){

                  n<-nrow(Data)

                   Data$GrEspeceMSCR<-ifelse(Data$GrEspece=="AUT","FIN",
                                     ifelse(Data$GrEspece=="EPX","RES",
                                           ifelse(Data$GrEspece=="ERR","FIN",Data$GrEspece)))

                Data$GrMSCR<-ifelse(Data$GrEspeceMSCR %in% c("ERR","FEN","FIN","HEG","RES","SAB") & Data$MSCR %in% c("M","S","MS"),"MS",
                                  ifelse(Data$GrEspeceMSCR %in% c("ERR","FEN","FIN","HEG","RES","SAB") & Data$MSCR %in% c("C","R","CR"),"CR",
                                        ifelse(Data$GrEspeceMSCR %in% c("ERS","BOJ") & Data$MSCR %in% c("M","S","MS"),"MS",Data$MSCR)))

                listeGrMSCR<-c(rep("R",n),rep("C",n),rep("CR",n),rep("MS",n))
                listeEss<-c(rep("BOJ",n),rep("ERS",n),rep("FEN",n),rep("FIN",n),
                  rep("HEG",n))
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
                XConvMSCRProd1024[,2]<-Data$DHPcm
                XConvMSCRProd1024[,3:6]<-(Data$GrMSCR==listeGrMSCR)*1
                XConvMSCRProd1024[,7:11]<-(Data$GrEspeceMSCR==listeEss)*1
                XConvMSCRProd1024[,12:16]<-(Data$GrEspeceMSCR==listeEss)*Data$DHPcm
                XConvMSCRProd1024[,17:20]<-(Data$GrMSCR==listeInterMSCR1 & Data$GrEspeceMSCR==listeInterEss1)*1
                XConvMSCRProd1024[,21:25]<-(Data$GrMSCR==listeInterMSCR2 & Data$GrEspeceMSCR==listeInterEss2)*1
                XConvMSCRProd1024[,26:29]<-(Data$GrMSCR==listeInterMSCR3 & Data$GrEspeceMSCR==listeInterEss3)*1
                XConvMSCRProd1024[,30]<-Data$DHPcm^2

               # selectionner les parametres de conversion en vigeur
               Para.ConvMSCRProd1024<-Para.ConvMSCRProd1024

               # Construction matrice beta
                BetaMat<-matrix(Para.ConvMSCRProd1024$ParameterEstimate,ncol=1)

              # Calcul
              logit <-as.vector(XConvMSCRProd1024 %*% BetaMat)

              pred<-exp(logit)/(1+exp(logit))

              prod0<-ifelse(pred>=runif(n=1),"sciage","pate")

        }else{

          if (is.na(Data$MSCR)==FALSE & Data$DHPcm>=23.1){

            n<-nrow(Data)

            Data$GrEspeceMSCR<-ifelse(Data$GrEspece=="AUT","FIN",
                                      ifelse(Data$GrEspece=="EPX","RES",
                                             ifelse(Data$GrEspece=="ERR","FIN",Data$GrEspece)))

            Data$GrMSCR<-ifelse(Data$GrEspeceMSCR %in% c("ERR","FEN","FIN","HEG","RES","SAB") & Data$MSCR %in% c("M","S","MS"),"MS",
                                ifelse(Data$GrEspeceMSCR %in% c("ERR","FEN","FIN","HEG","RES","SAB") & Data$MSCR %in% c("C","R","CR"),"CR",
                                       ifelse(Data$GrEspeceMSCR %in% c("ERS","BOJ") & Data$MSCR %in% c("M","S","MS"),"MS",Data$MSCR)))

            listeGrMSCR<-c(rep("R",n),rep("C",n),rep("CR",n),rep("MS",n))
            listeEss<-c(rep("BOJ",n),rep("ERS",n),rep("FIN",n),rep("HEG",n))

            # Construction matrice X
            XConvMSCRProd24<-matrix(0,ncol=11,nrow=n)
            XConvMSCRProd24[,1]<-1
            XConvMSCRProd24[,2:5]<-(Data$GrMSCR==listeGrMSCR)*1
            XConvMSCRProd24[,10]<-Data$DHPcm
            XConvMSCRProd24[,11]<-Data$DHPcm^2

            # selectionner les parametres de conversion en vigeur
            Para.ConvMSCRProd24<-Para.ConvMSCRProd24

            # Construction matrice beta
            BetaMat<-matrix(Para.ConvMSCRProd24$ParameterEstimate,ncol=1)

            # Calcul
            logit <-as.vector(XConvMSCRProd24 %*% BetaMat)

            pred<-exp(logit)/(1+exp(logit))

            prod0<-ifelse(pred>=runif(n=1),"sciage","pate")

          }else{

            prod0="pate"
          }
        }
      }
    }

  return(prod0)
}
