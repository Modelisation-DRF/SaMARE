#' Fonction qui attribut la classe MSCR. La fonction
#' applique les équation de 2006 pour les feuillus puis celle de SaMARE2018
#' pour les résineux puisquelle sépare les épinettes des résineux.
#'
#' @param Data Un dataframe contenant une ligne par arbre pour lesquels on
#'              veut attribuer le vigueur.
#' @param Para.ConvVigMSCR Un dataframe  contenant les paramettres des équations
#'                         de conversion de vigueur en MSCR.
#' @return Retourne le classement MSCR de l'arbre.
#' @examples
#'

AttribMSCR<-function(Data,Para.ConvVigMSCR){

  select=dplyr::select

          if (Data$GrEspece %in% c("BOJ","ERR","ERS","FEN","FIN","HEG")){

              n<-nrow(Data)
              Data$GrEspece<-ifelse(Data$GrEspece=="AUT","FIN",Data$GrEspece)

              listeEss<-c(rep("ERS",n),rep("BOJ",n),rep("ERR",n),rep("HEG",n),rep("FIN",n))

  # Construction matrice X
  XConvVigMSCR<-matrix(0,ncol=9,nrow=n)
  XConvVigMSCR[,1]<-1
  XConvVigMSCR[,2]<-(Data$vigu0=="ViG")*1
  XConvVigMSCR[,3]<-(Data$prod0=="sciage")*1
  XConvVigMSCR[,4]<-(Data$DHPcm*10)
  XConvVigMSCR[,5:9]<-(Data$GrEspece==listeEss)*1

  # selectionner les parametres de conversion en vigeur
  Para.ConvVigMSCR1<-Para.ConvVigMSCR %>% filter(SubModuleID==20 & response==1)
  Para.ConvVigMSCR2<-Para.ConvVigMSCR %>% filter(SubModuleID==20 & response==2)
  Para.ConvVigMSCR3<-Para.ConvVigMSCR %>% filter(SubModuleID==20 & response==3)

  # Construction matrice beta
  BetaMat1<-matrix(Para.ConvVigMSCR1$ParameterEstimate,ncol=1)
  BetaMat2<-matrix(Para.ConvVigMSCR2$ParameterEstimate,ncol=1)
  BetaMat3<-matrix(Para.ConvVigMSCR3$ParameterEstimate,ncol=1)

  # Calcul
  pred1 <-exp(as.vector(XConvVigMSCR %*% BetaMat1))
  pred2 <-exp(as.vector(XConvVigMSCR %*% BetaMat2))
  pred3 <-exp(as.vector(XConvVigMSCR %*% BetaMat3))

  probM=pred1/(1+pred1+pred2+pred3)
  probS=probM+pred2/(1+pred1+pred2+pred3)
  probC=probS+pred3/(1+pred1+pred2+pred3)
  #probR=1/(1+pred1+pred2+pred3)
  Alea<-runif(n=n)
  MSCR<-ifelse(Alea<probM,"M",ifelse(Alea<probS,"S",ifelse(Alea<probC,"C","R")))

          }else{
            n<-nrow(Data)

            listeEss<-c(rep("SAB",n),rep("RES",n))

            # Construction matrice X
            XConvVigMSCR<-matrix(0,ncol=5,nrow=n)
            XConvVigMSCR[,1]<-1
            XConvVigMSCR[,2]<-(Data$vigu0=="ViG")*1
            XConvVigMSCR[,3:4]<-(Data$GrEspece==listeEss)*1
            XConvVigMSCR[,5]<-Data$DHPcm

            # selectionner les parametres de conversion en vigeur
            Para.ConvVigMSCR1<-Para.ConvVigMSCR %>% filter(SubModuleID==21 & response==1)
            Para.ConvVigMSCR2<-Para.ConvVigMSCR %>% filter(SubModuleID==21 & response==2)
            Para.ConvVigMSCR3<-Para.ConvVigMSCR %>% filter(SubModuleID==21 & response==3)

            # Construction matrice beta
            BetaMat1<-matrix(Para.ConvVigMSCR1$ParameterEstimate,ncol=1)
            BetaMat2<-matrix(Para.ConvVigMSCR2$ParameterEstimate,ncol=1)
            BetaMat3<-matrix(Para.ConvVigMSCR3$ParameterEstimate,ncol=1)

            # Calcul
            pred1 <-exp(as.vector(XConvVigMSCR %*% BetaMat1))
            pred2 <-exp(as.vector(XConvVigMSCR %*% BetaMat2))
            pred3 <-exp(as.vector(XConvVigMSCR %*% BetaMat3))

            probM=pred1/(1+pred1+pred2+pred3)
            probS=probM+pred2/(1+pred1+pred2+pred3)
            probC=probS+pred3/(1+pred1+pred2+pred3)
            #probR=1/(1+pred1+pred2+pred3)
            Alea<-runif(n=n)
            MSCR<-ifelse(Alea<probM,"M",ifelse(Alea<probS,"S",ifelse(Alea<probC,"C","R")))

          }






  return(MSCR)
}
