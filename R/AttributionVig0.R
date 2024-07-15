#' Fonction qui attribut la vigueur au début de la simulation. La fonction
#' cherche dabord si le classement vigueur 1,2,3,4,5,6 est fourni sinon
#' elle cherche le classement MSCR pour attribuer la vigeur sinon il classe l'arbre
#' comme vigoureux.
#'
#' @param Data Un dataframe contenant une ligne par arbre pour lesquels on
#'              veut attribuer le vigueur.
#' @param Para.ConvMSCRVig Un dataframe  contenant les paramettres des équations
#'                         de conversion du classement MSCR en vigueur.
#' @return Retourne la vigueur de l'arbre vigoureux ("ViG")ou non vigoureux ("NONVIG").
#' @examples
#'
#' #l'attribution de la vigueur
#' resultat <- AttribVigu0(Data,Para.ConvMSCRVig)
#' print(resultat)
#'
#' @export
#'

AttribVigu0<-function(Data,Para.ConvMSCRVig){
  select=dplyr::select

  if (is.na(Data$Vigueur)==FALSE){

    vigu0<-ifelse(Data$Vigueur %in% c(1,2,5),"ViG","NONVIG")

  }else{

    if (is.na(Data$MSCR)==FALSE){

  n<-nrow(Data)

  Data$GrEspeceMSCR<-ifelse(Data$GrEspece=="AUT","FIN",ifelse(
                            Data$GrEspece=="EPX","RES",Data$GrEspece))

  listeMSCR<-c(rep("R",n),rep("C",n),rep("S",n),rep("M",n),rep("CR",n),rep("MS",n))
  listeEss<-c(rep("BOJ",n),rep("ERR",n),rep("ERS",n),rep("FEN",n),rep("FIN",n),
              rep("HEG",n),rep("RES",n),rep("SAB",n))

  # Construction matrice X
  XConvMSCRVig<-matrix(0,ncol=16,nrow=n)
  XConvMSCRVig[,1]<-1
  XConvMSCRVig[,2:7]<-(Data$MSCR==listeMSCR)*1
  XConvMSCRVig[,8:15]<-(Data$GrEspeceMSCR==listeEss)*1
  XConvMSCRVig[,16]<-Data$DHPcm

  # selectionner les parametres de conversion en vigeur
  Para.ConvMSCRVig<-Para.ConvMSCRVig

  # Construction matrice beta
  BetaMat<-matrix(Para.ConvMSCRVig$ParameterEstimate,ncol=1)

  # Calcul
  logit <-as.vector(XConvMSCRVig %*% BetaMat)


  pred<-exp(logit)/(1+exp(logit))

 vigu0<-ifelse(pred>=runif(n=1),"ViG","NONVIG")

    }else{
  vigu0="ViG"

    }
  }
  return(vigu0)
}
