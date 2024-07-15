#' Fonction qui simule les effets aléatoires à l'échelle de la placette
#'  et de la période de simulation pour les modules originaux du modèle SaMARE
#'
#' @param CovParms Un dataframe contenant la variance des effets aléatoires en
#'                  lien avec leur module respectif.
#' @param Data  Un dataframe contenant les arbres à simuler pour lesquels la
#'               placette sera extraite afin d'y générer des effets aléatoire.
#' @param NbIter Nombre d'itérations de la simulation.
#' @param NbPeriodes Nombre de périodes pour lesquelles les effets aléatoires
#'                   seront simulés.
#' @return Retourne une liste d'effets aléatoires à l'échelle de la placette et
#'          de la période de simulation.
#' @examples
#'
#' #resultat <- RandomPlacStep(CovParms, Data, NbIter, NbPeriodes)
#' print(resultat)


RandomPlacStep<-function (CovParms, Data, NbIter, NbPeriodes) {

  select=dplyr::select
  Placettes<-unique(Data$Placette)

  CovPlac<-CovParms %>%
          filter(Subject=="placette") %>%
          select(SubModuleID,ParameterEstimate)

  suppressMessages(
  RandPlac<-data.frame("Placette"=rep(Placettes,8)) %>%
            arrange(Placette) %>%
            mutate(SubModuleID=rep(c(1,2,3,4,5,6,7,8),length(Placettes)),NbIter=NbIter) %>%
            arrange(Placette,SubModuleID) %>%
            inner_join(CovPlac))

  randPlac<-function (RandPlac){
    SdPara<-sqrt(RandPlac$ParameterEstimate)
    rand<-data.frame("Placette"=RandPlac$Placette,"Iter"=rep(1:RandPlac$NbIter),
                     "SubModuleID"=RandPlac$SubModuleID,
                     "RandomPlac"=rnorm(n=RandPlac$NbIter,mean=0,sd=SdPara))

  }

  PredRandPlac<- RandPlac%>%
    mutate(ID=paste(Placette,"_",SubModuleID)) %>%
    group_by(ID) %>%
    nest() %>%
    mutate(RandPlac = map(data,randPlac)) %>%
    ungroup() %>%
    unnest(RandPlac) %>%
    select(-data,-ID)

  PredRandPlac<-PredRandPlac %>% slice(rep(1:n(), each = NbPeriodes)) %>%
                mutate(Step=rep(1:NbPeriodes,NbIter*length(Placettes)*8))

 ####################################Random Step####################

    CovStep<-CovParms %>%
      filter(Subject=="step") %>%
      select(SubModuleID,ParameterEstimate)

  suppressMessages(
    RandStep<-data.frame("Placette"=rep(Placettes,5*NbPeriodes)) %>%
              arrange(Placette) %>%
              mutate(SubModuleID=rep(c(1,2,6,7,8),length(Placettes)*NbPeriodes)) %>%
              arrange(Placette,SubModuleID) %>%
              mutate(Step=rep(1:NbPeriodes,length(Placettes)*5),NbIter=NbIter) %>%
              inner_join(CovStep))

    randStep<-function (RandStep){
      SdPara<-sqrt(RandStep$ParameterEstimate)
      rand<-data.frame("Placette"=RandStep$Placette,"Iter"=rep(1:RandStep$NbIter),
                       "SubModuleID"=RandStep$SubModuleID,"Step"=RandStep$Step,
                       "RandomStep"=rnorm(n=RandStep$NbIter,mean=0,sd=SdPara))

    }

    PredRandStep <- RandStep%>%
                    mutate(ID=paste(Placette,"_",SubModuleID,"_",Step)) %>%
                    group_by(ID) %>%
                    nest() %>%
                    mutate(RandStep = map(data,randStep)) %>%
                    ungroup() %>%
                    unnest(RandStep) %>%
                    select(-data,-ID)




  suppressMessages(
  Random<-left_join(PredRandPlac,PredRandStep))



return(Random)

}

#' Fonction qui simule les effets aléatoires à l'échelle de la placette
#'  pour les modules de recrutement basé sur les gaules et d'évolution des
#'  gaules de SaMARE

#' @param CovParms Un dataframe contenant la variance des effets aléatoires en
#'                  lien avec leur module respectif
#' @param Data Un dataframe contenant les informations sur les gaules pour lesquels la
#'               placette sera extraite afin d'y générer des effets aléatoire
#' @param NbIter Nombre d'itérations de la simulation
#' @return Retourne une liste d'effets aléatoires à l'échelle de la placette
#' @examples
#'  #resultat <- RandomPlacStepGaules(CovParms, Data, NbIter)
#' print(resultat)
#'
RandomPlacStepGaules<-function (CovParms, Data, NbIter) {

  select=dplyr::select
  Placettes<-unique(Data$Placette)

  CovPlac<-CovParms %>%
           filter(Subject=="placette") %>%
           select(SubModuleID,ParameterEstimate,response)

  suppressMessages(
    RandPlac<-data.frame("Placette"=rep(Placettes,13)) %>%
      arrange(Placette) %>%
      mutate(SubModuleID=rep(c(10,10,11,12,12,13,13,14,14,15,15,16,16),length(Placettes)),
             response=rep(c("pi","count","count","pi","count","pi","count","pi","count",
                            "pi","count","pi","count"),length(Placettes)),NbIter=NbIter) %>%
      arrange(Placette,SubModuleID) %>%
      inner_join(CovPlac))

  randPlac<-function (RandPlac){
            rand<-data.frame("Placette"=RandPlac$Placette,"Iter"=rep(1:RandPlac$NbIter),
                             "SubModuleID"=RandPlac$SubModuleID,
                             "response"=RandPlac$response,
                              "RandomPlac"=rnorm(n=RandPlac$NbIter,mean=0,sd=RandPlac$ParameterEstimate))

  }

  suppressWarnings(
  PredRandPlac <- RandPlac%>%
    mutate(ID=paste(Placette,"_",SubModuleID,"_",response)) %>%
    group_by(ID) %>%
    nest() %>%
    mutate(RandPlac = map(data,randPlac)) %>%
    ungroup() %>%
    unnest(RandPlac) %>%
    select(-data,-ID))

  PredRandPlac<-PredRandPlac %>% slice(rep(1:n()))


}


