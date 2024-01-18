
#' @param CovParms
#' @param Data
#' @param NbIter
#' @param NbPeriodes
#' @return
#' @examples
#'


RandomPlacStep<-function (CovParms, Data, NbIter, NbPeriodes) {

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


#' @param CovParms
#' @param Data
#' @param NbIter
#' @param NbPeriodes
#' @return
#' @examples
#'
RandomPlacStep1<-function (CovParms, Data, NbIter, NbPeriodes) {

  Placettes<-unique(Data$Placette)

  CovParms = data.table(CovParms)
  CovPlac = CovParms[Subject=="placette", .(SubModuleID,ParameterEstimate)]
  RandPlac <- data.table(Placette = rep(Placettes, 8))
  setorder(RandPlac, Placette)
  RandPlac[, SubModuleID := rep(1:8, length(Placettes))]
  RandPlac[, NbIter := NbIter]
  RandPlac <- merge(RandPlac, CovPlac, by = "SubModuleID")
  setorder(RandPlac, Placette, SubModuleID)

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

  PredRandPlac <- rbindlist(lapply(1:NbPeriodes, function(i) {
    df_copy <- PredRandPlac
    df_copy$Step <- i
    return(df_copy)
  }))


  ####################################Random Step####################
  CovStep = CovParms[Subject=="step", .(SubModuleID,ParameterEstimate)]

  RandStep <- data.table(Placette = rep(Placettes, 5*NbPeriodes))
  setorder(RandStep, Placette)
  RandStep[, SubModuleID := rep(c(1,2,6,7,8),length(Placettes)*NbPeriodes)]
  RandStep <- RandStep[order(Placette, SubModuleID)]
  RandStep[, `:=`(Step = rep(1:NbPeriodes, length(Placettes) * 5),
                  NbIter = NbIter)]
  RandStep <- merge(RandStep, CovStep, by = "SubModuleID")
  setorder(RandStep, Placette, SubModuleID)

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

#' @param CovParms
#' @param Data
#' @param NbIter
#' @param NbPeriodes
#' @return
#' @examples
#'
RandomPlacStepTest<-function (CovParms, Data, NbIter, NbPeriodes) {

  Placettes<-unique(Data$Placette); NbPeriodes = Horizon
  library(data.table);library(microbenchmark)

  # t1 = Sys.time()
  test =  microbenchmark(
    "DT" = {
      CovParms = data.table(CovParms)
      CovPlac = CovParms[Subject=="placette", .(SubModuleID,ParameterEstimate)]
      RandPlac <- data.table(Placette = rep(Placettes, 8))
      setorder(RandPlac, Placette)
      RandPlac[, SubModuleID := rep(1:8, length(Placettes))]
      RandPlac[, NbIter := NbIter]
      RandPlac <- merge(RandPlac, CovPlac, by = "SubModuleID")
      setorder(RandPlac, Placette, SubModuleID)
      # print(Sys.time()-t1)
    },
    "Dplyr" = {
      # t0 = Sys.time()
      CovPlac<-CovParms %>%
        filter(Subject=="placette") %>%
        select(SubModuleID,ParameterEstimate)
      suppressMessages(
        RandPlac<-data.frame("Placette"=rep(Placettes,8)) %>%
          arrange(Placette) %>%
          mutate(SubModuleID=rep(c(1,2,3,4,5,6,7,8),length(Placettes)),NbIter=NbIter) %>%
          arrange(Placette,SubModuleID) %>%
          inner_join(CovPlac))
      # print(Sys.time()-t0)
    })
  # Unit: milliseconds
  # expr     min       lq      mean   median       uq      max neval
  # DT  5.9278  6.38650  7.275178  6.71015  7.30235  15.3176   100
  # Dplyr 53.8022 61.39535 70.605109 68.06700 77.29055 113.5841   100

  randPlac<-function (RandPlac){
    SdPara<-sqrt(RandPlac$ParameterEstimate)
    rand<-data.frame("Placette"=RandPlac$Placette,"Iter"=rep(1:RandPlac$NbIter),
                     "SubModuleID"=RandPlac$SubModuleID,
                     "RandomPlac"=rnorm(n=RandPlac$NbIter,mean=0,sd=SdPara))
    return(rand)
  }

  PredRandPlac = RandPlac
  PredRandPlac[, ID := paste(Placette, "_", SubModuleID)]
  test = bind_rows(lapply(as.list(unique(PredRandPlac$ID)), function (x) randPlac(PredRandPlac[ID==x]) ))

  test =  microbenchmark(
    "Dplyr"= {
      PredRandPlac<- RandPlac%>%
        mutate(ID=paste(Placette,"_",SubModuleID)) %>%
        group_by(ID) %>%
        nest() %>%
        mutate(RandPlac = map(data,randPlac)) %>%
        ungroup() %>%
        unnest(RandPlac) %>%
        select(-data,-ID)
    },
    "DT" = {
      PredRandPlac = RandPlac
      PredRandPlac[, ID := paste(Placette, "_", SubModuleID)]
      test = bind_rows(lapply(as.list(unique(PredRandPlac$ID)), function (x) randPlac(PredRandPlac[ID==x]) ))
    }, times = 20)

  # test =  microbenchmark(
  #   "Direct"={
  #     PredRandPlac1 <- rbindlist(lapply(1:NbPeriodes, function(i) {
  #       df_copy <- PredRandPlac
  #       df_copy$Step <- i
  #       return(df_copy)
  #     }))},
  #   "Dplyr"={
  #     PredRandPlac2<-PredRandPlac %>% slice(rep(1:n(), each = NbPeriodes)) %>%
  #       mutate(Step=rep(1:NbPeriodes,NbIter*length(Placettes)*8))
  #   })
  #
  # > test
  # Unit: milliseconds
  # expr    min      lq     mean  median      uq     max neval
  # Direct 1.5420 1.67950 2.197072 1.78265 1.91470 16.6451   100
  # Dplyr 6.2509 6.79435 8.252493 7.01170 7.60915 25.3894   100

  ####################################Random Step####################

  CovStep = CovParms[Subject=="step", .(SubModuleID,ParameterEstimate)]
  test =  microbenchmark(
    "DT"= {
      RandStep <- data.table(Placette = rep(Placettes, 5*NbPeriodes))
      setorder(RandStep, Placette)
      RandStep[, SubModuleID := rep(c(1,2,6,7,8),length(Placettes)*NbPeriodes)]
      RandStep <- RandStep[order(Placette, SubModuleID)]
      RandStep[, `:=`(Step = rep(1:NbPeriodes, length(Placettes) * 5),
                      NbIter = NbIter)]
      RandStep <- merge(RandStep, CovStep, by = "SubModuleID")
      setorder(RandStep, Placette, SubModuleID)
    },
    "Dplyr"= {
      suppressMessages(
        RandStep1<-data.frame("Placette"=rep(Placettes,5*NbPeriodes)) %>%
          arrange(Placette) %>%
          mutate(SubModuleID=rep(c(1,2,6,7,8),length(Placettes)*NbPeriodes)) %>%
          arrange(Placette,SubModuleID) %>%
          mutate(Step=rep(1:NbPeriodes,length(Placettes)*5),NbIter=NbIter) %>%
          inner_join(CovStep))})
  # > test
  # Unit: milliseconds
  # expr     min      lq      mean   median       uq      max neval
  # DT  6.8236  7.8378  9.033342  8.63220  9.55155  16.4819   100
  # Dplyr 51.1863 61.1509 72.521519 66.91675 79.25015 266.3424   100
  #
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

######################################################################
##########################Pour les Gaules############################
####################################################################

#' @param CovParms
#' @param Data
#' @param NbIter
#' @return
#' @examples
#'
RandomPlacStepGaules<-function (CovParms, Data, NbIter) {

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


