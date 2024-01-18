
#' @param m_lower
#' @param diag
#' @param symmetric
#' @return
#' @examples
#'

reconstruct <- function(m_lower, diag = TRUE, symmetric = TRUE) {
               l <- length(m_lower)
               n <- (sqrt(1 + 8*l) + ifelse(diag, -1, 1))/2  # Solve n*(n + 1) = l for n
               m <- matrix(NA, n, n)

  # Reconstruct
              m[t(lower.tri(m, diag = diag))] <- m_lower
              m<-t(m)
              if (symmetric) { # If symmetric, fill also upper half
              m[upper.tri(m)] <- t(m)[upper.tri(m)]
              }
               return(m)
}
#' Parametres par modules
#'
#' @param ModuleID
#' @param ParaOri
#' @param ParaIter
#' @param Omega
#' @param NbIter
#' @return
#' @examples
#'
ParaOmega<-function(ModuleID,ParaOri,ParaIter,Omega,NbIter){
          OmegaMod<-Omega %>% filter(SubModuleID==ModuleID) %>% .$ParameterEstimate
          OmegaMat<-reconstruct(OmegaMod)

          ParaMod<-ParaOri %>% filter(SubModuleID==ModuleID &ParameterEstimate!=0) %>% .$ParameterEstimate

          beta<-rmvnorm(n=NbIter, mean=ParaMod, sigma=OmegaMat, method="chol")
          beta<-as.vector(t(beta))

          ParaMod<-ParaIter %>%
                    filter(SubModuleID==ModuleID)

          ParaModSans0<-ParaMod %>%
                        filter(ParameterEstimate!=0) %>%
                        select(-ParameterEstimate) %>%
                        mutate(ParaRandom=beta)
          suppressMessages(
          ParaMod<-left_join(ParaMod,ParaModSans0) %>%
                   mutate(ParameterEstimate=ifelse(is.na(ParaRandom==TRUE),ParameterEstimate,ParaRandom)) %>%
                   select(-ParaRandom))

}
#' Parametres par modules
#'
#' @param ModuleID
#' @param ParaOri
#' @param ParaIter
#' @param Omega
#' @param NbIter
#' @return
#' @examples
#'
ParaOmega1<-function(ModuleID,ParaOri,ParaIter,Omega,NbIter){
  ModuleID = 16;ParaOri=ParaGaules;ParaIter=ParaTotGaules;Omega=OmegaGaules;NbIter=NbIter
  Omega = data.table(Omega)
  OmegaMod <- Omega[SubModuleID == ModuleID, ParameterEstimate]
  OmegaMat<-reconstruct(OmegaMod)

  ParaOri = data.table(ParaOri)
  ParaMod <- ParaOri[SubModuleID==ModuleID & ParameterEstimate!=0, ParameterEstimate]

  beta<-rmvnorm(n=NbIter, mean=ParaMod, sigma=OmegaMat, method="chol")
  beta<-as.vector(t(beta))

  ParaIter = data.table(ParaIter)
  ParaMod<-ParaIter[SubModuleID==ModuleID]

  ParaModSans0 <- ParaMod[ParameterEstimate != 0, ParaRandom :=beta]
  ParaModSans0$ ParameterEstimate = NULL


  suppressMessages(
    ParaMod<-left_join(ParaMod,ParaModSans0) %>%
      mutate(ParameterEstimate=ifelse(is.na(ParaRandom==TRUE),ParameterEstimate,ParaRandom)) %>%
      select(-ParaRandom))

}
