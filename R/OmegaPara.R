#' Fonction qui reconstruit la matrice de variance-covariance à partir du
#' fichier Omega où chaque ligne correspond à une valeur de la matrice

#' @param m_lower Vecteur de valeures de la matrice de variance-covariance
#' @param diag Champ TRUE/FALSE qui indique si on doit reconstruire la diagonale de la matrice
#' @param symmetric Champ TRUE/FALSE qui indique si la matrice est symetrique
#' @return Retourne une matrice de variance-covariance symetrique
#' @export
#'

reconstruct <- function(m_lower, diag = TRUE, symmetric = TRUE) {

  select=dplyr::select

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
#' Fonction qui simule les paramètres des effets fixe d'une équation pour chaque itération en fonction de leur
#' variance-covariance.
#'
#' @param ModuleID Numéro du module pour lequel on veut simuler les paramètres.
#' @param ParaOri Dataframe qui contient les paramètres de base que l'on veut simuler.
#' @param ParaIter Copie du dataframe ParaOri répliqué n=NbIter fois.
#' @param Omega Dataframe qui contient les valeurs des matrices de variance-covariance.
#' @inheritParams SimulSaMARE
#' @export
#'
#'
ParaOmega<-function(ModuleID,ParaOri,ParaIter,Omega,NbIter){
  select=dplyr::select
          OmegaMod<-Omega %>% filter(SubModuleID==ModuleID) %>% .$ParameterEstimate

          OmegaMat<-reconstruct(OmegaMod,diag = TRUE, symmetric = TRUE)

          ParaMod<-ParaOri %>% filter(SubModuleID==ModuleID &ParameterEstimate!=0) %>% .$ParameterEstimate


          beta<-mvtnorm::rmvnorm(n=NbIter, mean=ParaMod, sigma=OmegaMat, method="chol")
          beta<-as.vector(t(beta))

          ParaMod<-ParaIter %>%
                    filter(SubModuleID==ModuleID)

           ParaModSans0<-ParaMod %>%
                         filter(ParameterEstimate!=0) %>%
                         select(-ParameterEstimate)%>%
                         dplyr::mutate(ParaRandom=beta)
           suppressMessages(
           ParaMod<-left_join(ParaMod,ParaModSans0) %>%
                    mutate(ParameterEstimate=ifelse(is.na(ParaRandom==TRUE),ParameterEstimate,ParaRandom)) %>%
                  select(-ParaRandom))
}






