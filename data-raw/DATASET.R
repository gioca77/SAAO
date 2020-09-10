## code to prepare `DATASET` dataset goes here

## code to prepare `DATASET` dataset goes here

### Funktion zum Erstellen von Zeitreihen fuer Fruehwarnsystem

# n: laenge der Zeitreihe
# mu: Fallzahl
# beta: jaehrlicher Anstieg (default = 1) (1.1 => 10% Anstieg)
# disp: Dispersionsparameter (default = 0 und somit Poissonverteilt)



zeit_sim <- function(n, mu, beta = 1, disp = 0, start = 2000){
  # Poisson Zeitreihen
  if (disp == 0){
    unfall_data <- data.frame(Jahr = 1:n,
                              Unfaelle = rpois(n, lambda = mu*beta^(0:(n-1))))
  }
  # negativ Binomial Zeitreihen
  else {
    unfall_data <- data.frame(Jahr = 1:n,
                              Unfaelle = rnbinom(n, size = disp, mu = mu*beta^(0:(n-1))))
  }
  dat <- NULL
  jahre <- unfall_data$Jahr+start-1
  for (i in 1:n){
    tage <- seq(as.Date(paste0(jahre[i], "-1-1")), as.Date(paste0(jahre[i], "-12-31")), 1)
    dat <- append(dat, sample(tage, size=unfall_data$Unfaelle[i], replace=T))
  }
  return(dat)
}

set.seed(1)
example_timeserie <- zeit_sim(n=12, mu=12, beta=1.01, disp=0.5)


usethis::use_data(example_timeserie, overwrite = TRUE)

### Funktion fuer Generieren von Zeitreihen mit Massnahmen-/Trendeffekt

# n: laenge der Zeitreihe
# mu: erwartete Anzahl Unfaelle
# beta: jaehrlicher Anstieg (default = 1) (1.1 => 10% Anstieg)
# disp: Dispersionsparameter (default = 0 und somit Poissonverteilt)
# massnahme ist Zeitpunkt der Massnahme nach Start der Zeitreihe

# default                     : Kein Effekt
# beta != 1                   : Trend (zu-/abnehmend, kein Effekt)
# mu_2 != mu                  : Massnahmeneffekt
# mu_2 != mu & beta != 1      : Massnahmeneffekt und Trend weiterfuehrend
# beta != beta_2              : Trendeffekt
# mu_2 != mu & beta != beta_2 : Massnahmeneffekt und Trendeffekt

zeit_massnahme_sim <- function(n = 10, massnahme = n-4,
                               mu = 15, mu_2 = NULL,
                               beta = 1, beta_2 = NULL,
                               disp = 0, start=2005){

  # Abbruch falls massnahme kleiner als Zeithorizont
  if (massnahme > n) {print("Abbruch, massnahme groesser als Zeitraum", return())}

  # Poisson Zeitreihe
  if (disp == 0){
    if (is.null(mu_2) & is.null(beta_2)) {
      unfall_data <- data.frame(Jahr = 1:n,
                                Massnahme = c(rep("vorher",(massnahme)),
                                              rep("nachher",n-massnahme)))
      mu_vorher_nachher <- mu*beta^(0:(n-1))
      unfall_data$Unfaelle <- rpois(n, lambda = mu_vorher_nachher)

      plot(unfall_data$Jahr, unfall_data$Unfaelle, type = "b",
           ylim = c(0, max(unfall_data$Unfaelle)))
      lines(1:massnahme, mu_vorher_nachher[1:massnahme], col = 2)
      lines((massnahme+1):n, mu_vorher_nachher[(massnahme+1):n], col = 3)
    }
    else if (!is.null(mu_2) & is.null(beta_2)) {
      unfall_data <- data.frame(Jahr = 1:n,
                                Massnahme = c(rep("vorher",(massnahme)),
                                              rep("nachher",n-massnahme)))
      mu_vorher <- mu*beta^(0:(massnahme-1))
      mu_nachher <- mu_2*beta^(0:(n-massnahme-1))
      unfall_data$Unfaelle <- rpois(n, lambda = c(mu_vorher, mu_nachher))

      plot(unfall_data$Jahr, unfall_data$Unfaelle, type = "b",
           ylim = c(0, max(unfall_data$Unfaelle)))
      lines(1:massnahme, mu_vorher, col = 2)
      lines((massnahme+1):n, mu_nachher, col = 3)

      return(unfall_data)
    }
    else if (!is.null(mu_2) & !is.null(beta_2)) {
      unfall_data <- data.frame(Jahr = 1:n,
                                Massnahme = c(rep("vorher",(massnahme)),
                                              rep("nachher",n-massnahme)))
      mu_vorher <- mu*beta^(0:(massnahme-1))
      mu_nachher <- mu_2*beta_2^(0:(n-massnahme-1))
      unfall_data$Unfaelle <- rpois(n, lambda = c(mu_vorher, mu_nachher))

      plot(unfall_data$Jahr, unfall_data$Unfaelle, type = "b",
           ylim = c(0, max(unfall_data$Unfaelle)))
      lines(1:massnahme, mu_vorher, col = 2)
      lines((massnahme+1):n, mu_nachher, col = 3)

      return(unfall_data)
    }
    else if (is.null(mu_2) & !is.null(beta_2)){
      unfall_data <- data.frame(Jahr = 1:n,
                                Massnahme = c(rep("vorher",(massnahme)),
                                              rep("nachher",n-massnahme)))
      mu_vorher <- mu*beta^(0:(massnahme-1))
      mu_nachher <- mu_vorher[massnahme]*beta_2^(0:(n-massnahme-1))
      mu_nachher[mu_nachher<0] <- runif(1, 0, 1)
      unfall_data$Unfaelle <- rpois(n, lambda = c(mu_vorher, mu_nachher))

      plot(unfall_data$Jahr, unfall_data$Unfaelle, type = "b",
           ylim = c(0, max(unfall_data$Unfaelle)))
      lines(1:massnahme, mu_vorher, col = 2)
      lines((massnahme+1):n, mu_nachher, col = 3)

      return(unfall_data)
    }
    else {
      print("Ungueltige Eingabe")
      return
    }
  }

  # negativ Binomial Zeitreihe
  else if (disp > 0) {
    if (is.null(mu_2) & is.null(beta_2)) {
      unfall_data <- data.frame(Jahr = 1:n,
                                Massnahme = c(rep("vorher",(massnahme)),
                                              rep("nachher",n-massnahme)))
      mu_vorher_nachher <- mu*beta^(0:(n-1))
      unfall_data$Unfaelle <- rnbinom(n, size = disp, mu = mu_vorher_nachher)

      plot(unfall_data$Jahr, unfall_data$Unfaelle, type = "b",
           ylim = c(0, max(unfall_data$Unfaelle)))
      lines(1:massnahme, mu_vorher_nachher[1:massnahme], col = 2)
      lines((massnahme+1):n, mu_vorher_nachher[(massnahme+1):n], col = 3)
    }
    else if (!is.null(mu_2) & is.null(beta_2)) {
      unfall_data <- data.frame(Jahr = 1:n,
                                Massnahme = c(rep("vorher",(massnahme)),
                                              rep("nachher",n-massnahme)))
      mu_vorher <- mu*beta^(0:(massnahme-1))
      mu_nachher <- mu_2*beta^(0:(n-massnahme-1))
      unfall_data$Unfaelle <- rnbinom(n, size = disp, mu = c(mu_vorher, mu_nachher))

      plot(unfall_data$Jahr, unfall_data$Unfaelle, type = "b",
           ylim = c(0, max(unfall_data$Unfaelle)))
      lines(1:massnahme, mu_vorher, col = 2)
      lines((massnahme+1):n, mu_nachher, col = 3)
    }
    else if (!is.null(mu_2) & !is.null(beta_2)) {
      unfall_data <- data.frame(Jahr = 1:n,
                                Massnahme = c(rep("vorher",(massnahme)),
                                              rep("nachher",n-massnahme)))
      mu_vorher <- mu*beta^(0:(massnahme-1))
      mu_nachher <- mu_2*beta_2^(0:(n-massnahme-1))
      unfall_data$Unfaelle <- rnbinom(n, size = disp, mu = c(mu_vorher, mu_nachher))

      plot(unfall_data$Jahr, unfall_data$Unfaelle, type = "b",
           ylim = c(0, max(unfall_data$Unfaelle)))
      lines(1:massnahme, mu_vorher, col = 2)
      lines((massnahme+1):n, mu_nachher, col = 3)
    }
    else if (is.null(mu_2) & !is.null(beta_2)){
      unfall_data <- data.frame(Jahr = 1:n,
                                Massnahme = c(rep("vorher",(massnahme)),
                                              rep("nachher",n-massnahme)))
      mu_vorher <- mu*beta^(0:(massnahme-1))
      mu_nachher <- mu_vorher[massnahme]*beta_2^(0:(n-massnahme-1))
      mu_nachher[mu_nachher<0] <- runif(1, 0, 1)
      unfall_data$Unfaelle <- rnbinom(n, size = disp, mu = c(mu_vorher, mu_nachher))

      plot(unfall_data$Jahr, unfall_data$Unfaelle, type = "b",
           ylim = c(0, max(unfall_data$Unfaelle)))
      lines(1:massnahme, mu_vorher, col = 2)
      lines((massnahme+1):n, mu_nachher, col = 3)
    }
    else {
      print("Ungueltige Eingabe")
      return
    }
  }
  else {
    print("Ungueltige Eingabe (disp < 0)")
  }
  dat <- NULL
  jahre <- unfall_data$Jahr+start-1
  for (i in 1:n){
    tage <- seq(as.Date(paste0(jahre[i], "-1-1")), as.Date(paste0(jahre[i], "-12-31")), 1)
    dat <- append(dat, sample(tage, size=unfall_data$Unfaelle[i], replace=T))
  }
  return(dat)
}

set.seed(7)
example_no_effect <- zeit_massnahme_sim(n = 10, massnahme = 6,
                                        mu = 15, mu_2 = NULL,
                                        beta = 1, beta_2 = NULL,
                                        disp = 0, start=2005)

usethis::use_data(example_no_effect, overwrite = TRUE)
