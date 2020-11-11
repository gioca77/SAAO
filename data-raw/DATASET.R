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

# Warnung mit Overdispersion
set.seed(1)
example1_timeserie <- zeit_sim(n=12, mu=12, beta=1.01, disp=1)

usethis::use_data(example1_timeserie, overwrite = TRUE)

# keine Warnung ohne Overdispersion
set.seed(1)
example2_timeserie <- zeit_sim(n=10, mu=25, beta=0.91, disp=0)

usethis::use_data(example2_timeserie, overwrite = TRUE)

# Warnung ohne Overdispersion
set.seed(11)
example3_timeserie <- zeit_sim(n=8, mu=13, beta=0.98, disp=0)

usethis::use_data(example3_timeserie, overwrite = TRUE)

# keine Warnung mit Overdispersion
set.seed(2)
example4_timeserie <- zeit_sim(n=14, mu=8, beta=1.02, disp=20)

usethis::use_data(example4_timeserie, overwrite = TRUE)

# Zeitreihe Fruewarnsystem
set.seed(4)
example_earlywarnung <- zeit_sim(n=14, mu=10, beta=1.01, disp=0)
length(example_earlywarnung)
example_earlywarnung <- c(example_earlywarnung, example_earlywarnung[158:169] + 10)

usethis::use_data(example_earlywarnung, overwrite = TRUE)



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

set.seed(7) # Massnahme 2011.01.01
example_no_effect <- zeit_massnahme_sim(n = 10, massnahme = 6,
                                        mu = 15, mu_2 = NULL,
                                        beta = 1, beta_2 = NULL,
                                        disp = 0, start=2005)

usethis::use_data(example_no_effect, overwrite = TRUE)

set.seed(5) # Massnahme 2012.01.01
example_measure_effect <- zeit_massnahme_sim(n = 12, massnahme = 8,
                                        mu = 18, mu_2 = 10,
                                        beta = 1, beta_2 = NULL,
                                        disp = 1.1, start=2005)

usethis::use_data(example_measure_effect, overwrite = TRUE)

set.seed(3) # Massnahme 2010.01.01
example_measure_and_trend_effect <- zeit_massnahme_sim(n = 12, massnahme = 8,
                                             mu = 18, mu_2 = 5,
                                             beta = 1.2, beta_2 = 0.9,
                                             disp = 0, start=2003)

usethis::use_data(example_measure_and_trend_effect, overwrite = TRUE)

set.seed(3) # Massnahme 2012.01.01
example_measure_effect_and_trend <- zeit_massnahme_sim(n = 14, massnahme = 9,
                                                       mu = 20, mu_2 = 9,
                                                       beta = 1.1, beta_2 = NULL,
                                                       disp = 0, start=2003)

usethis::use_data(example_measure_effect_and_trend, overwrite = TRUE)

set.seed(10) # Massnahme 2013.01.01
example_trend <- zeit_massnahme_sim(n = 10, massnahme = 6,
                                    mu = 15, mu_2 = NULL,
                                    beta = 1.075, beta_2 = NULL,
                                    disp = 0, start=2007)

usethis::use_data(example_trend, overwrite = TRUE)

set.seed(2) # Massnahme 2011.01.01
example_trend_effect <- zeit_massnahme_sim(n = 16, massnahme = 10,
                                           mu = 20, mu_2 = NULL,
                                           beta = 1.05, beta_2 = 0.9,
                                           disp = 10, start=2001)

usethis::use_data(example_trend_effect, overwrite = TRUE)


exposition_ex1 <- data.frame(start=c("01.01.2005", "09.10.2009", "01.06.2012", "01.10.2012"),  DTV=c(1000, 1200, 500, 2000))
usethis::use_data(exposition_ex1, overwrite = TRUE)
exposition_ex2 <- data.frame(year=c(2005:2020), DTV=seq(1000,2500, 100))
usethis::use_data(exposition_ex2, overwrite = TRUE)
exposition_ex3 <- data.frame(year=c(2005, 2010), DTV=c(1000,1500))
usethis::use_data(exposition_ex3, overwrite = TRUE)

