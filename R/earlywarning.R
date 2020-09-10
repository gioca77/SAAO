#' Function to determine the prediction interval for new value in time series
#'
#' @param accidents Vector with accident data
#' @param exposition Dataframe with Expositionsdata (DTV) DEFAULT NULL
#' @param n number of simulations
#' @param conf.level level of the prediction interval
#' @param from From which date or year (1.1) the time series should be considered. Optional. If not specified, the 1.1 from the year of the earliest accident is used.
#' @param until Until when date or year (31.12) the time series should be considered. Optional. If not specified, the 31.12 from the year of the latest accident is used.
#' @param main optional title for the plot
#' @param x_axis optional vector with the values for the x-axis
#' @param max_y optional maximum value for the y-axis
#' @param silent parameter to suppress error messages during model evaluation
#' @param lang language for output (en, fr, de or it)
#' @export
#' @examples
#'   # example
#'   earlywarning(accidents=example_timeserie)


earlywarning <- function(accidents, exposition = NULL, n = 1000, conf.level = 0.9, from = NULL,
                         until = NULL,
                        main = NULL, silent = TRUE, x_axis =NULL, max_y = NULL, lang = "en", v=1) {
  ## check mandatory input
  accidents <- try(as.Date(accidents, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
  ## check optional input
  if (!is.null(exposition)){
    if (dim(exposition)[2]>=2){
      colnames(exposition)[1:2] <- c("time", "DTV")
      if (any(nchar(as.character(exposition$time))!=4)){
        exposition$time <- try(as.Date(exposition$time, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent=silent)
      } else exposition$time <- try(as.numeric(exposition$time), silent=silent)
    }
  }
  if (!is.null(from)){
    if (nchar(as.character(from))!=4){
      from <- try(as.Date(from, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent=silent)
    } else from <-  try(as.Date(paste0(from, "-01-01")), silent=silent)
  }
  if (!is.null(until)){
    if (nchar(as.character(until))!=4){
      until <- try(as.Date(until, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent=silent)
    } else until <-  try(as.Date(paste0(until, "-12-31")), silent=silent)
  }
  if (!(lang %in% c("en", "de", "it", "fr"))){
    lang <- "en"
    warning("language unknown, set to english")
  }
  Check <- ArgumentCheck::newArgCheck()
  #* accidents format
  if (is(accidents)[1]==  "try-error")
    ArgumentCheck::addError(
      msg = "'accidents' not in the right format",
      argcheck = Check
    )
  #* exposition format
  if (!is.null(exposition)){
    if (dim(exposition)[2]<2)
      ArgumentCheck::addError(
        msg = "wrong input for 'exposition'",
        argcheck = Check
      )
    #* exposition time format
    if (is(exposition)[1]==  "try-error")
      ArgumentCheck::addError(
        msg = "wrong time format for 'exposition'",
        argcheck = Check
      )
  }
  #* from time format
  if (is(from)[1]==  "try-error")
    ArgumentCheck::addError(
      msg = "wrong time format for 'from'",
      argcheck = Check
    )
  #* conf.level format
  if (conf.level<0 | conf.level>1)
    ArgumentCheck::addError(
      msg = "'conf.level' must be between 0 and 1",
      argcheck = Check
    )

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)

  ## processing input
  if (is.null(from)) from <- as.Date(paste0(as.numeric( format(min(accidents), '%Y')), "-01-01"))
  if (is.null(until)) until <- as.Date(paste0(as.numeric( format(max(accidents), '%Y')), "-12-31"))
  timeserie <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric( format(until, '%Y'))),"-", format(until, '%m-%d')))+1
  if (from[1]>timeserie[1]) timeserie <- timeserie[-1]
  dat_model <- as.data.frame(matrix(NA, nrow=length(timeserie)-1, ncol=2))
  colnames(dat_model) <- c("Date", "accidents")
  dat_model$Date <- as.Date((as.numeric(timeserie[1:(length(timeserie) - 1)]) + as.numeric(timeserie[2:length(timeserie)] - 1)) / 2, origin="1970-01-01")
  for (i in 1:(length(timeserie) - 1)){
    dat_model[i, 2] <- sum(accidents >= timeserie[i] & accidents < timeserie[i+1])
  }
  ## add exposition to data
  if (!is.null(exposition)){
    tab <- data.frame(Date=seq(from, until, 1), DTV = exposition$DTV[1])
    if (is(exposition$time)[1] == "Date"){
      index <- match(tab$Date, exposition$time)
      if(length(index) > 2){
        for (i in 2:(length(index) - 1)){
          tab$DTV[tab$Date >= exposition$time[i] & tab$Date<exposition$time[i+1]] <- exposition$DTV[i]
        }
      }
      tab$DTV[tab$Date >= exposition$time[length(index)]] <- exposition$DTV[length(index)]
    }
    if (is.numeric(exposition$time)){
      for (i in 1:length(exposition$time)){
        tab$DTV[as.numeric(format(tab$Date, '%Y')) >= exposition$time[i]] <- exposition$DTV[i]
      }
    }
    dat_model$DTV <- NA
    for (i in 1:(length(timeserie)-1)){
      dat_model$DTV[i] <- mean(tab$DTV[tab$Date >= timeserie[i] & tab$Date < timeserie[i+1]])
    }
  }
  ## Model
  overdisp <- FALSE
  if (is.null(exposition)){
    ## Poisson_Modell
    fit <- glm(accidents~Date, data=dat_model[-dim(dat_model)[1], ], family = "poisson")
    ## deviance test
    test_overdisp <- 1 - pchisq(deviance(fit),df.residual(fit))
    if (v == 2){
      test_overdisp <- performance::check_overdispersion(fit)$p_value
    }
    if (test_overdisp <= 0.05){
      fit_start <- glm(accidents ~ Date, data=dat_model[-dim(dat_model)[1], ], family = "quasipoisson",
                       control = glm.control(epsilon = 1e-8, maxit = 200, trace = FALSE))
      fit_nb <- try(MASS::glm.nb(accidents~Date, data=dat_model[-dim(dat_model)[1], ], start=fit_start$coefficients,
                                 control = glm.control(epsilon = 1e-6, maxit = 200, trace = FALSE)), silent = TRUE)
      if (attr(fit_nb, "class")[1] != "try-error") {
        fit <- fit_nb
      } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated")
    }
  }
  if (!is.null(exposition)){
    ## Poisson_Modell
    fit <- glm(accidents ~ Date + offset(log(DTV)), data = dat_model[-dim(dat_model)[1], ], family = "poisson")
    ## deviance test
    test_overdisp <- 1 - pchisq(deviance(fit),df.residual(fit))
    if (v == 2){
      test_overdisp <- performance::check_overdispersion(fit)$p_value
    }
    # n <- dim(dat_model)[1]
    # k <- 2
    # yhat <- predict (fit, type="response")
    # z <- (dat[-dim(dat)[1], ]$Unfaelle-yhat)/sqrt(yhat)
    # sum(z^2)
    # cat ("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
    # cat ("p-value of overdispersion test is ", 1-pchisq (sum(z^2), n-k), "\n")
    if (test_overdisp <= 0.05){
      fit_start <- glm(accidents ~ Date + offset(log(DTV)), data=dat_model[-dim(dat_model)[1], ],
                       family = "quasipoisson",
                       control = glm.control(epsilon = 1e-8, maxit = 200, trace = FALSE))
      fit_nb <- try(MASS::glm.nb(accidents ~ Date + offset(log(DTV)),
                                 data=dat_model[-dim(dat_model)[1], ],
                                 start=fit_start$coefficients,
                                 control = glm.control(epsilon = 1e-6, maxit = 200, trace = FALSE)), silent=TRUE)
      if (attr(fit_nb, "class")[1] != "try-error")
      {
        fit <- fit_nb
        overdisp <- TRUE
      } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated")
    }
  }
  pred_new <- predict(fit, newdata=dat_model[dim(dat_model)[1], ], se.fit=TRUE, type="link")
  samp <- rnorm(n,  mean=pred_new$fit, sd=pred_new$se.fit)
  if (grepl("Negative Binomial", x=fit$family$family)){
    y_int <- rnbinom(n, mu=exp(samp), size = summary(fit)$theta)
  }
  if (fit$family$family=="poisson"){
    y_int <- rpois(n, lambda = exp(samp))
  }
  lp <- (1 - conf.level) / 2
  up <- 1 - lp
  ci <- quantile(y_int, c(lp, up))
  dat_fit <- NULL
  preds <- predict(fit, type= "link", se.fit = TRUE)
  dat_fit <- data.frame(Date = fit$model$Date,
                        expect = exp(preds$fit),
                        low = exp(preds$fit-qnorm(0.975)*preds$se.fit),
                        upp = exp(preds$fit+qnorm(0.975)*preds$se.fit))
  dat_total <- merge(x=dat_model, y=dat_fit, by="Date", all=TRUE)
  col_w <- ifelse(dat_total[dat_total$Date==max(dat_total$Date),"accidents"]>ci[2], "red", "black")
  if (is.null(max_y)) max_y <- max(max(dat_total[,c("accidents", "low", "upp", "expect")],
                                       na.rm=T),ci[2])*1.1
  # Base plot
  if (is.null(x_axis)) x_axis <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric(format(until, '%Y'))+1), "-01-01"))
  if (is.null(main)){
    if (lang == "en") main <- paste("Time Series Analysis", from, "-", until, sep=" ")
    if (lang == "de") main <- paste("Zeitreihenanalyse", from, "-", until, sep=" ")
    if (lang == "fr") main <- paste("Analyse des series chronologiques", from, "-", until, sep=" ")
    if (lang == "it") main <- paste("Analisi delle serie temporali", from, "-", until, sep=" ")
  }
  if (is.null(exposition)){
    if (is.null(max_y)) max_y <- max(dat_total[, c("accidents", "expect", "low", "upp")],
                                     na.rm=TRUE)*1.1
    p <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=accidents)) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=low,ymax=upp), fill="grey", alpha=0.5) +
      ggplot2::geom_line() +
      ggplot2::geom_point()  +
      ggplot2::geom_segment(ggplot2::aes(y = ci[1],
                                         x = max(Date), yend=ci[2],
                                         xend=max(Date)), col="blue") +
      ggplot2::geom_point(data=dat_total[dat_total$Date == max(dat_total$Date),],
                          ggplot2::aes(x = Date, y = accidents), col= col_w) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                  expand = c(0, 0), limits = c(0, max_y))+
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
      ggplot2::ggtitle(main) +
      ggplot2::theme_bw()
    if (lang == "de") p <- p + ggplot2::ylab("Unfaelle")
    if (lang == "it") p <- p + ggplot2::ylab("Incidenti")
  }
  if (!is.null(exposition)){
    if (is.null(max_y)) max_y <- max(dat_total[, c("accidents", "expect", "low", "upp")]/dat_total$DTV,
                                     na.rm=TRUE)*1.1
    scal <- 10^(floor(log10(ceiling(1/max_y))) + 1)
    p <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=accidents / DTV* scal)) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = low / DTV * scal, ymax = upp / DTV * scal),
                           fill="grey", alpha=0.5) +
      ggplot2::geom_line() +
      ggplot2::geom_point()  +
      ggplot2::geom_segment(ggplot2::aes(y = ci[1]  / dat_model$DTV[length(dat_model$DTV)] * scal,
                                         x = max(Date),
                                         yend=ci[2] / dat_model$DTV[length(dat_model$DTV)] * scal,
                                         xend=max(Date)), col="blue") +
      ggplot2::geom_point(data=dat_total[dat_total$Date == max(dat_total$Date),],
                          ggplot2::aes(x = Date, y = accidents / DTV* scal),
                          col= col_w) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                  expand = c(0, 0), limits = c(0, max_y*scal))+
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
      ggplot2::ggtitle(main) +
      ggplot2::theme_bw()
    if (lang == "en") p <- p + ggplot2::ylab(paste("Accident rate *", formatC(scal, format = "e", digits = 0)))
    if (lang == "de") p <- p + ggplot2::ylab(paste("Unfallrate *", formatC(scal, format = "e", digits = 0)))
    if (lang == "fr") p <- p + ggplot2::ylab(paste("Taux d'accidents *", formatC(scal, format = "e", digits = 0)))
    if (lang == "it") p <- p + ggplot2::ylab(paste("Tasso di incidenti *", formatC(scal, format = "e", digits = 0)))
  }
  return(list(p = p, ci = ci))
}

