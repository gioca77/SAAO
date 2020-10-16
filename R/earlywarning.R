#' Early warning system for a new data point
#'
#' @description Function to determine the return period / prediction interval for new value in a traffic accident time series.
#'
#' @details
#' The function provides a tool to evaluate if a new data point in a traffic accident count data time-series is compatible with the past data. The return period or the prediction interval for the new data point is determined from the past data by a simulation. If the data point is striking, this is an indication that something has changed substantially in the last year.
# 'The simulation of the interval is passed on the Poisson or, if overdispersion is detected, Negative Binomial model with the accident data in the previous years. With the model a prediction for the new year is executed. The expected value for the new observation is simulated from a normal distribution with the predicted value and the standard deviation of the prediction on the link scale. The simulated expected values are transformed back into the observation scale and used to simulate random observations using the Poisson or Negative Binomial distribution.
#' @param accidents Either an R date/time or character vector with accident dates. For character vectors, the following date formats are allowed '2014-04-22', '2014/04/22' respectively '22.4.2014'.
#' @param exposition Optional data frame with exposition data. The first column is the time value, the second column the exposure. If the time value is a specific date (e.g. '22.4.2014'), this is considered as the start date of this exposure. If the time value is a year (format '2010') the exposure is taken for the whole year. Exposure values are extended until a new entry is available. If necessary, the first exposure value is extended backwards. DEFAULT NULL.
#' @param from From which date or year (1.1.) the time series should be considered. Optional. If not specified, the 1.1 from the year of the earliest accident is used.
#' @param until Until what date or year (31.12) the time series should be considered. Optional. If not specified, the 31.12 from the year of the latest accident is used.
#' @param n Number of simulations
#' @param pred.level Level of the prediction interval, if NULL (default) an interval with return periods is constructed
#' @param main Optional title for the plot.
#' @param x_axis Optional vector with the values for the x-axis.
#' @param max_y Optional maximum value for the y-axis.
#' @param orientation_x Alignment of the labels of the x-axis; "v" for vertical, "h" for horizontal, by default horizontal alignment is selected for 8 years or less.
#' @param add_exp Option to supplement the output plot with the exposure as an additional axis. Furthermore an additionally plot of the exposure alone is produced. Only active if exposure is stated.
#' @param lang Language for output ("en", "fr", "de" or "it").
#' @seealso \code{\link[STAAD:timeseriesanalysis]{timeseriesanalysis()}} function to evaluate the trend in a traffic accident time series.
#' @return A specific R object (\code{class_earlywarning}) is generated as function output. The main object is the illustration with the prediction interval for the newest observation in the time-serie. The function \code{print.class_earlywarnin()} is used to extract the most important key figures of the analysis.
#' Specifically, the output contains a list of the following elements:
#' \item{\code{ci}}{Prediction interval or limits for a 5-, 10-, 20- and 100-year event for the new value.}
#' \item{\code{fit}}{Output of the counting regression model (Negative Binomial or Poisson family.}
#' \item{\code{return_period}}{Return period for the new value.}
#' \item{\code{data}}{Prepared data that were used for the analysis.}
#' \item{\code{test_overdisp}}{p-value of the deviance dispersion test.}
#' \item{\code{pred.level}}{Level of the prediction interval.}
#' \item{\code{plot}}{Plot graphical analysis (ggplot-class).}
#' \item{\code{lang}}{Selected language.}
#' \item{\code{plot_exposition}}{Addional plot of the exposition, if available (ggplot-class).}
#'
#' @export
#' @examples
#'   ex1 <- earlywarning(accidents = example1_timeserie)
#'   print(ex1)
#'   plot(ex1)
#'   summary(ex1)
#'   summary(ex1$fit)
#'   ex2 <- earlywarning(accidents = example2_timeserie, lang = "de")
#'   ex2
#'   ex3 <- earlywarning(accidents = example3_timeserie, lang = "it")
#'   print(ex3)
#'   ex4 <- earlywarning(accidents = example4_timeserie, lang = "de")
#'   print(ex4)
#'   plot(ex4)
#'   summary(ex4)
#'   ex5 <- earlywarning(accidents = example1_timeserie, exposition=exposition_ex1)
#'   plot(ex5)
#'   ex6 <- earlywarning(accidents = example1_timeserie, exposition=exposition_ex2, add_exp = TRUE)
#'   print(ex6)
#'   plot(ex6$plot_exposition)
#'   ex7 <- earlywarning(accidents = example3_timeserie, exposition=exposition_ex3)
#'   summary(ex7)

earlywarning <- function(accidents, exposition = NULL, from = NULL, until = NULL, n = 10000,
                         pred.level = NULL, main = NULL,  x_axis = NULL, max_y = NULL,
                         orientation_x = NULL, add_exp = FALSE, lang = "en") {
  silent = FALSE # silent: parameter to suppress error messages during model evaluation
  ## check mandatory input
  accidents <- try(as.Date(accidents, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")),
                   silent = silent)
  ## check optional input
  if (!is.null(exposition) & !is.null(dim(exposition))){
    if (dim(exposition)[2]>=2){
      colnames(exposition)[1:2] <- c("time", "Exp")
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
    warning("language unknown, set to English")
  }
  if (!is.null(orientation_x)){
    if (!(orientation_x %in% c("v", "V", "h", "H"))){
      orientation_x <- NULL
      warning('For orientation_x only "v" or "h" are allowed')
    }
  }
  if (is.null(add_exp) || !(add_exp %in% c(TRUE, FALSE))){
    add_exp <- FALSE
    warning('For add_exp only TRUE and FALSE are allowed. Set to FALSE')
  }
  Check <- newArgCheck_sep()
  #* accidents format
  if (is(accidents)[1]==  "try-error")
    addError_sep(
      msg = "'accidents' not in the right format",
      argcheck = Check
    )
  #* exposition format
  if (!is.null(exposition) & !is.null(dim(exposition))){
    if (dim(exposition)[2]<2)
      addError_sep(
        msg = "wrong input for 'exposition'",
        argcheck = Check
      )
    #* exposition time format
    if (is(exposition)[1]==  "try-error")
      addError_sep(
        msg = "wrong time format for 'exposition'",
        argcheck = Check
      )
  }
  if (!is.null(exposition) & is.null(dim(exposition))){
    addError_sep(
      msg = "wrong input for 'exposition'",
      argcheck = Check)
  }
  #* from time format
  if (is(from)[1]==  "try-error")
    addError_sep(
      msg = "wrong time format for 'from'",
      argcheck = Check)
  #* until time format
  if (is(until)[1]==  "try-error")
    addError_sep(
      msg = "wrong time format for 'until'",
      argcheck = Check)
  #* pred.level format
  if (!is.null(pred.level)){
    if (pred.level<0 | pred.level>1)
      addError_sep(
        msg = "'pred.level' must be between 0 and 1",
        argcheck = Check)
    }
  #* check until > from
  if (is(from)[1]=="Date" & is(until)[1]=="Date"){
    if (from > until)
      addError_sep(msg = "'until has to be greater than from'",
                   argcheck = Check)
  }
  #* Return errors and warnings (if any)
  finishArgCheck_sep(Check)
  if (any(format(accidents, '%Y')<100)) warning('Check the time format. Only the following formats are supported: "%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y"')
  ## processing input
  if (is.null(from)) from <- as.Date(paste0(as.numeric( format(min(accidents), '%Y')), "-01-01"))
  if (is.null(until)) until <- as.Date(paste0(as.numeric( format(max(accidents), '%Y')), "-12-31"))
  if(from > until) stop("until has to be greater than from")
  timeserie <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric(format(until + 1, '%Y'))),"-",
                              format(until + 1, '%m-%d')))
  if (from[1] > timeserie[1]) timeserie <- timeserie[-1]
  if (length(timeserie)<4){
    stop("Time series too short (less than 4 years).")
  }
  dat_model <- as.data.frame(matrix(NA, nrow=length(timeserie)-1, ncol=2))
  colnames(dat_model) <- c("Date", "accidents")
  dat_model$Date <- as.Date((as.numeric(timeserie[1:(length(timeserie) - 1)]) + as.numeric(timeserie[2:length(timeserie)] - 1)) / 2, origin="1970-01-01")
  for (i in 1:(length(timeserie) - 1)){
    dat_model[i, 2] <- sum(accidents >= timeserie[i] & accidents < timeserie[i+1])
  }
  ## add exposition to data
  if (!is.null(exposition)){
    tab <- data.frame(Date=seq(from, until, 1), Exp = exposition$Exp[1])
    if (is(exposition$time)[1] == "Date"){
      index <- match(tab$Date, exposition$time)
      if(length(index) > 2){
        for (i in 2:(length(index) - 1)){
          tab$Exp[tab$Date >= exposition$time[i] & tab$Date<exposition$time[i+1]] <- exposition$Exp[i]
        }
      }
      tab$Exp[tab$Date >= exposition$time[length(index)]] <- exposition$Exp[length(index)]
    }
    if (is.numeric(exposition$time)){
      for (i in 1:length(exposition$time)){
        tab$Exp[as.numeric(format(tab$Date, '%Y')) >= exposition$time[i]] <- exposition$Exp[i]
      }
    }
    dat_model$Exp <- NA
    for (i in 1:(length(timeserie)-1)){
      dat_model$Exp[i] <- mean(tab$Exp[tab$Date >= timeserie[i] & tab$Date < timeserie[i+1]])
    }
  }
  ## Model
  overdisp <- FALSE
  if (is.null(exposition)){
    ## Poisson_Modell
    fit <- glm(accidents~Date, data=dat_model[-dim(dat_model)[1], ], family = "poisson")
    ## Test Overdispersion
    test_overdisp <-  pchisq(sum(residuals(fit, type="pearson")^2), df=fit$df.residual, lower.tail=FALSE)
    #  Devianztest: 1 - pchisq(deviance(fit),df.residual(fit))
    if (test_overdisp <= 0.05){
      fit_start <- glm(accidents ~ Date, data=dat_model[-dim(dat_model)[1], ], family = "quasipoisson",
                       control = glm.control(epsilon = 1e-8, maxit = 200, trace = FALSE))
      fit_nb <- try(MASS::glm.nb(accidents~Date, data=dat_model[-dim(dat_model)[1], ], start=fit_start$coefficients,
                                 control = glm.control(epsilon = 1e-6, maxit = 200, trace = FALSE)), silent = TRUE)
      if (attr(fit_nb, "class")[1] != "try-error") {
        fit <- fit_nb
      } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated.")
    }
  }
  if (!is.null(exposition)){
    ## Poisson_Modell
    fit <- glm(accidents ~ Date + offset(log(Exp)), data = dat_model[-dim(dat_model)[1], ], family = "poisson")
    ## Test Overdispersion
    test_overdisp <- pchisq(sum(residuals(fit, type="pearson")^2), df=fit$df.residual, lower.tail=FALSE)
    #  Devianztest: 1 - pchisq(deviance(fit),df.residual(fit))
    if (test_overdisp <= 0.05){
      fit_start <- glm(accidents ~ Date + offset(log(Exp)), data=dat_model[-dim(dat_model)[1], ], family = "quasipoisson",
                       control = glm.control(epsilon = 1e-8, maxit = 200, trace = FALSE))
      fit_nb <- try(MASS::glm.nb(accidents ~ Date + offset(log(Exp)), data=dat_model[-dim(dat_model)[1], ], start=fit_start$coefficients,
                                 control = glm.control(epsilon = 1e-6, maxit = 200, trace = FALSE)), silent=TRUE)
      if (attr(fit_nb, "class")[1] != "try-error")
      {
        fit <- fit_nb
        overdisp <- TRUE
      } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated.")
    }
  }
  pred_new <- predict(fit, newdata=dat_model[dim(dat_model)[1], ], se.fit=TRUE, type="link")
  samp <- rnorm(n,  mean=pred_new$fit, sd=pred_new$se.fit)
  if (!is.null(fit$theta))
  {
    y_int <- rnbinom(n, mu=exp(samp), size = summary(fit)$theta)
  }
  if (is.null(fit$theta))
  {
    y_int <- rpois(n, lambda = exp(samp))
  }
  if (!is.null(pred.level)){
    lp <- (1 - pred.level) / 2
    up <- 1 - lp
    ci <- as.data.frame(matrix(quantile(y_int, c(lp, up), na.rm=TRUE),  nrow=1))
  }
  if (is.null(pred.level)){
    ci <- as.data.frame(matrix(data=quantile(y_int, c(0.005, 0.995, 0.025, 0.975,0.05, 0.95, 0.1, 0.9), na.rm=TRUE), nrow=4, byrow=T))
  }
  colnames(ci)[1:2] <- c("min", "max")
  ci$Date <- dat_model[dim(dat_model)[1], "Date"]
  ci$level <- factor(dim(ci)[1]:1, levels = 1:dim(ci)[1])
  ci$size <- c(1, 1.2, 1.4, 1.6)[1:dim(ci)[1]]
  dat_fit <- NULL
  preds <- predict(fit, type= "link", se.fit = TRUE)
  dat_fit <- data.frame(Date = fit$model$Date,
                        expect = exp(preds$fit),
                        low = exp(preds$fit-qnorm(0.975)*preds$se.fit),
                        upp = exp(preds$fit+qnorm(0.975)*preds$se.fit))

  if(sum(dat_fit$upp != Inf) != 0){
    return_period <- 1/(sum(y_int>=dat_model[dim(dat_model)[1], "accidents"], na.rm=TRUE)/n)
    if(return_period==Inf) return_period <- paste(">", 1/(1/n))
  }
  if(sum(dat_fit$upp != Inf) == 0){
    faelle <- dim(dat_fit)[1]
    q_Jahr <- 0.05^{1/faelle}
    lambda_est <- -log(q_Jahr)
    dat_fit$upp <- lambda_est
    if (!is.null(pred.level)){
      ci[1, 1:2] <- c(0, qpois(pred.level, lambda_est))
    }
    if (is.null(pred.level)){
      ci[, 1:2] <- as.data.frame(matrix(data=qpois(c(0.005, 0.995, 0.025, 0.975,0.05, 0.95, 0.1, 0.9), lambda_est), nrow=4, byrow=T))
    }
    return_period <- 1/(1-ppois(dat_model[dim(dat_model)[1], "accidents"], lambda_est))
    if(return_period > 1/(1/n)) return_period <- paste(">", 1/(1/n))
    warning(paste0("Since all values are 0, the prediction interval cannot be determined by simulation. Estimation by quantile of the Poisson distribution."))
  }
  dat_total <- merge(x=dat_model, y=dat_fit, by="Date", all=TRUE)
  col_w <- ifelse(dat_total[dat_total$Date==max(dat_total$Date),"accidents"] > ci[1,2], "red", "black")
  # Base plot
  if (is.null(orientation_x)) orientation_x <- ifelse(diff(as.numeric(format(range(dat_total$Date), '%Y'))) > 8, "v", "h")
  if (is.null(x_axis)) x_axis <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric(format(until, '%Y'))+1), "-01-01"))
  if (is.null(main)){
    if (lang == "en") main <- paste("Early Warning System")
    if (lang == "de") main <- paste("Frühwarnsystem")
    if (lang == "fr") main <- paste("Systeme d'alerte precoce", from, "-", until, sep=" ")
    if (lang == "it") main <- paste("Sistema di allarme rapido", from, "-", until, sep=" ")
  }
  if (is.null(exposition)){
    if (is.null(max_y)){
      max_data <- c(dat_total$accidents, dat_total$expect, dat_total$low, dat_total$upp, ci$max)
      max_y <- max(max_data[is.finite(max_data)], na.rm=TRUE)*1.1
    }
     p <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=accidents)) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=low,ymax=upp), fill="grey", alpha=0.5) +
      ggplot2::geom_line() +
      ggplot2::geom_point()  +
      ggplot2::geom_segment(data=ci, ggplot2::aes(y = min,
                                         x = Date, yend=max,
                                         xend=Date, col=level), size=ci$size) +
      ggplot2::geom_line() +
      ggplot2::geom_point(data=dat_total[dat_total$Date == max(dat_total$Date),],
                          ggplot2::aes(x = Date, y = accidents), col= col_w) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                  expand = c(0, 0), limits = c(0, max_y))+
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
      ggplot2::ggtitle(main) +
      ggplot2::theme_bw()
  }
  if (!is.null(exposition)){
    if (is.null(max_y)){
      max_data <- c(dat_total$accidents/dat_total$Exp, dat_total$expect/dat_total$Exp, dat_total$low/dat_total$Exp, dat_total$upp/dat_total$Exp,
                    ci$max/dat_model$Exp[length(dat_model$Exp)])
      max_y <- max(max_data[is.finite(max_data)], na.rm=TRUE)*1.1
    }
    scal <- 10^(floor(log10(ceiling(1/max_y))) + 1)
    p <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=accidents / Exp* scal)) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = low / Exp * scal, ymax = upp / Exp * scal),
                           fill="grey", alpha=0.5) +
      ggplot2::geom_line() +
      ggplot2::geom_point()  +
      ggplot2::geom_segment(data=ci, ggplot2::aes(y = min/ dat_model$Exp[length(dat_model$Exp)] * scal,
                                                  x = Date, yend=max/ dat_model$Exp[length(dat_model$Exp)] * scal,
                                                  xend=Date, col=level), size=ci$size) +
      ggplot2::geom_line() +
      ggplot2::geom_point(data=dat_total[dat_total$Date == max(dat_total$Date),],
                          ggplot2::aes(x = Date, y = accidents / Exp* scal),
                          col= col_w) +
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
      ggplot2::ggtitle(main) +
      ggplot2::theme_bw()
    if (!add_exp) p <- p + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                                       expand = c(0, 0), limits = c(0, max_y*scal))
    if (lang == "en") p <- p + ggplot2::ylab(paste("Accident rate *", formatC(scal, format = "e", digits = 0)))
    if (lang == "de") p <- p + ggplot2::ylab(paste("Unfallrate *", formatC(scal, format = "e", digits = 0)))
    if (lang == "fr") p <- p + ggplot2::ylab(paste("Taux d'accidents *", formatC(scal, format = "e", digits = 0)))
    if (lang == "it") p <- p + ggplot2::ylab(paste("Tasso di incidenti *", formatC(scal, format = "e", digits = 0)))
    if (add_exp){
      scal_acci <- ceiling(max(dat_total$Exp)/(max_y*scal)) * 1.05
      p <- p+ggplot2::geom_point(data=dat_total, ggplot2::aes(y = Exp/scal_acci), colour = "grey")+
        ggplot2::geom_line(data=dat_total, ggplot2::aes(y = Exp/scal_acci), colour = "grey")+
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                    expand = c(0, 0), limits = c(0, max_y * scal),
                                    sec.axis=ggplot2::sec_axis(~.*scal_acci * 1.1, name="exposition"))
      p2 <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=Exp)) +
        ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
        ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
        ggplot2::geom_line() +
        ggplot2::geom_point()  +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                    expand = c(0, 0), limits = c(0, max(dat_total$Exp, na.rm = TRUE)*1.1))+
        ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
        ggplot2::ggtitle(main) +
        ggplot2::ylab("exposition") +
        ggplot2::theme_bw()
      if (orientation_x == "v")  p2 <- p2 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
      if (lang == "de") p2 <- p2 + ggplot2::ylab("Exposition")
    }
  }
  if (orientation_x == "v")  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  if (lang == "en" & is.null(pred.level)) p <- p +
    ggplot2::scale_color_manual(name = "Return period",
                                values = c("1"="brown", "2"="blue", "3"="green", "4"="orange"),
                                labels = c("5-year event", "10-year event", "20-year event", "100-year event"))
  if (lang == "en" & !is.null(pred.level)) p <- p +
    ggplot2::scale_color_manual(name = "",
                                values = c("1"="orange"),
                                labels = paste0(pred.level*100, "% prediction interval"))
  if (lang == "de" & is.null(pred.level)) p <- p + ggplot2::ylab("Unfaelle")+
    ggplot2::scale_color_manual(name = "Jaehrlichkeit",
                                values = c("1"="brown", "2"="blue", "3"="green", "4"="orange"),
                                labels = c("5-jaehriges Ereignis", "10-jaehriges Ereignis", "20-jaehriges Ereignis", "100-jaehriges Ereignis"))
  if (lang == "de" & !is.null(pred.level)) p <- p +
    ggplot2::scale_color_manual(name = "",
                                values = c("1"="orange"),
                                labels = paste0(pred.level*100, "% intervalle de fluctuation"))
  if (lang == "fr" & is.null(pred.level)) p <- p +
    ggplot2::scale_color_manual(name = "Periode de retour",
                                values = c("1"="brown", "2"="blue", "3"="green", "4"="orange"),
                                labels = c("evenement 5 ans", "evenement 10 ans", "venement 20 ans",  "evenement 100 ans"))
  if (lang == "fr" & !is.null(pred.level)) p <- p +
    ggplot2::scale_color_manual(name = "",
                                values = c("1"="orange"),
                                labels = paste0(pred.level*100, "% intervalle de fluctuation"))

  if (lang == "it" & is.null(pred.level)) p <- p + ggplot2::ylab("Incidenti") +
    ggplot2::scale_color_manual(name = "Tempo di ritorno",
                                values = c("1"="brown", "2"="blue", "3"="green", "4"="orange"),
                                labels = c("evento dei 5 anni", "evento dei 10 anni", "evento dei 20 anni", "evento dei 100 anni"))
  if (lang == "it" & !is.null(pred.level)) p <- p +
    ggplot2::scale_color_manual(name = "",
                                values = c("1"="orange"),
                                labels = paste0(pred.level*100, "% intervallo di previsione"))
  if (lang == "de") p <- p + ggplot2::xlab("Datum")
  if (lang == "it") p <- p + ggplot2::xlab("Data")
  if (!add_exp | is.null(exposition)) output <- list(ci = ci[,1:2], fit = fit, return_period = return_period, data = dat_total,
                                                     test_overdisp = test_overdisp, plot = p, pred.level = pred.level, lang = lang)
  if (add_exp & !is.null(exposition)) output <- list(ci = ci[,1:2], fit = fit, return_period = return_period, data = dat_total,
                                                     test_overdisp = test_overdisp, plot = p, pred.level = pred.level, lang = lang,
                                                     plot_exposition = p2)
  class(output) <- "class_earlywarning"
  return(output)
}


#' @method print class_earlywarning
#' @export

"print.class_earlywarning" <- function(object)
{
  if (!inherits(object, "class_earlywarning"))
  {
    stop("Not a earlywarning object")
  }
  rp_w <- ifelse(is.numeric(object$return_period), round(object$return_period, 0), object$return_period)
  if (object$lang == "en"){
    striking <- "striking"
    nstriking <- "not striking"
    ci <- "Prognoseintervall"
    model <- "model"
    nb <- "Negative binomial"
    val <- "observation"
    pv <- "p-value"
    od <- "overdispersion"
    rp <- paste0(rp_w, "-year event")
  }
  if (object$lang == "de"){
    striking <- "auffaellig"
    nstriking <- "nicht auffaellig"
    ci <- "prediction interval"
    model <- "Modell"
    nb <- "Negative Binomial"
    val <- "Beobachtung"
    pv <- "p-Wert"
    od <- "Overdispersion"
    rp <- paste0(rp_w, "-jaehriges Ereignis")
  }
  if (object$lang == "fr"){
    striking <- "remarquable"
    nstriking <- "non remarquable"
    ci <- "intervalle de fluctuation"
    model <- "model"
    nb <- "binomiale negative"
    val <- "observation"
    pv <- "valeur p"
    od <- "overdispersion"
    rp <- paste0("evenement ", rp_w, " ans")
  }
  if (object$lang == "it"){
    striking <- "remarquable"
    nstriking <- "non remarquable"
    ci <- "intervallo di previsione"
    model <- "modello"
    nb <- " binomiale negativa"
    val <- "osservazione"
    pv <- "Valore p"
    od <- "overdispersion"
    rp <- paste0("evento dei ", rp_w, " anni")
  }
  print(object$plot)
  if (is.null(object$pred.level)){
    cat(rp)
  }
  if (!is.null(object$pred.level)){
    if (object$data[dim(object$data)[1],2] >= object$ci[1,1] && object$data[dim(object$data)[1],2] <= object$ci[1,2]){
      cat(paste0(nstriking, " (", object$pred.level*100, "%-", ci," ", round(object$ci[1,1],2), "-", round(object$ci[1, 2],2), "; ",
                 val, ": ", object$data[dim(object$data)[1],2], ")"))
      } else {
        cat(paste0(striking, " (", object$pred.level*100, "%-", ci," ", object$ci[1,1], "-", object$ci[1,2], "; ",
               val, ": ", object$data[dim(object$data)[1],2], ")"))
      }
  }
  if (!is.null(object$fit$theta))
  {
    cat("\n", paste(nb, model, "(Theta =", round(object$fit$theta, 2), ")"))
  }
  if (is.null(object$fit$theta))
  {
    cat("\n", paste(object$fit$family$family, model))
  }
  cat("\n", paste(pv, od, round(object$test_overdisp, 3)))
  cat("\n")
}



#' @method plot class_earlywarning
#' @export

"plot.class_earlywarning" <- function(object)
{
  if (!inherits(object, "class_earlywarning"))
  {
    stop("Not a earlywarning object")
  }
  print(object$plot)
}

#' @method summary class_earlywarning
#' @export
"summary.class_earlywarning" <- function(object)
{
  if (!inherits(object, "class_earlywarning"))
  {
    stop("Not a earlywarning object")
  }
  sum_output <- list(Confidence_interval = object$ci, data = object$data, fit = object$fit, p_value_test_overdispersion = object$test_overdisp)
  sum_output
}
