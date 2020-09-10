#' Function to evaluate the trend in accident time series
#'
#' @param accidents vector with accident data. The following data formats are allowed '2014-04-22', '2014/04/22' respectively '22.4.2014'
#' @param exposition optinal Dataframe with expositions data. The first column is the time value, the second column the exposure. If the time value is a specific date (e.g. '22.4.2014'), this is considered as the start date of this exposure. If the time value is a year (format '2010') the exposure is taken for the whole year. Exposure values are extended until a new entry is available. If necessary, the first exposure value is extended back forwards. DEFAULT NULL
#' @param from From which date or year (1.1) the time series should be considered. Optional. If not specified, the 1.1 from the year of the earliest accident is used.
#' @param until Until when date or year (31.12) the time series should be considered. Optional. If not specified, the 31.12 from the year of the latest accident is used.
#' @param show_outliers TRUE/FALSE if outliers with Pearson residual greater than 2 should be visualized in color
#' @param pearson_line if line for pearson residual equal 2 should be drawn or not
#' @param main optional title for the plot
#' @param x_axis optional vector with the values for the x-axis
#' @param max_y optional maximum value for the y-axis
#' @param silent parameter to suppress error messages during model evaluation
#' @param lang language for output ("en", "fr", "de" or "it")
#' @export
#' @examples
#'   timeseriesanalysis(accidents = example1_timeserie)
#'   timeseriesanalysis(accidents = example2_timeserie, lang = "de")
#'   test <- timeseriesanalysis(accidents = example2_timeserie, lang = "it")
#'   print(test)
#'   # plot(test)
#'   summary(test)

timeseriesanalysis <- function(accidents, exposition = NULL, from = NULL, until = NULL, show_outliers = TRUE,
                               pearson_line = TRUE, main = NULL, max_y = NULL, x_axis = NULL, v = 1, silent = TRUE,
                               lang = "en"){
  ## check mandatory input
  accidents <- try(as.Date(accidents, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent=silent)
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
  #* until time format
  if (is(until)[1]==  "try-error")
    ArgumentCheck::addError(
      msg = "wrong time format for 'until'",
      argcheck = Check
    )

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)


  ## processing input
  if (is.null(from)) from <- as.Date(paste0(as.numeric( format(min(accidents), '%Y')), "-01-01"))
  if (is.null(until)) until <- as.Date(paste0(as.numeric( format(max(accidents), '%Y')), "-12-31"))
  timeserie <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric( format(until, '%Y')) + 1),"-", format(from, '%m-%d')))
  if (until+1<timeserie[length(timeserie)]) timeserie <- timeserie[-length(timeserie)]
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
    fit <- glm(accidents~Date, data=dat_model, family = "poisson")
    ## deviance test
    test_overdisp <- 1 - pchisq(deviance(fit),df.residual(fit))
    if (v == 2){
      test_overdisp <- performance::check_overdispersion(fit)$p_value
    }
    if (test_overdisp <= 0.05){
      fit_start <- glm(accidents ~ Date, data=dat_model, family = "quasipoisson",
                       control = glm.control(epsilon = 1e-8, maxit = 200, trace = FALSE))
      fit_nb <- try(MASS::glm.nb(accidents~Date, data=dat_model, start=fit_start$coefficients,
                           control = glm.control(epsilon = 1e-6, maxit = 200, trace = FALSE)), silent = TRUE)
      if (attr(fit_nb, "class")[1] != "try-error") {
        fit <- fit_nb
      } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated")
    }
  }
  if (!is.null(exposition)){
    ## Poisson_Modell
    fit <- glm(accidents ~ Date + offset(log(DTV)), data = dat_model, family = "poisson")
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
      fit_start <- glm(accidents ~ Date + offset(log(DTV)), data=dat_model, family = "quasipoisson",
                       control = glm.control(epsilon = 1e-8, maxit = 200, trace = FALSE))
      fit_nb <- try(MASS::glm.nb(accidents ~ Date + offset(log(DTV)), data=dat_model, start=fit_start$coefficients,
                           control = glm.control(epsilon = 1e-6, maxit = 200, trace = FALSE)), silent=TRUE)
      if (attr(fit_nb, "class")[1] != "try-error")
      {
        fit <- fit_nb
        overdisp <- TRUE
      } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated")
    }
  }
  ## Pearson residuals for outliers
  dat_fit <- data.frame(pearson = residuals.glm(fit, type="pearson")>=2,
                        row.names = attr(fit$qr$qr, "dimnames")[[1]])
  dat_fit$col <- 1
  if (show_outliers){
    dat_fit$col[dat_fit$pearson] <- 2
    ## Adjustment of the colors if several outliers in a row
    r.grexp <- gregexpr("22+", paste(dat_fit$col, collapse=""))
    if (r.grexp[[1]][1]!=-1){
      for (i in 1:length(r.grexp[[1]]))
      {
        dat_fit$col[(r.grexp[[1]][i]+1):(r.grexp[[1]][i]+
                                           attr(r.grexp[[1]],"match.length")[i]-1)] <- 3
      }
    }
  }
  dat_fit$col <- factor(dat_fit$col, levels=c("1","2", "3"))
  ## Expected value
  dat_fit$expect <- predict(fit, type="response")
  ## Calculation of confidence interval
  preds <- predict(fit, type= "link", se.fit = TRUE)
  dat_fit$low <- exp(preds$fit - qnorm(0.975) * preds$se.fit)
  dat_fit$upp <- exp(preds$fit + qnorm(0.975) * preds$se.fit)
  dat_fit$Date <- fit$model$Date
  dat_total <- merge(x=dat_model, y=dat_fit, by="Date", all=TRUE)
  ## Pearsionlinie
  if (overdisp){
    dat_total$pearson_line <- 2 * sqrt(fit$fitted.values+fit$fitted.values^2 / fit$theta) + fit$fitted.values
  }
  if (!overdisp){
    dat_total$pearson_line <- 2 * sqrt(fit$fitted.values) + fit$fitted.values
  }
  # Modelldevianz <- 1-pchisq(fit$null-fit$dev, df=(fit$df.null-fit$df.res))
  # Residualdevianz <- 1-pchisq(fit$dev, df=(fit$df.res))
  ## visualization
  if (is.null(x_axis)) x_axis <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric(format(until, '%Y'))+1), "-01-01"))
  if (is.null(main)){
    if (lang == "en") main <- paste("Time Series Analysis", from, "-", until, sep=" ")
    if (lang == "de") main <- paste("Zeitreihenanalyse", from, "-", until, sep=" ")
    if (lang == "fr") main <- paste("Analyse des series chronologiques", from, "-", until, sep=" ")
    if (lang == "it") main <- paste("Analisi delle serie temporali", from, "-", until, sep=" ")
  }
  # Base plot
  if (is.null(exposition)){
    if (is.null(max_y)) max_y <- max(dat_total[, c("accidents", "expect", "low", "upp", "pearson_line")],
                                     na.rm=TRUE)*1.1
    p <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=accidents)) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=low,ymax=upp), fill="grey", alpha=0.5) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour=col)) +
      ggplot2::scale_colour_manual(values = c("black", "orange", "red"), guide = FALSE) +
      ggplot2::geom_line(ggplot2::aes(y=expect), col="blue")+
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), expand = c(0, 0), limits = c(0,max_y))+
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
      ggplot2::ggtitle(main) +
      ggplot2::theme_bw()
    if (pearson_line) p <- p + ggplot2::geom_line(ggplot2::aes(x=Date, y=pearson_line), linetype=2, colour="orange")
    if (lang == "de") p <- p + ggplot2::ylab("Unfaelle")
    if (lang == "it") p <- p + ggplot2::ylab("Incidenti")
  }
  if (!is.null(exposition)){
    if (is.null(max_y)) max_y <- max(dat_total[, c("accidents", "expect", "low", "upp", "pearson_line")]/dat_total$DTV,
                                     na.rm=TRUE)*1.1
    scal <- 10^(floor(log10(ceiling(1/max_y))) + 1)
    p <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=accidents/DTV* scal)) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=low/DTV* scal,ymax=upp/DTV* scal), fill="grey", alpha=0.5) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour=col)) +
      ggplot2::scale_colour_manual(values = c("black", "orange", "red"), guide = FALSE) +
      ggplot2::geom_line(ggplot2::aes(y=expect/DTV* scal), col="blue")+
      ggplot2::scale_y_continuous(breaks =  scales::pretty_breaks(), expand = c(0, 0), limits=c(0,max_y*scal))+
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
      ggplot2::ggtitle(main)+
      ggplot2::theme_bw()
    if (pearson_line) p <- p + ggplot2::geom_line(ggplot2::aes(x=Date, y=pearson_line/DTV* scal), linetype=2, colour="orange")
    if (lang == "en") p <- p + ggplot2::ylab(paste("Accident rate *", formatC(scal, format = "e", digits = 0)))
    if (lang == "de") p <- p + ggplot2::ylab(paste("Unfallrate *", formatC(scal, format = "e", digits = 0)))
    if (lang == "fr") p <- p + ggplot2::ylab(paste("Taux d'accidents *", formatC(scal, format = "e", digits = 0)))
    if (lang == "it") p <- p + ggplot2::ylab(paste("Tasso di incidenti *", formatC(scal, format = "e", digits = 0)))
  }
  trend <- as.numeric((exp(summary(fit)$coefficients["Date", 1])-1) * difftime(until, from, units="day") / dim(dat_total)[1])
  p_value_trend <- summary(fit)$coefficients["Date", 4]
  if (lang == "de") p <- p + ggplot2::xlab("Datum")
  if (lang == "it") p <- p + ggplot2::xlab("Date")
  output <- list(fit = fit, data = dat_total, trend = trend, p_value_trend = p_value_trend,
                 test_overdisp = test_overdisp, plot= p, lang = lang)
  class(output) <- "class_timeseriesanalyis"
  return(output)
}


"print.class_timeseriesanalyis" <- function(object)
{
  if (!inherits(object, "class_timeseriesanalyis"))
  {
    stop("Not a timeseriesanalyis object")
  }
  if (object$lang == "en"){
    reliability <- c("not reliable, no effect", "highly reliable", "well reliable", "weakly reliable")
    trend <- "trend"
    rise <- "annual increase of"
    fall <- "annual decrease of"
    model <- "model"
  }
  if (object$lang == "de"){
    reliability <- c("nicht verlaesslich, keine Wirkung", "stark verlaesslich", "gut verlaesslich", "schwach verlaesslich")
    trend <- "Trend"
    rise <- "jaehrlicher Anstieg von"
    fall <- "jaehrlicher Abnahme von"
    model <- "Modell"
  }
  if (object$lang == "fr"){
    reliability <- c("pas fiable, pas d'effet", "tres fiable", "assez fiable", "faiblement fiable")
    trend <- "Tendance"
    rise <- "augmentation annuelle de"
    fall <- "diminution annuelle de"
    model <- "model"
  }
  if (object$lang == "it"){
    reliability <- c("non affidabile, nessun effett", "altamente affidabile", "altamente affidabile",
                     "debolmente affidabile")
    trend <- "Tendenza"
    rise <- "aumento annuo del"
    fall <- "decremento annuo del"
    model <- "modello"
  }
  direction <- ifelse(object$trend >= 0, rise, fall)
  if(is.na(object$p_value_trend))
    {
    k <- 1
    } else if(object$p_value_trend <= 0.01 && object$p_value_trend >= 0)
      {
      k<- 2
      } else if (object$p_value_trend > 0.01 && object$p_value_trend <= 0.05)
        {
        k <- 3
        } else if (object$p_value_trend > 0.05 && object$p_value_trend <= 0.10)
          {
          k <- 4
          } else k <- 1
  print(object$plot)
  cat(paste(object$fit$family$family, model))
  cat("\n", paste0(trend,": ", direction, " ", round(object$trend*100, 2), "% (", reliability[k] ,")"))
}




"plot.class_timeseriesanalyis" <- function(object)
{
  if (!inherits(object, "class_timeseriesanalyis"))
  {
    stop("Not a timeseriesanalyis object")
  }
  print(object$plot)
}

"summary.class_timeseriesanalyis" <- function(object)
{
  if (!inherits(object, "class_timeseriesanalyis"))
  {
    stop("Not a timeseriesanalyis object")
  }
  sum_output <- list(model = object$fit, data = object$data, trend = object$trend, p_value_trend = object$p_value_trend,
             p_value_test_overdispersion = object$test_overdisp)
  sum_output
}

