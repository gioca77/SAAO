#' Function to evaluate the trend in accident time series
#'
#' @param accidents either an R date/time or character vector with accident data. For character vectors, the following data formats are allowed '2014-04-22', '2014/04/22' respectively '22.4.2014'
#' @param exposition optional data frame with exposition data. The first column is the time value, the second column the exposure. If the time value is a specific date (e.g. '22.4.2014'), this is considered as the start date of this exposure. If the time value is a year (format '2010') the exposure is taken for the whole year. Exposure values are extended until a new entry is available. If necessary, the first exposure value is extended back forwards. DEFAULT NULL
#' @param from from which date or year (1.1.) the time series should be considered. Optional. If not specified, the 1.1 from the year of the earliest accident is used.
#' @param until Until when date or year (31.12) the time series should be considered. Optional. If not specified, the 31.12 from the year of the latest accident is used.
#' @param pearson_line TRUE/FALSE if line for pearson residual equal 2 should be drawn or not
#' @param show_outliers FALSE/TRUE if outliers with Pearson residual greater than 2 should be highlighted in color
#' @param main optional title for the plot
#' @param x_axis optional vector with the values for the x-axis
#' @param max_y optional maximum value for the y-axis
#' @param orientation_x Alignment of the labels of the x-axis; "v" for vertical, "h" for horizontal, by default horizontal alignment is selected for 8 years or less, above that a vertical
#' @param lang language for output ("en", "fr", "de" or "it")
#' @export
#' @examples
#'   ex1 <- timeseriesanalysis(accidents = example1_timeserie)
#'   print(ex1)
#'   plot(ex1)
#'   summary(ex1)
#'   summary(ex1$fit)
#'   ex2 <- timeseriesanalysis(accidents = example2_timeserie, lang = "de")
#'   ex2
#'   ex3 <- timeseriesanalysis(accidents = example3_timeserie, lang = "it")
#'   print(ex3)
#'   ex4 <- timeseriesanalysis(accidents = example4_timeserie, lang = "de")
#'   print(ex4)
#'   plot(ex4)
#'   summary(ex4)
#'   ex5 <- timeseriesanalysis(accidents = example1_timeserie, exposition=exposition_ex1)
#'   plot(ex5)
#'   ex6 <- timeseriesanalysis(accidents = example1_timeserie, exposition=exposition_ex2)
#'   print(ex6)
#'   ex7 <- timeseriesanalysis(accidents = example1_timeserie, exposition=exposition_ex3)
#'   summary(ex7)

timeseriesanalysis <- function(accidents, exposition = NULL, from = NULL, until = NULL,
                               pearson_line = TRUE, show_outliers = FALSE,
                               main = NULL, max_y = NULL, x_axis = NULL,
                               orientation_x = NULL, lang = "en"){
  silent = FALSE # silent: parameter to suppress error messages during model evaluation
  ## check mandatory input
  accidents <- try(as.Date(accidents, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent=silent)
  ## check optional input
  if (!is.null(exposition) & !is.null(dim(exposition))){
    if (dim(exposition)[2]>=2 ){
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
    warning("language unknown, set to english")
  }
  if (!is.null(orientation_x)){
    if (!(orientation_x %in% c("v", "V", "h", "H"))){
      orientation_x <- NULL
      warning('For orientation_x only "v" or "h" are allowed')
    }
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
      argcheck = Check
    )
  #* until time format
  if (is(until)[1]==  "try-error")
    addError_sep(
      msg = "wrong time format for 'until'",
      argcheck = Check
    )
  #* check until > from
  if (is(from)[1]=="Date" & is(until)[1]=="Date"){
    if (from > until)
      addError_sep(msg = "'until has to be greater then from'",
                   argcheck = Check)
  }
  #* Return errors and warnings (if any)
  finishArgCheck_sep(Check)
  if (any(format(accidents, '%Y')<100)) warning('Check the time format. Only the following formats are supported: "%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y"')
  ## processing input
  if (is.null(from)) from <- as.Date(paste0(as.numeric( format(min(accidents), '%Y')), "-01-01"))
  if (is.null(until)) until <- as.Date(paste0(as.numeric( format(max(accidents), '%Y')), "-12-31"))
  if(from > until) stop("until has to be greater then from")
  timeserie <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric( format(until, '%Y'))),"-", format(until+1, '%m-%d')))
  if (from[1]>timeserie[1]) timeserie <- timeserie[-1]
  if (length(timeserie)<3){
    stop("time series too short (less than 3 years)")
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
    fit <- glm(accidents~Date, data=dat_model, family = "poisson")
    ## Test Overdispersion
    test_overdisp <-  pchisq(sum(residuals(fit, type="pearson")^2), df=fit$df.residual, lower.tail=FALSE)
    #  Devianztest: 1 - pchisq(deviance(fit),df.residual(fit))
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
    fit <- glm(accidents ~ Date + offset(log(Exp)), data = dat_model, family = "poisson")
    ## Test Overdispersion
    test_overdisp <- pchisq(sum(residuals(fit, type="pearson")^2), df=fit$df.residual, lower.tail=FALSE)
    #  Devianztest: 1 - pchisq(deviance(fit),df.residual(fit))
    if (test_overdisp <= 0.05){
      fit_start <- glm(accidents ~ Date + offset(log(Exp)), data=dat_model, family = "quasipoisson",
                       control = glm.control(epsilon = 1e-8, maxit = 200, trace = FALSE))
      fit_nb <- try(MASS::glm.nb(accidents ~ Date + offset(log(Exp)), data=dat_model, start=fit_start$coefficients,
                           control = glm.control(epsilon = 1e-6, maxit = 200, trace = FALSE)), silent=TRUE)
      if (attr(fit_nb, "class")[1] != "try-error")
      {
        fit <- fit_nb
        overdisp <- TRUE
      } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated")
    }
  }
  if (fit$df.residual==0){
    stop("Too few data for reliable estimating the model parameters")
  }
  ## Expected value
  dat_model$expect <- predict(fit, type="response")
  ## Calculation of confidence interval
  preds <- predict(fit, type= "link", se.fit = TRUE)
  dat_model$low <- exp(preds$fit - qnorm(0.975) * preds$se.fit)
  dat_model$upp <- exp(preds$fit + qnorm(0.975) * preds$se.fit)
  dat_model$Date <- fit$model$Date
  if(sum(dat_model$upp != Inf) == 0){
    faelle <- dim(dat_model)[1]
    q_Jahr <- 0.05^{1/faelle}
    lambda_est <- -log(q_Jahr)
    dat_model$upp <- lambda_est
  }
  ## Pearson residuals for outliers and pearson-Line
  if (grepl("Negative Binomial", x=fit$family$family)){
    dat_model$pearson_line <- fit$fitted.values + 2 * sqrt(fit$fitted.values+(fit$fitted.values^2/fit$theta))
    dat_model$pearson <- residuals.glm(fit, type="pearson")>=2
    ##stats::rstandard(fit, type="pearson")>=2
  }
  if (fit$family$family=="poisson"){
    dat_model$pearson_line <- fit$fitted.values + 2 * sqrt(fit$fitted.values)
    dat_model$pearson <- residuals.glm(fit, type="pearson")>=2
  }
  dat_model$col <- 1
  if (show_outliers){
    dat_model$col[dat_model$pearson] <- 2
    ## Adjustment of the colors if several outliers in a row
    r.grexp <- gregexpr("22+", paste(dat_model$col, collapse=""))
    if (r.grexp[[1]][1]!=-1){
      for (i in 1:length(r.grexp[[1]]))
      {
        dat_model$col[(r.grexp[[1]][i]+1):(r.grexp[[1]][i]+
                                           attr(r.grexp[[1]],"match.length")[i]-1)] <- 3
      }
    }
  }
  dat_model$col <- factor(dat_model$col, levels=c("1","2", "3"))
  ## visualization
  if (is.null(x_axis)) x_axis <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric(format(until, '%Y'))+1), "-01-01"))
  if (is.null(main)){
    if (lang == "en") main <- paste("Time Series Analysis", from, "-", until, sep=" ")
    if (lang == "de") main <- paste("Zeitreihenanalyse", from, "-", until, sep=" ")
    if (lang == "fr") main <- paste("Analyse des series chronologiques", from, "-", until, sep=" ")
    if (lang == "it") main <- paste("Analisi delle serie temporali", from, "-", until, sep=" ")
  }
  # Base plot
  if (is.null(orientation_x)) orientation_x <- ifelse(diff(as.numeric(format(range(dat_model$Date), '%Y'))) > 8, "v", "h")
  if (is.null(exposition)){
    if (is.null(max_y)){
      max_data <- c(dat_model$accidents, dat_model$expect, dat_model$low, dat_model$upp, dat_model$pearson_line)
      max_y <- max(max_data[is.finite(max_data)], na.rm=TRUE)*1.1
    }
    p <- ggplot2::ggplot(dat_model,  ggplot2::aes(x=Date, y=accidents)) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=low,ymax=upp), fill="grey", alpha=0.5) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour=col)) +
      ggplot2::scale_colour_manual(values = c("black", "orange", "red"), guide = FALSE) +
      ggplot2::geom_line(ggplot2::aes(y=expect), col="blue")+
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), expand = c(0, 0), limits = c(0,max_y)) +
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y")) +
      ggplot2::ggtitle(main) +
      ggplot2::theme_bw()
    if (pearson_line) p <- p + ggplot2::geom_line(ggplot2::aes(x=Date, y=pearson_line), linetype=2, colour="orange")
    if (orientation_x == "v")  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    if (lang == "de") p <- p + ggplot2::ylab("Unfaelle")
    if (lang == "it") p <- p + ggplot2::ylab("Incidenti")
  }
  if (!is.null(exposition)){
    if (is.null(max_y)){
      max_data <- c(dat_model$accidents/dat_model$Exp, dat_model$expect/dat_model$Exp, dat_model$low/dat_model$Exp, dat_model$upp/dat_model$Exp,
                    dat_model$pearson_line/dat_model$Exp)
      max_y <- max(max_data[is.finite(max_data)], na.rm=TRUE)*1.1
    }
    scal <- 10^(floor(log10(ceiling(1/max_y))) + 1)
    p <- ggplot2::ggplot(dat_model,  ggplot2::aes(x=Date, y=accidents/Exp* scal)) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=low/Exp* scal,ymax=upp/Exp* scal), fill="grey", alpha=0.5) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour=col)) +
      ggplot2::scale_colour_manual(values = c("black", "orange", "red"), guide = FALSE) +
      ggplot2::geom_line(ggplot2::aes(y=expect/Exp* scal), col="blue")+
      ggplot2::scale_y_continuous(breaks =  scales::pretty_breaks(), expand = c(0, 0), limits=c(0,max_y*scal))+
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
      ggplot2::ggtitle(main)+
      ggplot2::theme_bw()
    if (pearson_line) p <- p + ggplot2::geom_line(ggplot2::aes(x=Date, y=pearson_line/Exp* scal), linetype=2, colour="orange")
    if (orientation_x == "v")  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    if (lang == "en") p <- p + ggplot2::ylab(paste("Accident rate *", formatC(scal, format = "e", digits = 0)))
    if (lang == "de") p <- p + ggplot2::ylab(paste("Unfallrate *", formatC(scal, format = "e", digits = 0)))
    if (lang == "fr") p <- p + ggplot2::ylab(paste("Taux d'accidents *", formatC(scal, format = "e", digits = 0)))
    if (lang == "it") p <- p + ggplot2::ylab(paste("Tasso di incidenti *", formatC(scal, format = "e", digits = 0)))
  }
  trend <- as.numeric((exp(summary(fit)$coefficients["Date", 1])-1) * difftime(until, from, units="day") / dim(dat_model)[1])
  p_value_trend <- summary(fit)$coefficients["Date", 4]
  if (lang == "de") p <- p + ggplot2::xlab("Datum")
  if (lang == "it") p <- p + ggplot2::xlab("Data")
  output <- list(fit = fit, data = dat_model, trend = trend, p_value_trend = p_value_trend,
                 test_overdisp = test_overdisp, plot= p, lang = lang)
  class(output) <- "class_timeseriesanalyis"
  return(output)
}

#' @method plot class_timeseriesanalyis
#' @export

"print.class_timeseriesanalyis" <- function(object)
{
  if (!inherits(object, "class_timeseriesanalyis"))
  {
    stop("Not a timeseriesanalyis object")
  }
  if (object$lang == "en"){
    reliability <- c("not reliable, no effect proven", "highly reliable", "well reliable", "weakly reliable")
    trend <- "trend"
    rise <- "annual increase of"
    fall <- "annual decrease of"
    model <- "model"
    nb <- "Negative binomial"
    pv <- "p-value"
    od <- "overdispersion"
  }
  if (object$lang == "de"){
    reliability <- c("nicht verlaesslich, keine Wirkung nachgewiesen", "stark verlaesslich", "gut verlaesslich", "schwach verlaesslich")
    trend <- "Trend"
    rise <- "jaehrlicher Anstieg von"
    fall <- "jaehrlicher Abnahme von"
    model <- "Modell"
    nb <- "Negative Binomial"
    pv <- "p-Wert"
    od <- "Overdispersion"
  }
  if (object$lang == "fr"){
    reliability <- c("pas fiable, pas d'effet prouve", "tres fiable", "assez fiable", "faiblement fiable")
    trend <- "Tendance"
    rise <- "augmentation annuelle de"
    fall <- "diminution annuelle de"
    model <- "model"
    nb <- "binomiale negative"
    pv <- "valeur p"
    od <- "overdispersion"
  }
  if (object$lang == "it"){
    reliability <- c("non affidabile, nessun effett provato", "altamente affidabile", "altamente affidabile",
                     "debolmente affidabile")
    trend <- "Tendenza"
    rise <- "aumento annuo del"
    fall <- "decremento annuo del"
    model <- "modello"
    nb <- "binomiale negativa"
    pv <- "Valore p"
    od <- "overdispersion"
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
  if (!is.null(object$fit$theta))
    {
    cat(paste(nb, model, "(Theta =", round(object$fit$theta, 2), ")"))
  }
  if (is.null(object$fit$theta))
  {
    cat(paste(object$fit$family$family, model))
  }
  cat("\n", paste(pv, od, round(object$test_overdisp, 3)))
  cat("\n", paste0(trend,": ", direction, " ", round(object$trend*100, 2), "% (", reliability[k] ,")"))
  cat("\n")
}


#' @method print class_timeseriesanalyis
#' @export

"plot.class_timeseriesanalyis" <- function(object)
{
  if (!inherits(object, "class_timeseriesanalyis"))
  {
    stop("Not a timeseriesanalyis object")
  }
  print(object$plot)
}

#' @method summary class_timeseriesanalyis
#' @export

"summary.class_timeseriesanalyis" <- function(object)
{
  if (!inherits(object, "class_timeseriesanalyis"))
  {
    stop("Not a timeseriesanalyis object")
  }
  sum_output <- list(model = object$fit, data = object$data, trend_percent_change_per_year = object$trend, trend_muliplicator_per_year =1+object$trend, p_value_trend = object$p_value_trend,
             p_value_test_overdispersion = object$test_overdisp)
  sum_output
}

