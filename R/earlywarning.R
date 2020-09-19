#' Function to determine the prediction interval for new value in time series
#'
#' @param accidents either an R date/time or character vector with accident data. For character vectors, the following data formats are allowed '2014-04-22', '2014/04/22' respectively '22.4.2014'
#' @param exposition optional data frame with exposition data. The first column is the time value, the second column the exposure. If the time value is a specific date (e.g. '22.4.2014'), this is considered as the start date of this exposure. If the time value is a year (format '2010') the exposure is taken for the whole year. Exposure values are extended until a new entry is available. If necessary, the first exposure value is extended back forwards. DEFAULT NULL
#' @param from from which date or year (1.1.) the time series should be considered. Optional. If not specified, the 1.1 from the year of the earliest accident is used.
#' @param until Until when date or year (31.12) the time series should be considered. Optional. If not specified, the 31.12 from the year of the latest accident is used.
#' @param n number of simulations
#' @param conf.level level of the prediction interval
#' @param main optional title for the plot
#' @param x_axis optional vector with the values for the x-axis
#' @param max_y optional maximum value for the y-axis
#' @param silent parameter to suppress error messages during model evaluation
#' @param lang language for output ("en", "fr", "de" or "it")
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
#'   ex6 <- earlywarning(accidents = example1_timeserie, exposition=exposition_ex2)
#'   print(ex6)
#'   ex7 <- earlywarning(accidents = example3_timeserie, exposition=exposition_ex3)
#'   summary(ex7)


earlywarning <- function(accidents, exposition = NULL, from = NULL, until = NULL, n = 1000, conf.level = 0.9,
                        main = NULL, silent = TRUE, x_axis =NULL, max_y = NULL, lang = "en") {
  ## check mandatory input
  accidents <- try(as.Date(accidents, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
  ## check optional input
  if (!is.null(exposition)){
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
      } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated")
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
      } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated")
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
  # Base plot
  if (is.null(x_axis)) x_axis <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric(format(until, '%Y'))+1), "-01-01"))
  if (is.null(main)){
    if (lang == "en") main <- paste("Early Warning System")
    if (lang == "de") main <- paste("Frühwarnsystem")
    if (lang == "fr") main <- paste("Systeme d'alerte precoce", from, "-", until, sep=" ")
    if (lang == "it") main <- paste("Sistema di allarme rapido", from, "-", until, sep=" ")
  }
  if (is.null(exposition)){
    if (is.null(max_y)) max_y <- max(max(dat_total[, c("accidents", "expect", "low", "upp"),], na.rm=TRUE),
                            ci)*1.1
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
    if (is.null(max_y)) max_y <- max(max(dat_total[, c("accidents", "expect", "low", "upp"),]/dat_total$Exp, na.rm=TRUE),
                                     ci/dat_model$Exp[length(dat_model$Exp)])*1.1
    scal <- 10^(floor(log10(ceiling(1/max_y))) + 1)
    p <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=accidents / Exp* scal)) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=timeserie, colour="darkgrey", linetype=2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = low / Exp * scal, ymax = upp / Exp * scal),
                           fill="grey", alpha=0.5) +
      ggplot2::geom_line() +
      ggplot2::geom_point()  +
      ggplot2::geom_segment(ggplot2::aes(y = ci[1]  / dat_model$Exp[length(dat_model$Exp)] * scal,
                                         x = max(Date),
                                         yend=ci[2] / dat_model$Exp[length(dat_model$Exp)] * scal,
                                         xend=max(Date)), col="blue") +
      ggplot2::geom_point(data=dat_total[dat_total$Date == max(dat_total$Date),],
                          ggplot2::aes(x = Date, y = accidents / Exp* scal),
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
  output <- list( ci = ci, data = dat_total, fit=fit, test_overdisp = test_overdisp, plot = p, lang = lang)
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
  if (object$lang == "en"){
    striking <- "striking"
    nstriking <- "not striking"
    ci <- "Confidence_interval"
    model <- "model"
    nb <- "Negative binomial"
    val <- "observation"
    pv <- "p-value"
    od <- "overdispersion:"
  }
  if (object$lang == "de"){
    striking <- "auffaellig"
    nstriking <- "nicht auffaellig"
    ci <- "Konfidenzintervall"
    model <- "Modell"
    nb <- "Negative Binomial"
    val <- "Beobachtung"
    pv <- "p-Wert"
    od <- "Overdispersion"
  }
  if (object$lang == "fr"){
    striking <- "remarquable"
    nstriking <- "non remarquable"
    ci <- "Intervalle de confiance"
    model <- "model"
    nb <- "binomiale negative"
    val <- "observation"
    pv <- "valeur p"
    od <- "overdispersion"
  }
  if (object$lang == "it"){
    striking <- "remarquable"
    nstriking <- "non remarquable"
    ci <- "Intervallo di confidenza"
    model <- "modello"
    nb <- " binomiale negativa"
    val <- "osservazione"
    pv <- "Valore p"
    od <- "overdispersion"
  }
  print(object$plot)
  if (object$data[dim(object$data)[1],2] >= as.numeric(object$ci)[1] && object$data[dim(object$data)[1],2] <=as.numeric(object$ci)[2]){
    cat(paste0(nstriking, " (", ci," ", round(as.numeric(object$ci)[1],2), "-", round(as.numeric(object$ci)[2],2), "; ",
             val, ": ", object$data[dim(object$data)[1],2], ")"))
  } else {
    cat(paste0(striking, " (", ci," ", as.numeric(object$ci)[1], "-", as.numeric(object$ci)[2], "; ",
               val, ": ", object$data[dim(object$data)[1],2], ")"))
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
}



#' @method plot class_earlywarning
#' @export

"plot.class_earlywarning" <- function(object)
{
  print("x")
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
