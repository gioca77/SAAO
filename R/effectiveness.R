#' Evaluation of effectiveness of measures
#'
#' @description Function to evaluate the effectiveness of measures to reduce traffic accidents.
#'
#' @details
#' Traffic accident counts (or rates) before and after the implementation of a traffic measure are analyzed to evaluate the effect of the measure. Since accidents are count data, they are modelled using count regression methods, by default a Poisson model. However, the fit is tested for overdispersion and in case of significant overdispersion the model is automatically replaced by a Negative Binomial model. For flexibly making justice to the specific situation, six different scenarios for the effect of the measure are evaluated using different model formulations: no effect, trend, measure effect, trend effect, measure effect and trend, measure and trend effect. The most suitable mopdel is chosen via AIC, displayed in a plot and commented in printed output. Optionally, traffic exposure can be provided, resulting in the analysis of accident rates. The measure effect evaluates the difference directly before and directly after the measure.
#' An important assumption in the analysis is that the decision for traffic measures happens independently of the observed number of accidents. Accident numbers are random variables that fluctuate. If a measure is taken due to a randomly increased number of accidents, this leads to an overestimation of the effect of the measure in the analysis, since in such cases a decrease in the number of accidents can be expected even without a measure (regression-to-the-mean phenomenon). This is particularly problematic for site-specific measures with small accident numbers. Ideally, only observations from the period after the decision to implement a measure should be considered in the effectiveness analysis. In practice however, this often proves difficult because measures are implemented quickly and the remaining time series very short.
#'
#' @param accidents Either an R date/time or character vector with accident dates. For character vectors, only the following date formats are allowed: '2014-04-22', '2014/04/22', '22.4.2014'.
#' @param measure_start The date when the implementation of the measure started (e.g. character '22.4.2014' or R date/time).
#' @param measure_end The date when the implementation of the measure was terminated (respectively first day after the measure finished). If there is a period of acclimatization until road users have become accustomed to the new traffic regime, this can or should be integrated here.
#' @param exposition Optional data frame with exposition data. The first column is the time value, the second column the exposure. If the time value is a specific date (e.g. '22.4.2014'), this is considered as the start date of this exposure. If the time value is a year (format '2010') the exposure is taken for the whole year. Exposure values are extended until a new entry is available. If necessary, the first exposure value is extended backwards. DEFAULT NULL.
#' @param from From which date or year (1.1.) the time series should be considered. Optional. If not specified, the 1.1 from the year of the earliest accident is used.
#' @param until Until what date or year (31.12) the time series should be considered. Optional. If not specified, the 31.12 from the year of the latest accident is used.
#' @param main Optional title for the plot.
#' @param x_axis Optional, points at which tick-marks are to be drawn.
#' @param max_y Optional maximum value for the y-axis.
#' @param min_y Optional minimum value for the y-axis, defaults to 0.
#' @param orientation_x Alignment of the labels of the x-axis; "v" for vertical, "h" for horizontal, by default horizontal alignment is selected for 8 years or less.
#' @param add_exp Option to supplement the output plot with the exposure as an additional axis. Furthermore an additional plot of the exposure alone is produced. Only active if exposure is available.
#' @param KI_plot TRUE/FALSE, indicating if an additional illustration with the 95\% confidence interval for the measure effect is produced (only of limited use for models without measure effect).
#' @param lang Language for output ("en", "fr", "de" or "it"), defaults to "en".
#' @seealso \code{\link[STAAD:effectiveness_multiple]{effectiveness_multiple()}} for the joint analysis of a measure that was implemented at several locations.
#' @return A specific R object (\code{class_effectiveness}) is generated as function output. The main object is the plot with a graphical analysis of the measures' effect. The function \code{print.class_effectiveness()} extracts the most important key figures of the analysis.
#' Specifically, the output contains a list of the following elements:
#' \item{\code{fit}}{Output of the selected count regression model (Poisson or Negative Binomial family.}
#' \item{\code{modelname}}{Selected model scenario.}
#' \item{\code{data}}{Prepared data that were used for the analysis.}
#' \item{\code{pvalue_measure}}{p-value of the positive measure effect, if it exists.}
#' \item{\code{pvalue_interaction}}{p-value of the interaction term, if it exists.}
#' \item{\code{test_overdisp}}{p-value of the deviance dispersion test.}
#' \item{\code{plot}}{Plot graphical analysis (ggplot-class).}
#' \item{\code{plot_KI}}{Additional illustration with the 95\% confidence interval for the measure effect (ggplot-class).}
#' \item{\code{conf_limit}}{Overlapping of the confidence intervals before and after the measure.}
#' \item{\code{lang}}{Selected language.}
#' \item{\code{plot_exposition}}{Addional plot of the exposition, if available (ggplot-class).}
#' @export
#' @examples
#'   ex1 <- effectiveness(accidents = example_no_effect, measure_start = '1.1.2011', measure_end = '1.1.2011')
#'   print(ex1)
#'   plot(ex1)
#'   plot_ci(ex1)
#'   summary(ex1)
#'   ex2 <- effectiveness(accidents = example_measure_effect, measure_start = '1.1.2012', measure_end = '1.1.2012')
#'   ex2
#'   ex3 <- effectiveness(accidents = example_measure_and_trend_effect, measure_start = '2011-01-01', measure_end = '2011-1-1')
#'   plot(ex3)
#'   ex4 <- effectiveness(accidents = example_measure_effect_and_trend, measure_start = '2012/01/01', measure_end = '2012/1/1')
#'   ex4
#'   ex5 <- effectiveness(accidents = example_trend, measure_start = '1.1.2013', measure_end = '1.1.2013')
#'   print(ex5)
#'   ex6 <- effectiveness(accidents = example_trend_effect, measure_start = '1.1.2011', measure_end = '1.1.2011', lang = "fr")
#'   print(ex6)
#'   ex7 <- effectiveness(accidents = example_no_effect, measure_start = '1.1.2011', measure_end = '1.1.2011', exposition = exposition_ex1, lang = "de")
#'   summary(ex7)
#'   plot(ex7)
#'   ex8 <- effectiveness(accidents = example_measure_effect, measure_start = '1.1.2012', measure_end = '1.4.2012', exposition = exposition_ex2, add_exp = TRUE, lang = "it")
#'   plot(ex8)
#'   plot(ex8$plot_exposition)


effectiveness <- function(accidents, measure_start, measure_end, exposition = NULL, from = NULL,
                          until = NULL, main = NULL, x_axis = NULL, max_y = NULL, min_y = NULL,
                          orientation_x = NULL, add_exp = FALSE, KI_plot = TRUE,  lang = "en"){
  ## internal parameters
  silent = FALSE # silent: parameter to suppress error messages during model evaluation
  intervalle <- c(0.001, seq(0.01,0.99,0.01),0.991,0.992,0.993,0.994, 0.995, 0.996, 0.997, 0.998, 0.999, 0.9999) #Confidence intervals taken into account
  ## check mandatory input
  accidents <- try(as.Date(accidents, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")),
                   silent = silent)
  measure_start <- try(as.Date(measure_start, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")),
                       silent = silent)
  measure_end <- try(as.Date(measure_end, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")),
                     silent = silent)
  ## check optional input
  if (!is.null(exposition) & !is.null(dim(exposition))){
    if (dim(exposition)[2] >= 2){
      colnames(exposition)[1:2] <- c("time", "Exp")
      if (any(nchar(as.character(exposition$time))!=4)){
        exposition$time <- try(as.Date(exposition$time, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
      } else exposition$time <- try(as.numeric(exposition$time), silent = silent)
    }
  }
  if (!is.null(from)){
    if (nchar(as.character(from)) != 4){
      from <- try(as.Date(from, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")),
                  silent = silent)
    } else from <-  try(as.Date(paste0(from, "-01-01")), silent=silent)
  }
  if (!is.null(until)){
    if (nchar(as.character(until)) != 4){
      until <- try(as.Date(until, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")),
                   silent = silent)
    } else until <-  try(as.Date(paste0(until, "-12-31")), silent = silent)
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
  if (methods::is(accidents)[1] ==  "try-error")
    addError_sep(
      msg = "'accidents' not in the right format",
      argcheck = Check
    )
  #* measure_start format
  if (methods::is(measure_start)[1] ==  "try-error")
    addError_sep(
      msg = "wrong time format for 'measure_start'",
      argcheck = Check
    )
  #* measure_end format
  if (methods::is(measure_end)[1] ==  "try-error")
    addError_sep(
      msg = "wrong time format for 'measure_end'",
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
    if (methods::is(exposition)[1]==  "try-error")
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
  if (methods::is(from)[1] ==  "try-error")
    addError_sep(
      msg = "wrong time format for 'from'",
      argcheck = Check
    )
  #* until time format
  if (methods::is(until)[1] ==  "try-error")
    addError_sep(
      msg = "wrong time format for 'until",
      argcheck = Check
    )
  #* check until > from
  if (methods::is(from)[1]=="Date" & methods::is(until)[1]=="Date"){
    if (from > until)
      addError_sep(msg = "'until' has to be greater then 'from'", argcheck = Check)
  }
  #* check measure_end >= measure_start
  if (methods::is(measure_end)[1]=="Date" & methods::is(measure_start)[1]=="Date"){
    if (measure_start > measure_end)
      addError_sep(msg = "'measure_end' has to be greater or equal then 'measure_start'", argcheck = Check)
  }
  #* Return errors and warnings (if any)
  finishArgCheck_sep(Check)
  ## processing input
  measure_mean <- as.Date(mean(c(as.numeric(measure_start),as.numeric(measure_end))), origin="1970-01-01")
  if (is.null(from)) from <- as.Date(paste0(as.numeric( format(min(accidents), '%Y')), "-01-01"))
  if (is.null(until)) until <- as.Date(paste0(as.numeric( format(max(accidents), '%Y')), "-12-31"))
  if (from > until) stop("until has to be greater than from")
  if (from > measure_start) stop("measure_start has to be after from")
  if (until < measure_end) stop("measure_end has to be before until")
  before <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric( format(measure_start, '%Y'))),"-", format(measure_start, '%m-%d')))
  if (from > before[1]) before <- before[-1]
  after <- as.Date(paste0(as.numeric(format(measure_end, '%Y')):as.numeric( format(until+1, '%Y')),"-", format(measure_end, '%m-%d')))
  if ((until + 1) < after[length(after)]) after <- after[-length(after)]
  ## generate data for model
  dat_before <- as.data.frame(matrix(NA, nrow=length(before)-1, ncol=2))
  if (dim(dat_before)[1]==0) stop("timeserie before measure start not long enough")
  colnames(dat_before) <- c("Date", "accidents")
  dat_before$Date <- as.Date((as.numeric(before[1:(length(before)-1)])+as.numeric(before[2:length(before)]-1))/2, origin="1970-01-01")
  for (i in 1:(length(before)-1)){
    dat_before[i,2] <- sum(accidents >= before[i] & accidents < before[i+1])
  }
  dat_before$measure <- factor("before", levels=c("before", "after")) #"before" # indicator variable
  dat_after <- as.data.frame(matrix(NA, nrow=length(after)-1, ncol=2))
  if (dim(dat_after)[1]==0) stop("timeserie after measure end not long enough")
  colnames(dat_after) <- c("Date", "accidents")
  dat_after$Date <- as.Date((as.numeric(after[1:(length(after)-1)]) + as.numeric(after[2:length(after)]-1))/2, origin="1970-01-01")
  for (i in 1:(length(after)-1)){
    dat_after[i,2] <- sum(accidents >= after[i] & accidents < after[i+1])
  }
  dat_after$measure <- factor("after", levels=c("before", "after")) #  indicator variable
  ## add exposition to data
  if (!is.null(exposition)){
    tab <- data.frame(Date=seq(from, until, 1), Exp=exposition$Exp[1])
    if (methods::is(exposition$time)[1]=="Date"){
      index <- match(tab$Date, exposition$time)
      if(length(index)>2){
        for (i in 2:(length(index)-1)){
          tab$Exp[tab$Date>=exposition$time[i] & tab$Date<exposition$time[i+1]] <- exposition$Exp[i]
        }
      }
      tab$Exp[tab$Date>=exposition$time[length(index)]] <- exposition$Exp[length(index)]
    }
    if (is.numeric(exposition$time)){
      for (i in 1:length(exposition$time)){
        tab$Exp[as.numeric(format(tab$Date, '%Y')) >= exposition$time[i]] <- exposition$Exp[i]
      }
    }
    dat_before$Exp <- NA
    for (i in 1:(length(before)-1)){
      dat_before$Exp[i] <- mean(tab$Exp[tab$Date >= before[i] & tab$Date < before[i+1]])
    }
    dat_after$Exp <- NA
    for (i in 1:(length(after)-1)){
      dat_after$Exp[i] <- mean(tab$Exp[tab$Date >= after[i] & tab$Date < after[i+1]])
    }
  }
  dat_model <- rbind(dat_before, dat_after)
  during <- sum(accidents >= measure_start & accidents <= measure_end)
  measure_length <-  as.numeric(difftime(measure_end, measure_start, units="days")/365)
  ## check if time before and after measure
  if (sum(table(dat_model$measure)!=0)==1) stop("No data before or after the measure.")
  if (sum(dat_model$accident>0)< 2) stop("Less than 2 years with accident values. No stable estimation of the confidence intervals.")
  if (sum(dat_model$accident[dat_model$measure=="before"]>0) < 1) stop("No usable accidents before the measure.")
  if (is.null(exposition)){
    ## model measure and trend effect
    fit1 <- stats::glm(accidents~Date*measure, data=dat_model, family = "poisson")
    ## model trend effect
    fit2 <- stats::glm(accidents~Date+I(pmax(0,Date-as.numeric(measure_mean))), data=dat_model, family = "poisson")
    ## model measure effect and trend
    fit3 <- stats::glm(accidents~Date+measure, data=dat_model, family = "poisson")
    ## model measure effect
    fit4 <- stats::glm(accidents~measure, data=dat_model, family = "poisson")
    ## model trend
    fit5 <- stats::glm(accidents~Date, data=dat_model, family = "poisson")
    ## model no effect
    fit6 <- stats::glm(accidents~1, data=dat_model, family = "poisson")
    min_model <- try(max(which(c(fit1$aic, fit2$aic, fit3$aic, fit4$aic, fit5$aic, fit6$aic)==
                                 min(c(fit1$aic, fit2$aic, fit3$aic, fit4$aic, fit5$aic, fit6$aic), na.rm=TRUE))))
    if (methods::is(min_model)[1]=="try-error" | length(min_model)==0 | min_model == -Inf)
      stop("With this data none of the models can be calculated!")
    modelname <- c("measure and trend effect", "trend effect", "measure effect and trend",
                   "measure effect", "trend", "no effect")[min_model]
  }
  if (!is.null(exposition)){
    ## model measure and trend effect
    fit1 <- stats::glm(accidents~Date*measure+offset(log(Exp)), data=dat_model, family = "poisson")
    ## model trend effect
    fit2 <- stats::glm(accidents~Date+I(pmax(0,Date-as.numeric(measure_mean)))+offset(log(Exp)), data=dat_model, family = "poisson")
    ## model measure effect and trend
    fit3 <- stats::glm(accidents~Date+measure+offset(log(Exp)), data=dat_model, family = "poisson")
    ## model measure effect
    fit4 <- stats::glm(accidents~measure+offset(log(Exp)), data=dat_model, family = "poisson")
    ## model trend
    fit5 <- stats::glm(accidents~Date+offset(log(Exp)), data=dat_model, family = "poisson")
    ## model no effect
    fit6 <- stats::glm(accidents~1+offset(log(Exp)), data=dat_model, family = "poisson")
    min_model <- try(max(which(c(fit1$aic, fit2$aic, fit3$aic, fit4$aic, fit5$aic, fit6$aic)==
                                 min(c(fit1$aic, fit2$aic, fit3$aic, fit4$aic, fit5$aic, fit6$aic), na.rm=TRUE))))
    if (methods::is(min_model)[1]=="try-error" | length(min_model)==0 | min_model == -Inf)
      stop("With this data none of the models can be calculated!")
    modelname <- c("measure and trend effect", "trend effect", "measure effect and trend",
                   "measure effect", "trend", "no effect")[min_model]
  }
  fit <- get(paste("fit", min_model, sep=""))
  #test_overdisp <- 1-stats::pchisq(stats::deviance(fit), stats::df.residual(fit))
  test_overdisp <- stats::pchisq(sum(stats::residuals(fit, type="pearson")^2), df=fit$df.residual, lower.tail=FALSE)
  if (test_overdisp<=0.05){
    fit_start <- stats::glm(fit$formula, data=dat_model, family = "quasipoisson",
                     control = stats::glm.control(epsilon = 1e-8, maxit = 200, trace = FALSE))
    fit_nb <- try(MASS::glm.nb(fit$formula, data=dat_model, start=fit_start$coefficients,
                         control = stats::glm.control(epsilon = 1e-6, maxit = 200, trace = FALSE)), silent=TRUE)
    if (attr(fit_nb, "class")[1] != "try-error") {
      fit <- fit_nb
    } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated")
  }
  ## p_value for the measure effect only with negative measure effect, 1-sided test-> p-value/2
  pvalue_measure <- NA
  if("measureafter" %in% rownames(summary(fit)$coefficients) & min_model!=1){
    pvalue_measure <- ifelse(stats::coef(summary(fit))["measureafter", 1] < 0, stats::coef(summary(fit))["measureafter", 4]/2,NA)
  }
  ## p_value for interaction only if the interaction term is negative
  pvalue_interaction <- NA
  if("Date:measureafter" %in% rownames(summary(fit)$coefficients)){
    pvalue_interaction <- ifelse(stats::coef(summary(fit))["Date:measureafter", 1]<0, stats::coef(summary(fit))["Date:measureafter", 4]/2,NA)
  }
  ## For the model with interaction, an estimation for the p-value for the measure is made over the confidence intervals.
  ## (Only if interaction term negative)
  conf_limit <- NA
  if(min_model==1){
    pvalue_measure <- NA
    bef <- data.frame(measure=factor("before", levels=c("before", "after")), Date=measure_mean)
    aft <- data.frame(measure=factor("after", levels=c("before", "after")), Date=measure_mean)
    if (!is.null(exposition)){
      bef$Exp <- dat_before$Exp[length(dat_before$Exp)]
      aft$Exp <- dat_after$Exp[1]
    }
    preds_bef <- try(stats::predict(fit, type="link", newdata = bef, se.fit = TRUE), silent=silent)
    preds_aft <- try(stats::predict(fit, type="link", newdata = aft, se.fit = TRUE), silent=silent)
    if (exp(preds_aft$fit) < exp(preds_bef$fit)){
      if (methods::is(preds_bef)[1]!="try-error"){
        i_v <- exp(preds_bef$fit-1*stats::qnorm(intervalle)*preds_bef$se.fit)
        }
      if (methods::is(preds_aft)[1]!="try-error"){
        i_n <- exp(preds_aft$fit+stats::qnorm(intervalle)*preds_aft$se.fit)
      }
      conf_limit <- intervalle[which(i_v-i_n<0)[1]-1]*100
      if(length(conf_limit) == 0) conf_limit <- "<0.1"
      if(is.na(conf_limit)) conf_limit <- "> 99.99"
      if (is.na(pvalue_interaction) & lang %in% c("en", "fr", "it")) pvalue_measure <- paste("No overlap of the ", conf_limit, "%-confidence intervals.", sep="")
      if (is.na(pvalue_interaction) & lang == "de") pvalue_measure <- paste("Keine Ueberlappung der ", conf_limit, "%-Konfidenzintervalle.", sep="")
      if (is.na(pvalue_interaction) & lang == "en") pvalue_measure <- paste(pvalue_measure, " !Attention the trend increases more after the measure than before!", sep="")
      if (is.na(pvalue_interaction) & lang == "fr") pvalue_measure <- paste(pvalue_measure, " !Attention la tendance augemente plus fortement apres la mesure qu'auparavant!", sep="")
      if (is.na(pvalue_interaction) & lang == "it") pvalue_measure <- paste(pvalue_measure, " !Attenzione - la tendenza aumenta piu fortemente dopo la misura rispetto a prima!", sep="")
      if (is.na(pvalue_interaction) & lang == "de") pvalue_measure <- paste(pvalue_measure, " !Achtung der Trend steigt nach der Massnahme staerker als davor!", sep="")
    }
  }
  pvalue_trend <- NA
  if("Date" %in% rownames(summary(fit)$coefficients)){
    pvalue_trend <- stats::coef(summary(fit))["Date", 4]
  }
  ## Pearson residuals
  dat_fit <- data.frame(pearson=stats::residuals.glm(fit, type="pearson")>=2,
                        row.names=attr(fit$qr$qr, "dimnames")[[1]])
  ## expected value
  dat_fit$expect <- stats::predict(fit, type="response")
  ## calculation of confidence intervals
  if (is.null(exposition)){
    before_bor <- data.frame(Date=before, measure=factor("before", levels=c("before", "after")))
    after_bor <- data.frame(Date=after, measure=factor("after", levels=c("before", "after")))
    expect_before <- rbind(data.frame(Date=before[1], accidents=NA, measure=factor("before", levels=c("before", "after"))), dat_before,
                           data.frame(Date=measure_start, accidents=NA, measure=factor("before", levels=c("before", "after"))))
    expect_after <- rbind(data.frame(Date=measure_end, accidents=NA, measure=factor("after", levels=c("before", "after"))), dat_after,
                          data.frame(Date=after[length(after)], accidents=NA, measure=factor("after", levels=c("before", "after"))))
  }
  if (!is.null(exposition)){
    before_bor <- data.frame(Date=before, measure=factor("before", levels=c("before", "after")),
                             Exp=stats::approx(x=dat_model$Date, y=dat_model$Exp, xout=before, rule=2 )$y)
    after_bor <- data.frame(Date=after, measure=factor("after", levels=c("before", "after")),
                            Exp=stats::approx(x=dat_model$Date, y=dat_model$Exp, xout=after, rule=2 )$y)
    expect_before <- rbind(data.frame(Date=before[1], accidents=NA, measure=factor("before", levels=c("before", "after")),
                                      Exp=stats::approx(x=dat_model$Date, y=dat_model$Exp, xout=before[1], rule=2)$y),
                           dat_before, data.frame(Date=measure_start, accidents=NA,
                                                  measure=factor("before", levels=c("before", "after")),
                                                  Exp=stats::approx(x=dat_model$Date, y=dat_model$Exp, xout=measure_start, rule=2)$y))
    expect_after <- rbind(data.frame(Date=measure_end, accidents=NA, measure=factor("after", levels=c("before", "after")),
                                     Exp=stats::approx(x=dat_model$Date, y=dat_model$Exp, xout=measure_end, rule=2)$y), dat_after,
                          data.frame(Date=after[length(after)], accidents=NA, measure=factor("after", levels=c("before", "after")),
                                     Exp=stats::approx(x=dat_model$Date, y=dat_model$Exp, xout=after[length(after)], rule=2)$y))
  }
  ## Combine original data with the calculated data
  dat_model$rownames <- rownames(dat_model)
  dat_fit$rownames <- rownames(dat_fit)
  dat_total <- merge(x=dat_model, y=dat_fit, by="rownames", all=TRUE)
  dat_total <- dat_total[order(dat_total$Date),]
  preds_exp_bef <- try(stats::predict(fit, type= "link", newdata= expect_before, se.fit = TRUE))
  preds_exp_aft <- try(stats::predict(fit, type= "link", newdata= expect_after, se.fit = TRUE))
  preds_before_bor <- try(stats::predict(fit, type= "link", newdata= before_bor, se.fit = TRUE))
  preds_after_bor <- try(stats::predict(fit, type= "link", newdata= after_bor, se.fit = TRUE))
  expect_before$expect <- exp(preds_exp_bef$fit)
  expect_after$expect <-  exp(preds_exp_aft$fit)
  before_bor$expect <- exp(preds_before_bor$fit)
  after_bor$expect <- exp(preds_after_bor$fit)
  expect_before$low <- exp(preds_exp_bef$fit-stats::qnorm(0.975)*preds_exp_bef$se.fit)
  expect_before$upp <- exp(preds_exp_bef$fit+stats::qnorm(0.975)*preds_exp_bef$se.fit)
  expect_after$low <- exp(preds_exp_aft$fit-stats::qnorm(0.975)*preds_exp_aft$se.fit)
  expect_after$upp <- exp(preds_exp_aft$fit+stats::qnorm(0.975)*preds_exp_aft$se.fit)
  before_bor$low <- exp(preds_before_bor$fit-stats::qnorm(0.975)*preds_before_bor$se.fit)
  before_bor$upp <- exp(preds_before_bor$fit+stats::qnorm(0.975)*preds_before_bor$se.fit)
  after_bor$low <- exp(preds_after_bor$fit-stats::qnorm(0.975)*preds_after_bor$se.fit)
  after_bor$upp <- exp(preds_after_bor$fit+stats::qnorm(0.975)*preds_after_bor$se.fit)
  ## visualization
  if (is.null(main)) main <- paste("measure", paste(measure_mean, collapse = ","), sep=" ")
  if (is.null(x_axis)) x_axis <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric(format(until, '%Y'))+1), "-01-01"))
  year_format <- ifelse(measure_start>100, "%Y", "%y")
  if(sum(after_bor$upp[after_bor$measure=="after"] != Inf) == 0){
    faelle <- sum(after_bor$measure=="after")
    q_Jahr <- 0.05^{1/faelle}
    lambda_est <- -log(q_Jahr)
    #after_bor$upp[after_bor$measure=="after"] <- lambda_est #0.5*qchisq(1-q_Jahr, df=2*lambda_est)
    after_bor$upp <- lambda_est
    expect_after$upp <- lambda_est
    bef <- data.frame(measure=factor("before", levels=c("before", "after")), Date=measure_mean)
    if (!is.null(exposition)){
      bef$Exp <- dat_before$Exp[length(dat_before$Exp)]
    }
    preds_bef <- try(stats::predict(fit, type="link", newdata = bef, se.fit = TRUE), silent=silent)
    if (methods::is(preds_bef)[1]!="try-error"){
      i_v <- exp(preds_bef$fit-1*stats::qnorm(intervalle)*preds_bef$se.fit)
    }
    q_Jahr_int <- (1-intervalle)^{1/faelle}
    i_n <- -log(q_Jahr_int)
    conf_limit <- intervalle[which(i_v-i_n<0)[1]-1]*100
    if(is.na(conf_limit)) conf_limit <- "> 99.99"
    if (is.na(pvalue_interaction) & lang %in% c("en", "fr", "it")) pvalue_measure <- paste("No overlap of the ", conf_limit, "%-confidence intervals.", sep="")
    if (is.na(pvalue_interaction) & lang == "de") pvalue_measure <- paste("Keine Ueberlappung der ", conf_limit, "%-Konfidenzintervalle.", sep="")
  }
  # Base plot
  if (is.null(orientation_x)) orientation_x <- ifelse(diff(as.numeric(format(range(dat_total$Date), '%Y'))) > 8, "v", "h")
  if (is.null(min_y)) min_y <- 0
  if (is.null(exposition)){
    if (is.null(max_y)){
      max_data <- c(dat_total$accidents, dat_total$upp, before_bor$upp, after_bor$upp, expect_after$upp, expect_before$upp)
      max_y <- max(max_data[is.finite(max_data)], na.rm=TRUE)*1.1
    }
    p <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=accidents)) +
      ggplot2::geom_vline(xintercept=before_bor$Date, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=after_bor$Date, colour="darkgrey", linetype=2)+
      ggplot2::geom_ribbon(data=rbind(expect_before, expect_after), ggplot2::aes(ymin=low,ymax=upp), fill="grey", alpha=0.5)+
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format(year_format))+
      ggplot2::ggtitle(main)+
      ggplot2::theme_bw()
    before_bor$accidents=NA
    after_bor$accidents=NA
    before_bor$accidents=as.numeric(before_bor$accidents)
    after_bor$accidents=as.numeric(after_bor$accidents)
    for (z in seq(1, dim(before_bor)[1], 2)){
      p <- p +
        ggplot2::geom_ribbon(data=before_bor[z:(z+1),], ggplot2::aes(ymin=low,ymax=upp), fill="darkgrey", alpha=0.5)
    }
    for (z in seq(1, dim(after_bor)[1], 2)){
      p <- p +
        ggplot2::geom_ribbon(data=after_bor[z:(z+1),], ggplot2::aes(ymin=low,ymax=upp), fill="darkgrey", alpha=0.5)
    }
    p <- p +
      ggplot2::geom_point() +
      ggplot2::geom_line(data=expect_before, ggplot2::aes(y=expect, x=Date), col="blue", linetype = 1)+
      ggplot2::geom_line(data=expect_after, ggplot2::aes(y=expect, x=Date), col="blue", linetype = 1)+
      ggplot2::geom_segment(ggplot2::aes(x=expect_before$Date[dim(expect_before)[1]], xend=expect_after$Date[1],
                       y=expect_before$expect[dim(expect_before)[1]],
                       yend=expect_after$expect[1]), col="blue", linetype  = 2)+
      ggplot2::geom_line(data=dat_total[dat_total$Date<measure_mean,], ggplot2::aes(x=Date, y=accidents)) +
      ggplot2::geom_vline(xintercept=measure_end, colour="red")+
      ggplot2::geom_vline(xintercept=measure_start, colour="red")+
      ggplot2::geom_point(x=mean(c(measure_end, measure_start)), y=during/measure_length, colour="grey", na.rm=TRUE) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                  expand = c(0, 0), limits = c(min_y, max_y))
    ## Connect the measured values after the measure, if there are enough measured values
    if (sum(dat_total$Date>measure_mean)>1){
      p <- p +
        ggplot2::geom_line(data=dat_total[dat_total$Date>measure_mean,], ggplot2::aes(x=Date, y=accidents))
    }
    if (orientation_x == "v")  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    if (lang == "de") p <- p + ggplot2::ylab("Unfaelle")
    if (lang == "it") p <- p + ggplot2::ylab("Incidenti")
    if (lang == "en") p <- p + ggplot2::ylab("Accidents")
    if (lang == "fr") p <- p + ggplot2::ylab("Accidents")
  }
  if (!is.null(exposition)){
    if (!is.null(max_y)){
      scal <- 10^(floor(log10(ceiling(1/max_y))) + 1)
    }
    if (is.null(max_y)){
      max_y <- max(dat_total$expect/dat_total$Exp, dat_total$accidents/dat_total$Exp,
                                     before_bor$upp/before_bor$Exp[before_bor$upp!=Inf],
                                     after_bor$upp/after_bor$Exp[after_bor$upp!=Inf], na.rm=TRUE)*1.1
      scal <- 10^(floor(log10(ceiling(1/max_y))) + 1)
    }
    p <- ggplot2::ggplot(dat_total, ggplot2::aes(x=Date, y=accidents/Exp* scal)) +
      ggplot2::geom_vline(xintercept=before_bor$Date, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=after_bor$Date, colour="darkgrey", linetype=2) +
      ggplot2::geom_ribbon(data=rbind(expect_before, expect_after),
                           ggplot2::aes(ymin=low/Exp* scal,ymax=upp/Exp* scal), fill="grey", alpha=0.5) +
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format(year_format)) +
      ggplot2::ylab(paste("accident rate *", formatC(scal, format = "e", digits = 0))) +
      ggplot2::ggtitle(main) +
      ggplot2::theme_bw()
    before_bor$accidents=NA
    after_bor$accidents=NA
    for (z in seq(1, dim(before_bor)[1], 2)){
      p <- p +
        ggplot2::geom_ribbon(data=before_bor[z:(z+1),], ggplot2::aes(ymin=low/Exp* scal,ymax=upp/Exp* scal),
                             fill="darkgrey", alpha=0.5)
    }
    for (z in seq(1, dim(after_bor)[1], 2)){
      p <- p +
        ggplot2::geom_ribbon(data=after_bor[z:(z+1),], ggplot2::aes(ymin=low/Exp* scal,ymax=upp/Exp* scal),
                             fill="darkgrey", alpha=0.5)
    }
    p <- p +
      ggplot2::geom_point() +
      ggplot2::geom_line(data=expect_before, ggplot2::aes(y=expect/Exp* scal, x=Date), col="blue", linetype = 1)+
      ggplot2::geom_line(data=expect_after, ggplot2::aes(y=expect/Exp* scal, x=Date), col="blue", linetype = 1)+
      ggplot2::geom_segment(ggplot2::aes(x=expect_before$Date[dim(expect_before)[1]], xend=expect_after$Date[1],
                       y=expect_before$expect[dim(expect_before)[1]]/expect_before$Exp[dim(expect_before)[1]]* scal,
                       yend=expect_after$expect[1]/expect_after$Exp[1]* scal), col="blue", linetype  = 2)+
      ggplot2::geom_line(data=dat_total[dat_total$Date<measure_mean,], ggplot2::aes(x=Date, y=accidents/Exp* scal)) +
      ggplot2::geom_vline(xintercept=measure_end, colour="red")+
      ggplot2::geom_vline(xintercept=measure_start, colour="red")+
      ggplot2::geom_point(x=mean(c(measure_end, measure_start)), y=during/measure_length/tab$Exp[round(tab$Date)==round(measure_mean)]*scal, colour="grey", na.rm=TRUE)
    ## Connect the measured values after the measure, if there are enough measured values
    if (sum(dat_total$Date>measure_mean)>1){
      p <- p +
        ggplot2::geom_line(data=dat_total[dat_total$Date>measure_mean,], ggplot2::aes(x=Date, y=accidents/Exp* scal))
    }
    if (!add_exp){
      p <- p + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                           expand = c(0, 0), limits = c(min_y *scal, max_y * scal))
    }
    if (orientation_x == "v")  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    if (lang == "en") p <- p + ggplot2::ylab(paste("Accident rate *", formatC(scal, format = "e", digits = 0)))
    if (lang == "de") p <- p + ggplot2::ylab(paste("Unfallrate *", formatC(scal, format = "e", digits = 0)))
    if (lang == "fr") p <- p + ggplot2::ylab(paste("Taux d'accidents *", formatC(scal, format = "e", digits = 0)))
    if (lang == "it") p <- p + ggplot2::ylab(paste("Tasso di incidenti *", formatC(scal, format = "e", digits = 0)))
    if (add_exp){
      scal_acci <- ceiling(max(dat_total$Exp)/(max_y*scal)) * 1.05
      p <- p+ggplot2::geom_point(data=dat_total, ggplot2::aes(y = Exp/scal_acci), colour = "darkgrey")+
        ggplot2::geom_line(data=dat_total, ggplot2::aes(y = Exp/scal_acci), colour = "darkgrey")
      if (lang %in% c("fr", "en")) p <- p + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                                             expand = c(0, 0), limits = c(min_y *scal, max_y * scal),
                                                             sec.axis=ggplot2::sec_axis(~.*scal_acci * 1.1,
                                                                                        name="exposition"))
      if (lang == "de") p <- p + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                                             expand = c(0, 0), limits = c(min_y *scal, max_y * scal),
                                                             sec.axis=ggplot2::sec_axis(~.*scal_acci * 1.1,
                                                                                        name="Exposition"))
      if (lang == "it") p <- p + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                                             expand = c(0, 0), limits = c(min_y *scal, max_y * scal),
                                                             sec.axis=ggplot2::sec_axis(~.*scal_acci * 1.1,
                                                                                        name="esposizione"))
      p3 <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=Exp)) +
        ggplot2::geom_vline(xintercept=before_bor$Date, colour="darkgrey", linetype=2) +
        ggplot2::geom_vline(xintercept=after_bor$Date, colour="darkgrey", linetype=2) +
        ggplot2::geom_line() +
        ggplot2::geom_point()  +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                    expand = c(0, 0), limits = c(0, max(dat_total$Exp, na.rm = TRUE)*1.1))+
        ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
        ggplot2::ggtitle(main) +
        ggplot2::ylab("exposition") +
        ggplot2::theme_bw()
      if (orientation_x == "v")  p3 <- p3 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
      if (lang == "de") p3 <- p3 + ggplot2::ylab("Exposition")
      if (lang == "it") p3 <- p3 + ggplot2::ylab("esposizione")
    }
  }
  if (lang == "de") p <- p + ggplot2::xlab("Datum")
  if (lang == "it") p <- p + ggplot2::xlab("Data")

  ## Additional optional plot with 95% confidence interval for measure effectiveness
  if (KI_plot){
    d <- expect_before[dim(expect_before)[1], "expect"]-expect_after[1, "expect"]
    borders <- c(expect_before[dim(expect_before)[1], "upp"]-expect_after[1, "low"],
                 expect_before[dim(expect_before)[1], "low"]-expect_after[1, "upp"])
    dat_KI <- data.frame(d, borders)
    p2 <- ggplot2::ggplot(dat_KI, ggplot2::aes(x=1, y=borders)) +
      ggplot2::geom_line(size=2.5)+
      ggplot2::coord_flip()+
      ggplot2::ggtitle(paste("95% confidence interval", main))+
      ggplot2::ylab("")+
      ggplot2::xlab("")+
      ggplot2::xlim(c(0.9,1.1))+
      ggplot2::ylim(c(-1,1)*c(max(abs(borders))*1.5))+
      ggplot2::geom_segment(ggplot2::aes(x = 0.9, y = 0, xend = 1.1, yend = 0), col="grey", size=1.5)+
      ggplot2::geom_segment(ggplot2::aes(x = 0.99, y = d, xend = 1.01, yend = d), colour = "red", size=2)+
      ggplot2::theme(axis.ticks.y=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_blank())
  }
  if (!KI_plot) p2 = NULL
  if (!add_exp | is.null(exposition)) output <- list(fit = fit, modelname=modelname, data = rbind(expect_before, expect_after),
                                                     pvalue_measure= pvalue_measure, pvalue_trend=pvalue_trend,
                                                     pvalue_interaction= pvalue_interaction, test_overdisp = test_overdisp,
                                                     plot=p, plot_KI = p2, conf_limit = conf_limit, lang = lang)
  if (add_exp & !is.null(exposition)) output <- list(fit = fit, modelname=modelname, data = rbind(expect_before, expect_after),
                                                     pvalue_measure= pvalue_measure, pvalue_trend=pvalue_trend,
                                                     pvalue_interaction= pvalue_interaction, test_overdisp = test_overdisp,
                                                     plot=p, plot_KI = p2, conf_limit = conf_limit, lang = lang, plot_exposition = p3)


  class(output) <- "class_effectiveness"
  return(output)
}


#' @method print class_effectiveness
#' @export

"print.class_effectiveness" <- function(object, plot = TRUE)
{
  if (!inherits(object, "class_effectiveness"))
  {
    stop("Not a effectiveness object")
  }
  modelname <- c("measure and trend effect", "trend effect", "measure effect and trend",
                 "measure effect", "trend", "no effect")
  reliability <- c("not reliable, no effect proven", "highly reliable", "well reliable", "weakly reliable")
  measure <- "Effect of measures"
  min_model <- which(object$modelname==modelname)
  if (object$lang == "en"){
    model <- "model"
    nb <- "Negative binomial"
    pm <- "Poisson"
    pv <- "p-value"
    od <- "overdispersion"
  }
  if (object$lang == "de"){
    modelname <- c("Massnahmen- und Trendeffekt", "Trendeffekt", "Massnahmeneffekt und Trend",
                   "Massnahmeneffekt", "Trend", "kein Effekt")
    reliability <- c("nicht verlaesslich, keine Wirkung nachgewiesen", "stark verlaesslich", "gut verlaesslich", "schwach verlaesslich")
    measure <- "Massnahmeneffekt"
    model <- "Modell"
    nb <- "Negative Binomial"
    pm <- "Poisson"
    pv <- "p-Wert"
    od <- "Overdispersion"
  }
  if (object$lang == "fr"){
    modelname <- c("Effet des mesures et de la tendance", "Effet de tendance", "Effet des mesures et tendance",
                   "Effet des mesures", "Tendance", "aucun effet")
    reliability <- c("pas fiable, pas d'effet prouve", "tres fiable", "assez fiable", "faiblement fiable")
    measure <- "Effet des mesures"
    model <- "model"
    nb <- "binomiale negative"
    pm <- "poisson"
    pv <- "valeur p"
    od <- "surdispersion"
  }
  if (object$lang == "it"){
    modelname <- c("Misure ed effetto tendenza", "Effetto tendenza",
                   "Effetto delle misure e della tendenza",
                   "Effetto delle misure", "Tendenza", "nessun effett")
    reliability <- c("non affidabile, nessun effett provato", "altamente affidabile", "altamente affidabile",
                     "debolmente affidabile")
    measure <- "Effetto delle misure"
    model <- "modello"
    nb <- "binomiale negativa"
    pm <- "poisson"
    pv <- "valore p"
    od <- "overdispersion"
  }
  if (is.na(object$conf_limit)){
    if(is.na(object$pvalue_measure))
    {
      k <- 1
    } else if(object$pvalue_measure <= 0.01 && object$pvalue_measure >= 0)
    {
      k<- 2
    } else if (object$pvalue_measure > 0.01 && object$pvalue_measure <= 0.05)
    {
      k <- 3
    } else if (object$pvalue_measure > 0.05 && object$pvalue_measure <= 0.10)
    {
      k <- 4
    } else k <- 1
  }
  if (!is.na(object$conf_limit)){
    if(object$conf_limit >= 99 | object$conf_limit == "> 99.99")
    {
      k<- 2
    } else if (object$conf_limit < 99 && object$conf_limit >= 95)
    {
      k <- 3
    } else if (object$conf_limit < 95 && object$conf_limit >= 90)
    {
      k <- 4
    } else k <- 1
  }
  if (plot) print(object$plot)
  cat(modelname[min_model])
  if (!is.null(object$fit$theta))
  {
    cat("\n", paste(nb, model, "(Theta =", round(object$fit$theta, 2), ")"))
  }
  if (is.null(object$fit$theta))
  {
    cat("\n", paste(pm, model))
  }
  cat("\n", paste(pv, od, round(object$test_overdisp, 3)))
  if (!is.na(object$conf_limit)) cat("\n", paste0(measure,": ", reliability[k], " (",object$pvalue_measure, ")"))
  if (is.na(object$conf_limit)) cat("\n", paste0(measure,": ", reliability[k]))
  cat("\n")
}


#' @method plot class_effectiveness
#' @export

"plot.class_effectiveness" <- function(object)
{
  if (!inherits(object, "class_effectiveness"))
  {
    stop("Not a effectiveness object")
  }
  print(object$plot)
}


#' @method summary class_effectiveness
#' @export


"summary.class_effectiveness" <- function(object)
{
  if (!inherits(object, "class_effectiveness"))
  {
    stop("Not a effectiveness object")
  }
  sum_output <- list(model = object$fit, modelname=object$modelname, data = object$data,
                     p_value_measure = object$pvalue_measure, p_value_trend = object$pvalue_trend,
                     p_value_interaction = object$pvalue_interaction, p_value_test_overdispersion = object$test_overdisp)
  sum_output
}
