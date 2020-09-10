#' Function to evaluate the effectiveness of measures
#'
#' @details
#' Traffic accidents before and after the implementation of a traffic measure are analyzed to evaluate the effect of the #' measure. Since accident are counting data, they are modeled using count regression, by default with a Poisson model. #' However, the model is tested for overdispersion and in case of significant overdispersion it is automatically switched #' to a Negative binomial model. For the situation analysis, six different model scenarios for the measure are evaluated: #' no effect, trend, effect of measures, measure effect and trend, trend effect, measures and trend effect. The best
#' model is displayed. The exposure can optionally be considered as an offset.
#'
#' @param accidents vector with accident data. The following data formats are allowed '2014-04-22', '2014/04/22' respectively '22.4.2014'
#' @param measure_start Start of the implementation of measures (format '2014-04-22', '2014/04/22' respectively '22.4.2014')
#' @param measure_end End of measure implementation respectively first day after the measure (format '2014-04-22', '2014/04/22' respectively '22.4.2014')
#' @param exposition optinal Dataframe with expositions data. The first column is the time value, the second column the exposure. If the time value is a specific date (e.g. '22.4.2014'), this is considered as the start date of this exposure. If the time value is a year (format '2010') the exposure is taken for the whole year. Exposure values are extended until a new entry is available. If necessary, the first exposure value is extended back forwards. DEFAULT NULL
#' @param from From which date or year (1.1) the time series should be considered. Optional. If not specified, the 1.1 from the year of the earliest accident is used.
#' @param until Until when date or year (31.12) the time series should be considered. Optional. If not specified, the 31.12 from the year of the latest accident is used.
#' @param main optional title for the plot
#' @param x_axis optional vector with the values for the x-axis
#' @param y_axis optional vector with the values for the y-axis
#' @param max_y optional maximum value for the y-axis
#' @param KI_plot TRUE/FALSE if an additional illustration with the 95\% confidence interval for the measure effect should be produced (only of limited use for models without measure effect)
#' @param silent parameter to suppress error messages during model evaluation
#' @param lang language for output ("en", "fr", "de" or "it")
#' @export plot.class_effectiveness
#' @export print.class_effectiveness
#' @export summary.class_effectiveness
#' @export
#' @examples
#'   ex1 <- effectiveness(accidents = example_no_effect, measure_start = '1.1.2011', measure_end = '1.1.2011')
#'   print(ex1)
#'   plot(ex1)
#'   plot_ci(ex1)
#'   summary(ex1)
#'   ex2 <- effectiveness(accidents = example_measure_effect, measure_start = '1.1.2011', measure_end = '1.1.2011')
#'   ex3 <- effectiveness(accidents = example_measure_effect_and_trend , measure_start = '1.1.2011', measure_end = '1.1.2011')
#'   ex4 <- effectiveness(accidents = example_no_effect, measure_start = '1.1.2011', measure_end = '1.1.2011', exposition = exposition_ex1)
#'
effectiveness <- function(accidents, measure_start, measure_end, exposition = NULL, from = NULL, until = NULL, main = NULL, x_axis = NULL, y_axis = NULL, max_y = NULL, KI_plot = TRUE, silent = TRUE, lang = "en"){
  ## check mandatory input
  accidents <- try(as.Date(accidents, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
  measure_start <- try(as.Date(measure_start, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
  measure_end <- try(as.Date(measure_end, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
  ## check optional input
  if (!is.null(exposition)){
    if (dim(exposition)[2] >= 2){
      colnames(exposition)[1:2] <- c("time", "DTV")
      if (any(nchar(as.character(exposition$time))!=4)){
        exposition$time <- try(as.Date(exposition$time, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
      } else expostion$time <- try(as.numeric(exposition$time), silent = silent)
    }
  }
  if (!is.null(from)){
    if (nchar(as.character(from)) != 4){
      from <- try(as.Date(from, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
    } else from <-  try(as.Date(paste0(from, "-01-01")), silent=silent)
  }
  if (!is.null(until)){
    if (nchar(as.character(until)) != 4){
      until <- try(as.Date(until, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
    } else until <-  try(as.Date(paste0(until, "-12-31")), silent = silent)
  }
  if (!(lang %in% c("en", "de", "it", "fr"))){
    lang <- "en"
    warning("language unknown, set to english")
  }

  Check <- ArgumentCheck::newArgCheck()
  #* accidents format
  if (is(accidents)[1] ==  "try-error")
    ArgumentCheck::addError(
      msg = "'accidents' not in the right format",
      argcheck = Check
    )
  #* measure_start format
  if (is(measure_start)[1] ==  "try-error")
    ArgumentCheck::addError(
      msg = "'wrong time format for 'measure_start'",
      argcheck = Check
    )
  #* measure_end format
  if (is(measure_end)[1] ==  "try-error")
    ArgumentCheck::addError(
      msg = "'wrong time format for 'measure_end'",
      argcheck = Check
    )
  #* exposition format
  if (!is.null(exposition)){
    if (dim(exposition)[2] < 2)
      ArgumentCheck::addError(
        msg = "'wrong input for 'exposition'",
        argcheck = Check
      )
    #* exposition time format
    if (is(exposition)[1] ==  "try-error")
      ArgumentCheck::addError(
        msg = "'wrong time format for 'exposition'",
        argcheck = Check
      )
  }
  #* from time format
  if (is(from)[1] ==  "try-error")
    ArgumentCheck::addError(
      msg = "'wrong time format for 'from'",
      argcheck = Check
    )
  #* until time format
  if (is(until)[1] ==  "try-error")
    ArgumentCheck::addError(
      msg = "'wrong time format for 'until'",
      argcheck = Check
    )

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)

  ## processing input
  measure_mean <- as.Date(mean(c(as.numeric(measure_start),as.numeric(measure_end))), origin="1970-01-01")
  if (is.null(from)) from <- as.Date(paste0(as.numeric( format(min(accidents), '%Y')), "-01-01"))
  if (is.null(until)) until <- as.Date(paste0(as.numeric( format(max(accidents), '%Y')), "-12-31"))
  before <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric( format(measure_start, '%Y'))),"-", format(measure_start, '%m-%d')))
  if (from>before[1]) before <- before[-1]
  after <- as.Date(paste0(as.numeric(format(measure_end, '%Y')):as.numeric( format(until+1, '%Y')),"-", format(measure_end, '%m-%d')))
  if (until+1<after[length(after)]) after <- after[-length(after)]
  ## generate data for model
  dat_before <- as.data.frame(matrix(NA, nrow=length(before)-1, ncol=2))
  colnames(dat_before) <- c("Date", "accidents")
  dat_before$Date <- as.Date((as.numeric(before[1:(length(before)-1)])+as.numeric(before[2:length(before)]-1))/2, origin="1970-01-01")
  for (i in 1:(length(before)-1)){
    dat_before[i,2] <- sum(accidents >= before[i] & accidents < before[i+1])
  }
  dat_before$measure <- factor("before", levels=c("before", "after")) #"before" # indicator variable
  dat_after <- as.data.frame(matrix(NA, nrow=length(after)-1, ncol=2))
  colnames(dat_after) <- c("Date", "accidents")
  dat_after$Date <- as.Date((as.numeric(after[1:(length(after)-1)]) + as.numeric(after[2:length(after)]-1))/2, origin="1970-01-01")
  for (i in 1:(length(after)-1)){
    dat_after[i,2] <- sum(accidents >= after[i] & accidents < after[i+1])
  }
  dat_after$measure <- factor("after", levels=c("before", "after")) #  indicator variable

  ## add exposition to data
  if (!is.null(exposition)){
    tab <- data.frame(Date=seq(from, until, 1), DTV=exposition$DTV[1])
    if (is(exposition$time)[1]=="Date"){
      index <- match(tab$Date, exposition$time)
      if(length(index)>2){
        for (i in 2:(length(index)-1)){
          tab$DTV[tab$Date>=exposition$time[i] & tab$Date<exposition$time[i+1]] <- exposition$DTV[i]
        }
      }
      tab$DTV[tab$Date>=exposition$time[length(index)]] <- exposition$DTV[length(index)]
    }
    if (is.numeric(exposition$time)){
      for (i in 1:length(exposition$time)){
        tab$DTV[as.numeric(format(tab$Date, '%Y')) >= exposition$time[i]] <- exposition$DTV[i]
      }
    }
    dat_before$DTV <- NA
    for (i in 1:(length(before)-1)){
      dat_before$DTV[i] <- mean(tab$DTV[tab$Date >= before[i] & tab$Date < before[i+1]])
    }
    dat_after$DTV <- NA
    for (i in 1:(length(after)-1)){
      dat_after$DTV[i] <- mean(tab$DTV[tab$Date >= after[i] & tab$Date < after[i+1]])
    }
  }
  #  approx(x=exposition$time, y=exposition$DTV, xout=seq(from, until, 1))
  dat_model <- rbind(dat_before, dat_after)
  during <- sum(accidents >= measure_start & accidents <= measure_end)
  measure_length <-  as.numeric(difftime(measure_end, measure_start, units="days")/365)
  ## check if time before and after measure
  if (sum(table(dat_model$measure)!=0)==1) stop("no data before or after the measure")
  if (is.null(exposition)){
    ## model measure and trend effect
    fit1 <- glm(accidents~Date*measure, data=dat_model, family = "poisson")
    ## model trend effect
    fit2 <- glm(accidents~Date+I(pmax(0,Date-as.numeric(measure_mean))), data=dat_model, family = "poisson")
    ## model measure effect and trend
    fit3 <- glm(accidents~Date+measure, data=dat_model, family = "poisson")
    ## model measure effect
    fit4 <- glm(accidents~measure, data=dat_model, family = "poisson")
    ## model trend
    fit5 <- glm(accidents~Date, data=dat_model, family = "poisson")
    ## model no effect
    fit6 <- glm(accidents~1, data=dat_model, family = "poisson")
    min_model <- try(max(which(c(fit1$aic, fit2$aic, fit3$aic, fit4$aic, fit5$aic, fit6$aic)==
                                 min(c(fit1$aic, fit2$aic, fit3$aic, fit4$aic, fit5$aic, fit6$aic), na.rm=TRUE))))
    if (is(min_model)[1]=="try-error" | length(min_model)==0 | min_model == -Inf)
      stop("With this data none of the models can be calculated!")
    modelname <- c("measure and trend effect", "trend effect", "measure effect and trend",
                   "measure effect", "trend", "no effect")[min_model]
  }
  if (!is.null(exposition)){
    ## model measure and trend effect
    fit1 <- glm(accidents~Date*measure+offset(log(DTV)), data=dat_model, family = "poisson")
    ## model trend effect
    fit2 <- glm(accidents~Date+I(pmax(0,Date-as.numeric(measure_mean)))+offset(log(DTV)), data=dat_model, family = "poisson")
    ## model measure effect and trend
    fit3 <- glm(accidents~Date+measure+offset(log(DTV)), data=dat_model, family = "poisson")
    ## model measure effect
    fit4 <- glm(accidents~measure+offset(log(DTV)), data=dat_model, family = "poisson")
    ## model trend
    fit5 <- glm(accidents~Date+offset(log(DTV)), data=dat_model, family = "poisson")
    ## model no effect
    fit6 <- glm(accidents~1+offset(log(DTV)), data=dat_model, family = "poisson")
    min_model <- try(max(which(c(fit1$aic, fit2$aic, fit3$aic, fit4$aic, fit5$aic, fit6$aic)==
                                 min(c(fit1$aic, fit2$aic, fit3$aic, fit4$aic, fit5$aic, fit6$aic), na.rm=TRUE))))
    if (is(min_model)[1]=="try-error" | length(min_model)==0 | min_model == -Inf)
      stop("With this data none of the models can be calculated!")
    modelname <- c("measure and trend effect", "trend effect", "measure effect and trend",
                   "measure effect", "trend", "no effect")[min_model]
    fit <- get(paste("fit", min_model, sep=""))
  }
  fit <- get(paste("fit", min_model, sep=""))
  test_overdisp <- 1-pchisq(deviance(fit),df.residual(fit))
  if (test_overdisp<=0.05){
    fit_start <- glm(fit$formula, data=dat_model, family = "quasipoisson",
                     control = glm.control(epsilon = 1e-8, maxit = 200, trace = FALSE))
    fit_nb <- try(MASS::glm.nb(fit$formula, data=dat_model, start=fit_start$coefficients,
                         control = glm.control(epsilon = 1e-6, maxit = 200, trace = FALSE)), silent=TRUE)
    if (attr(fit_nb, "class")[1] != "try-error") {
      fit <- fit_nb
    } else warning("Warning: Overdispersion in the Poisson Model, but Negative Binomial Model could not be estimated")
  }
  ## p_value for the measure effect only with negative measure effect, 1-sided test-> p-value/2
  pvalue_measure <- NA
  if("measureafter" %in% rownames(summary(fit)$coefficients) & min_model!=1){
    pvalue_measure <- ifelse(coef(summary(fit))["measureafter", 1]<0, coef(summary(fit))["measureafter", 4]/2,NA)
  }
  ## p_value for interaction only if the interaction term is negative
  pvalue_interaction <- NA
  if("Date:measureafter" %in% rownames(summary(fit)$coefficients)){
    pvalue_interaction <- ifelse(coef(summary(fit))["Date:measureafter", 1]<0, coef(summary(fit))["Date:measureafter", 4]/2,NA)
  }
  ## For the model with interaction, an estimation for the p-value for the measure is made over the confidence intervals.
  ## (Only if interaction term negative)
  if(min_model==1 & !is.na(pvalue_interaction)){
    pvalue_measure <- NA
    bef <- data.frame(measure=factor("before", levels=c("before", "after")), Date=measure_mean)
    aft <- data.frame(measure=factor("after", levels=c("before", "after")), Date=measure_mean)
    if (!is.null(exposition)){
      bef$DTV <- dat_before$DTV[length(dat_before$DTV)]
      aft$DTV <- dat_after$DTV[1]
    }
    preds_bef <- try(predict(fit, type="link", newdata = bef, se.fit = TRUE), silent=silent)
    preds_aft <- try(predict(fit, type="link", newdata = aft, se.fit = TRUE), silent=silent)
    intervalle <- c(seq(0.01,0.99,0.01),0.991,0.992,0.993,0.994, 0.995, 0.996, 0.997, 0.998, 0.999, 0.9999, 0.99999, 0.999999)
    if (is(preds_bef)[1]!="try-error"){
      i_v <- exp(preds_bef$fit-1*qnorm(intervalle)*preds_bef$se.fit)
    }
    if (is(preds_aft)[1]!="try-error"){
      i_n <- exp(preds_aft$fit+qnorm(intervalle)*preds_aft$se.fit)
    }
    pvalue_measure <- paste("no overlap of the ", intervalle[which(i_v-i_n<0)[1]-1]*100, "%-confidence interval", sep="")
  }
  pvalue_trend <- NA
  if("Date" %in% rownames(summary(fit)$coefficients)){
    pvalue_trend <- coef(summary(fit))["Date", 4]
  }
  ## Pearson residuals
  dat_fit <- data.frame(pearson=residuals.glm(fit, type="pearson")>=2,
                        row.names=attr(fit$qr$qr, "dimnames")[[1]])
  ## expected value
  dat_fit$expect <- predict(fit, type="response")
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
    before_bor <- data.frame(Date=before, measure=factor("before", levels=c("before", "after")), DTV=approx(x=dat_before$Date, y=dat_before$DTV, xout=before, rule=2 )$y)
    after_bor <- data.frame(Date=after, measure=factor("after", levels=c("before", "after")), DTV=approx(x=dat_after$Date, y=dat_after$DTV, xout=after, rule=2 )$y)
    expect_before <- rbind(data.frame(Date=before[1], accidents=NA, measure=factor("before", levels=c("before", "after")),
                                      DTV=approx(x=dat_before$Date, y=dat_before$DTV, xout=before[1], rule=2)$y),
                           dat_before, data.frame(Date=measure_start, accidents=NA, measure=factor("before", levels=c("before", "after")),
                                                  DTV=approx(x=dat_before$Date, y=dat_before$DTV, xout=measure_start, rule=2)$y))
    expect_after <- rbind(data.frame(Date=measure_end, accidents=NA, measure=factor("after", levels=c("before", "after")),
                                     DTV=approx(x=dat_after$Date, y=dat_after$DTV, xout=measure_end, rule=2)$y), dat_after,
                          data.frame(Date=after[length(after)], accidents=NA, measure=factor("after", levels=c("before", "after")),
                                     DTV=approx(x=dat_after$Date, y=dat_after$DTV, xout=after[length(after)], rule=2)$y))
  }
  ## Combine original data with the calculated data
  dat_model$rownames <- rownames(dat_model)
  dat_fit$rownames <- rownames(dat_fit)
  dat_total <- merge(x=dat_model, y=dat_fit, by="rownames", all=TRUE)
  dat_total <- dat_total[order(dat_total$Date),]
  preds_exp_bef <- try(predict(fit, type= "link", newdata= expect_before, se.fit = TRUE))
  preds_exp_aft <- try(predict(fit, type= "link", newdata= expect_after, se.fit = TRUE))
  preds_before_bor <- try(predict(fit, type= "link", newdata= before_bor, se.fit = TRUE))
  preds_after_bor <- try(predict(fit, type= "link", newdata= after_bor, se.fit = TRUE))
  expect_before$expect <- exp(preds_exp_bef$fit)
  expect_after$expect <-  exp(preds_exp_aft$fit)
  before_bor$expect <- exp(preds_before_bor$fit)
  after_bor$expect <- exp(preds_after_bor$fit)
  expect_before$low <- exp(preds_exp_bef$fit-qnorm(0.975)*preds_exp_bef$se.fit)
  expect_before$upp <- exp(preds_exp_bef$fit+qnorm(0.975)*preds_exp_bef$se.fit)
  expect_after$low <- exp(preds_exp_aft$fit-qnorm(0.975)*preds_exp_aft$se.fit)
  expect_after$upp <- exp(preds_exp_aft$fit+qnorm(0.975)*preds_exp_aft$se.fit)
  before_bor$low <- exp(preds_before_bor$fit-qnorm(0.975)*preds_before_bor$se.fit)
  before_bor$upp <- exp(preds_before_bor$fit+qnorm(0.975)*preds_before_bor$se.fit)
  after_bor$low <- exp(preds_after_bor$fit-qnorm(0.975)*preds_after_bor$se.fit)
  after_bor$upp <- exp(preds_after_bor$fit+qnorm(0.975)*preds_after_bor$se.fit)
  ## visualization
  if (is.null(main)) main <- paste("measure", paste(measure_mean, collapse = ","), sep=" ")
  if (is.null(x_axis)) x_axis <- as.Date(paste0(as.numeric(format(from, '%Y')):(as.numeric(format(until, '%Y'))+1), "-01-01"))
  # if (test_conf){
  if(sum(after_bor$upp[after_bor$measure=="after"] != Inf)==0){
    #if(Modell_final=="Unfaelle ~ Massnahme"){ #macht das bei den anderen Modellen auch Sinn
    faelle <- sum(after_bor$measure=="after")
    q_Jahr <- 0.025^{1/faelle}
    #plot(x=1:10, y=0.025^(1/(1:10)), pch=16, xlab="Jahre", ylab="P(0 pro Jahr)", las=1)
    lambda_est <- -log(q_Jahr)#seq(0, 5, 0.001)[which(qpois(q_Jahr, lambda=seq(0, 5, 0.001))>0)[1]-1]
    #qpois(1-q_Jahr,lambda=lambda_est)
    after_bor$upp[after_bor$measure=="after"] <- lambda_est#0.5*qchisq(1-q_Jahr, df=2*lambda_est)
    after_bor$upp <- lambda_est
    expect_after$upp <- lambda_est
  }
  # Base plot
  if (is.null(exposition)){
    p <- ggplot2::ggplot(dat_total,  ggplot2::aes(x=Date, y=accidents)) +
      ggplot2::geom_vline(xintercept=before_bor$Date, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=after_bor$Date, colour="darkgrey", linetype=2)+
      ggplot2::geom_ribbon(data=rbind(expect_before, expect_after), ggplot2::aes(ymin=low,ymax=upp), fill="grey", alpha=0.5)+
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
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
      ggplot2::geom_point(x=mean(c(measure_end, measure_start)), y=during/measure_length, colour="grey", na.rm=TRUE)

    ## Connect the measured values after the measure, if there are enough measured values
    if (sum(dat_total$Date>measure_mean)>1){
      p <- p +
        ggplot2::geom_line(data=dat_total[dat_total$Date>measure_mean,], ggplot2::aes(x=Date, y=accidents))
    }
    ## customization
    if (!is.null(y_axis)){
      p <- p +
        ggplot2::scale_y_continuous(breaks=y_axis, expand = c(0, 0), limits=range(y_axis))
    }
    if (is.null(y_axis)){
      p <- p +
        ggplot2::scale_y_continuous(breaks=  scales::pretty_breaks())
    }
  }
  if (!is.null(exposition)){
    if (is.null(max_y)) max_y <- max(dat_total$expect/dat_total$DTV, dat_total$accidents/dat_total$DTV,
                                     before_bor$upp/before_bor$DTV, after_bor$upp/after_bor$DTV, na.rm=TRUE)*1.1
    scal <- 10^(floor(log10(ceiling(1/max_y))) + 1)
    p <- ggplot2::ggplot(dat_total, ggplot2::aes(x=Date, y=accidents/DTV* scal)) +
      ggplot2::geom_vline(xintercept=before_bor$Date, colour="darkgrey", linetype=2) +
      ggplot2::geom_vline(xintercept=after_bor$Date, colour="darkgrey", linetype=2)+
      ggplot2::geom_ribbon(data=rbind(expect_before, expect_after), ggplot2::aes(ymin=low/DTV* scal,ymax=upp/DTV* scal), fill="grey", alpha=0.5)+
      ggplot2::scale_x_date(breaks=x_axis, labels = scales::date_format("%Y"))+
      ggplot2::ylab(paste("accident rate *", formatC(scal, format = "e", digits = 0))) +
      ggplot2::ggtitle(main)+
      ggplot2::theme_bw()
    before_bor$accidents=NA
    after_bor$accidents=NA
    for (z in seq(1, dim(before_bor)[1], 2)){
      p <- p +
        ggplot2::geom_ribbon(data=before_bor[z:(z+1),], ggplot2::aes(ymin=low/DTV* scal,ymax=upp/DTV* scal), fill="darkgrey", alpha=0.5)
    }
    for (z in seq(1, dim(after_bor)[1], 2)){
      p <- p +
        ggplot2::geom_ribbon(data=after_bor[z:(z+1),], ggplot2::aes(ymin=low/DTV* scal,ymax=upp/DTV* scal), fill="darkgrey", alpha=0.5)
    }
    p <- p +
      ggplot2::geom_point() +
      ggplot2::geom_line(data=expect_before, ggplot2::aes(y=expect/DTV* scal, x=Date), col="blue", linetype = 1)+
      ggplot2::geom_line(data=expect_after, ggplot2::aes(y=expect/DTV* scal, x=Date), col="blue", linetype = 1)+
      ggplot2::geom_segment(ggplot2::aes(x=expect_before$Date[dim(expect_before)[1]], xend=expect_after$Date[1],
                       y=expect_before$expect[dim(expect_before)[1]]/expect_before$DTV[dim(expect_before)[1]]* scal,
                       yend=expect_after$expect[1]/expect_after$DTV[1]* scal), col="blue", linetype  = 2)+
      ggplot2::geom_line(data=dat_total[dat_total$Date<measure_mean,], ggplot2::aes(x=Date, y=accidents/DTV* scal)) +
      ggplot2::geom_vline(xintercept=measure_end, colour="red")+
      ggplot2::geom_vline(xintercept=measure_start, colour="red")+
      ggplot2::geom_point(x=mean(c(measure_end, measure_start)), y=during/measure_length/tab$DTV[round(tab$Date)==round(measure_mean)]*scal, colour="grey", na.rm=TRUE)

    ## Connect the measured values after the measure, if there are enough measured values
    if (sum(dat_total$Date>measure_mean)>1){
      p <- p +
        ggplot2::geom_line(data=dat_total[dat_total$Date>measure_mean,], ggplot2::aes(x=Date, y=accidents/DTV* scal))
    }
    ## customization
    if (!is.null(y_axis)){
      p <- p +
        ggplot2::scale_y_continuous(breaks=y_axis, expand = c(0, 0), limits=range(y_axis))
    }
    if (is.null(y_axis)){
      p <- p +
        ggplot2::scale_y_continuous(breaks=  scales::pretty_breaks())
    }
  }
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
  output <- list(fit = fit, modelname=modelname, data = rbind(expect_before, expect_after),  pvalue_measure= pvalue_measure, pvalue_trend=pvalue_trend,  pvalue_interaction= pvalue_interaction,
                 test_overdisp = test_overdisp, plot_KI = p2,  plot=p, lang = lang)
  class(output) <- "class_effectiveness"
  return(output)
}

"print.class_effectiveness" <- function(object)
{
  if (!inherits(object, "class_effectiveness"))
  {
    stop("Not a effectiveness object")
  }
  modelname <- c("measure and trend effect", "trend effect", "measure effect and trend",
               "measure effect", "trend", "no effect")
  reliability <- c("not reliable, no effect", "highly reliable", "well reliable", "weakly reliable")
  measure <- "Effect of measures"
  min_model <- which(object$modelname==modelname)
  if (object$lang == "de"){
    modelname <- c("Massnahmen- und Trendeffekt", "Trendeffekt", "Massnahmeneffekt and Trend",
                   "Massnahmeneffekt", "Trend", "kein Effekt")
    reliability <- c("nicht verlaesslich, keine Wirkung", "stark verlaesslich", "gut verlaesslich", "schwach verlaesslich")
    measure <- "Massnahmeneffekt"
  }
  if (object$lang == "fr"){
    modelname <- c("Effet des mesures et de la tendance", "Effet de tendance", "Effet des mesures et tendance",
                   "Effet des mesures", "Tendance", "aucun effet")
    reliability <- c("pas fiable, pas d'effet", "tres fiable", "assez fiable", "faiblement fiable")
    measure <- "Effet des mesures"
  }
  if (object$lang == "it"){
     modelname <- c("Misure ed effetto tendenza", "Effetto tendenza", "Effetto delle misure ed tendenza",
                    "Effetto delle misure", "Tendenza", "nessun effett")
     reliability <- c("non affidabile, nessun effett", "altamente affidabile", "altamente affidabile",
                      "debolmente affidabile")
     measure <- "Effetto delle misure"
  }
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
  print(object$plot)
  cat(modelname[min_model])
  if (min_model==1 & !is.na(object$pvalue_interaction)) cat("\n", paste0(measure,": ", object$pvalue_measure))
  if (!(min_model==1 & !is.na(object$pvalue_interaction))) cat("\n", paste0(measure,": ", reliability[k]))
}


"plot.class_effectiveness" <- function(object)
{
  if (!inherits(object, "class_effectiveness"))
  {
    stop("Not a effectiveness object")
  }
  print(object$plot)
}



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


