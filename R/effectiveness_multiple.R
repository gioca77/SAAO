#' Evaluation effectiveness of measures with data from multiple locations
#'
#' @description Function to evaluate the effectiveness of measures to reduce traffic accidents with data from multiple locations.
#'
#' @details
#' Traffic accidents before and after the implementation of a traffic measure at different locations are analyzed together to evaluate the overall effect of the measure. For the analysis, the accident data at the different locations before and after the action are added up. The implementation of the measures does not have to take place simultaneously, the data is adjusted accordingly. Before and after the measure, only the time range that is available for all locations is taken into account. This means that the site with the shortest time series before the measure and the site with the shortest time series after the measure determine the length of the evaluation period. As in the function \code{\link[STAAD:effectiveness]{effectiveness}} the counting data are modeled using count regression, by default with a Poisson model. However, the model is tested for overdispersion and in case of significant overdispersion it is automatically switched to a Negative Binomial model. For the situation analysis, six different model scenarios for the measure are evaluated:  no effect, trend, effect of measures, measure effect and trend, trend effect, measures and trend effect. The best model is displayed. The exposure can optionally be considered as an offset.
#' @param accidents A list with either an R date/time or character vectors with accident dates. For character vectors, the following date formats are allowed '2014-04-22', '2014/04/22' respectively '22.4.2014'.
#' @param measure_start A vector with the dates (R date/time as well as character in '2014-04-22', '2014/04/22' or '22.4.2014 format) when the implementation of the measure started for each location. The vector must either be the same length as the number of elements in the accident-list or contain only one value. If only one value is given, it is used for all locations.
#' @param measure_end A vector with the dates the implementation when the measure was finished for each location (first day after the measure). The vector must either be the same length as the number of elements in the accident-list or contain only one value. If only one value is given, it is used for all locations.
#' @param exposition Optional list with data frames with exposition data. The first column is the time value, the second column the exposure. If the time value is a specific date (e.g. '22.4.2014'), this is considered as the start date of this exposure. If the time value is a year (format '2010') the exposure is taken for the whole year. Exposure values are extended until a new entry is available. If necessary, the first exposure value is extended backwards. If only one data frame is given, it is used for all locations. DEFAULT NULL.
#' @param from Vector with dates or year (1.1.) from which the time series should be considered. Optional. If not specified, the 1.1 from the year of the earliest accident is used. Must be specified for none or all locations.  If only one value is given, it is used for all locations.
#' @param until Vector with dates or year (31.12) until which the time series should be considered. Optional. If not specified, the 31.12 from the year of the latest accident is used. Must be specified for none or all locations.  If only one value is given, it is used for all locations.
#' @param main Optional title for the plot.
#' @param x_axis Optional vector with the values for the x-axis.
#' @param max_y Optional maximum value for the y-axis.
#' @param orientation_x Alignment of the labels of the x-axis; "v" for vertical, "h" for horizontal, by default horizontal alignment is selected for 8 years or less.
#' @param add_exp Option to supplement the output plot with the exposure as an additional axis. Furthermore an additionally plot of the exposure alone is produced. Only active if exposure is stated.
#' @param KI_plot TRUE/FALSE if an additional illustration with the 95\% confidence interval for the measure effect is produced (only of limited use for models without measure effect).
#' @param lang language for output ("en", "fr", "de" or "it").
#' @seealso \code{\link[STAAD:effectiveness]{effectiveness()}} for the analysis of a single measure.
#' @return A specific R object (\code{class_effectiveness_multi}) is generated as function output. The main object is the illustration with a graphical analysis of the measures. The \code{print.class_effectiveness_multi()} is used to extract the most important key figures of the analysis.
#' Specifically, the output contains a list of the following elements:
#' \item{\code{fit}}{Output of the selected counting regression model (Negative Binomial or Poisson family.}
#' \item{\code{modelname}}{Selected model.}
#' \item{\code{data}}{Prepared data combined from the different locations that were used for the analysis.}
#' \item{\code{pvalue_measure}}{p-value of the positive measure effect, if it exists.}
#' \item{\code{pvalue_interaction}}{p-value of the interaction term, if it exists.}
#' \item{\code{test_overdisp}}{p-value of the deviance dispersion test.}
#' \item{\code{plot}}{Plot graphical analysis (ggplot-class).}
#' \item{\code{plot_all}}{List with plots with the analysis of all the individuell locations. Can be visualized with the function \code{\link[STAAD:plot_all]{plot_all()}}.}
#' \item{\code{cases}}{Number of analyzed locations.}
#' \item{\code{cases_exp}}{Number of locations with exposition data.}
#' \item{\code{plot_KI}}{Additional illustration with the 95\% confidence interval for the measure effect (ggplot-class).}
#' \item{\code{conf_limit}}{Overlapping of the confidence intervals before and after the measure.}
#' \item{\code{lang}}{Selected language.}
#' \item{\code{plot_exposition}}{Addional plot of the exposition, if available (ggplot-class).}
#' @export
#' @examples
#'   multi1 <- effectiveness_multiple(accidents = list(example_no_effect, example_measure_effect),
#'   measure_start = c('1.1.2011', '1.1.2012'), measure_end = c('1.1.2011', '1.1.2012'))
#'   print(multi1)
#'   multi2 <- effectiveness_multiple(accidents = list(example_no_effect, example_measure_effect),
#'   measure_start = c('1.1.2011', '1.1.2012'), measure_end = c('1.1.2011', '1.1.2012'), exposition = exposition_ex2)
#'   print(multi2)
#'   plot_all(multi2)

effectiveness_multiple <- function(accidents, measure_start, measure_end,
                                   exposition = NULL, from = NULL,
                          until = NULL, main = NULL, x_axis = NULL,
                          max_y = NULL, orientation_x = NULL,
                          add_exp = FALSE, KI_plot = FALSE,  lang = "en"){
  ## internal parameters
  silent = FALSE # silent: parameter to suppress error messages during model evaluation
  ## check mandatory input
  if (methods::is(accidents)[1] == "list"){
    len <- length(accidents)
  } else len <- 1
  if (length(measure_start) == 1) measure_start <- rep(measure_start, len)
  if (length(measure_end) == 1) measure_end <- rep(measure_end, len)
  for (i in 1:len){
    accidents[[i]] <- try(as.Date(accidents[[i]], tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")),
                          silent = silent)
  }
  measure_start <- try(as.Date(measure_start, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")),
                       silent = silent)
  measure_end <- try(as.Date(measure_end, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")),
                     silent = silent)
  Check <- newArgCheck_sep()
  #* accidents format
  for (i in 1:len){
    if (methods::is(accidents[[i]])[1] ==  "try-error")
      addError_sep(msg = paste0("'accidents' [[", i, "]] not in the right format"), argcheck = Check)
  }
  #* measure_start format
  if (methods::is(measure_start)[1] ==  "try-error")
    addError_sep(msg = "wrong time format for 'measure_start'", argcheck = Check)
  if (length(measure_start) !=  len)
    addError_sep(msg = "not the right amount of values for 'measure_start'", argcheck = Check)
  #* measure_end format
  if (methods::is(measure_end)[1] ==  "try-error")
    addError_sep(msg = "wrong time format for 'measure_end'", argcheck = Check)
  if (length(measure_end) !=  len)
    addError_sep(msg = "not the right amount of values for 'measure_end'", argcheck = Check)
  ## check optional input
  if (!is.null(exposition)){
    if (methods::is(exposition)[1] != "list") exposition <- rep(list(exposition), len)
    for (i in 1:length(exposition)){
      if (!is.null(dim(exposition[[i]]))){
        if (dim(exposition[[i]])[2] >= 2){
          colnames(exposition[[i]])[1:2] <- c("time", "Exp")
          if (any(nchar(as.character(exposition[[i]]$time))!=4)){
            exposition[[i]]$time <- try(as.Date(exposition[[i]]$time, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
          } else exposition[[i]]$time <- try(as.numeric(exposition[[i]]$time), silent = silent)
        }
      }
    }
  }
  #* exposition format
  if (!is.null(exposition)){
    for (i in 1:len){
      if (!is.null(dim(exposition[[i]]))){
        if (dim(exposition[[i]])[2]<2)
        addError_sep(msg = paste0("wrong input for 'exposition' ", i), argcheck = Check)
      }
    }
    if (length(exposition) !=  len)
      addError_sep(msg = "not the right amount of elements for 'exposition'", argcheck = Check)
  }
  if (!is.null(from)){
    if (length(from) == 1) from <- rep(from, len)
    if (any(nchar(as.character(from)) != 4)){
      from <- try(as.Date(from, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
      } else from <-  try(as.Date(paste0(from, "-01-01")), silent=silent)
  }
  if (!is.null(until)){
    if (length(until) == 1) until <- rep(until, len)
    if (any(nchar(as.character(until)) != 4)){
      until <- try(as.Date(until, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y")), silent = silent)
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
  #* from time format
  if (methods::is(from)[1] ==  "try-error")
    addError_sep(msg = "wrong time format for 'from'", argcheck = Check)
  #* until time format
  if (methods::is(until)[1] ==  "try-error")
    addError_sep(msg = "wrong time format for 'until'", argcheck = Check)
  #* check until > from
  if (methods::is(from)[1]=="Date" & methods::is(until)[1]=="Date"){
    if (any(from > until))
      addError_sep(msg = "'until' has to be greater then 'from'", argcheck = Check)
  }
  #* check measure_end >= measure_start
  if (methods::is(measure_end)[1]=="Date" & methods::is(measure_start)[1]=="Date"){
    if (any(measure_start > measure_end))
      addError_sep(msg = "'measure_end' has to be greater or equal then 'measure_start'", argcheck = Check)
  }
  #* Return errors and warnings (if any)
  finishArgCheck_sep(Check)
  before_length <- rep(NA,len)
  after_length <- rep(NA,len)
  plot_list <- list()
  if (lang == "en") main_loc <- "location"
  if (lang == "de") main_loc <- "location"
  if (lang == "fr") main_loc <- "site"
  if (lang == "it") main_loc <- "posizione"
  for(i in 1:len){
    if(is.null(from)) from_case <- NULL
    if(!is.null(from)) from_case <- from[i]
    if(is.null(until)) until_case <- NULL
    if(!is.null(until)) until_case <- until[i]
    if (is.null(exposition)) exposition_case <- NULL
    if (!is.null(exposition)) exposition_case <- exposition[[i]]
    case <- try(effectiveness(accidents = accidents[[i]], measure_start = measure_start[i], measure_end = measure_end[i],
                          exposition = exposition_case, main=paste(main_loc, i), from = from_case, until = until_case, lang = lang,
                          add_exp = add_exp))
    if (methods::is(case)[1] ==  "try-error") {
      stop(paste0("Error in location ", i, "."))
    }
    before_length[i] <- dim(case$data[case$data$measure=="before" & !is.na(case$data$accidents), ])[1]
    after_length[i] <- dim(case$data[case$data$measure=="after" & !is.na(case$data$accidents), ])[1]
    assign(paste0("case_",i), case)
    plot_list[[i]] <- case$plot
  }
  plot_all <- plot_list
  before_total <- rep(0, min(before_length))
  after_total <- rep(0, min(after_length))
  if (!is.null(exposition)){
    before_exp <- rep(0, min(before_length))
    after_exp <- rep(0, min(after_length))
  }
  len_exp <- 0
  for(i in 1:len){
    out <- get(paste0("case_",i))
    before_total <- before_total + out$data$accidents[out$data$measure=="before" &
                                                        !is.na(out$data$accidents)][(before_length[i] - min(before_length)+1):before_length[i]]
    after_total <- after_total + out$data$accidents[out$data$measure=="after" &
                                                      !is.na(out$data$accidents)][1:min(after_length)]
    if (!is.null(exposition) & !is.null(out$data$Exp)){
      len_exp <- len_exp + 1
      before_exp <- before_exp +  out$data$Exp[out$data$measure=="before" &
                                                     !is.na(out$data$accidents)][(before_length[i] - min(before_length)+1):before_length[i]]
      after_exp <- after_exp +  out$data$Exp[out$data$measure=="after" &
                                             !is.na(out$data$accidents)][1:min(after_length)]
    }
  }
  accident_total <- NULL
  for (i in 1:(min(before_length)+min(after_length))){
    accident_total <- append(accident_total, rep(as.Date(paste0(i, "-07-01")),c(before_total, after_total)[i]))
  }
  x_axis <- as.Date(paste0(1:(min(before_length)+min(after_length)), "-07-01"))
  if (is.null(main)) main <- ""
  if(is.null(exposition)) exposition_total <- NULL
  if(!is.null(exposition)) exposition_total <- data.frame(Date=as.Date(paste0(1:(min(before_length)+min(after_length)), "-07-01")),
                                                          Exp = c(before_exp, after_exp))
  total <- effectiveness(accidents = accident_total, measure_start = as.Date(paste0(min(before_length)+1, "-01-01")),
                         measure_end = as.Date(paste0(min(before_length)+1, "-01-01")), exposition = exposition_total,
                         x_axis = x_axis, main=main, max_y = max_y, orientation_x = orientation_x,
                         KI_plot = KI_plot,  lang = lang, add_exp = add_exp)
  dat <- total$data[2:(dim(total$dat)[1]-1),]
  dat$Date <- c(1:min(before_length), rep(min(before_length)+0.5,2), (min(before_length+1):(min(before_length)+min(after_length))))
  colnames(dat)[1] <- "Year"
  if (!add_exp | is.null(exposition)) output <- list(fit = total$fit, modelname = total$modelname, data = total$data,  pvalue_measure= total$pvalue_measure,
                                                     pvalue_trend = total$pvalue_trend,  pvalue_interaction = total$pvalue_interaction,
                                                     test_overdisp = total$test_overdisp, plot = total$plot, plot_all = plot_all, cases = len,
                                                     cases_exp = len_exp, plot_KI = total$plot_KI, conf_limit = total$conf_limit,
                                                     lang = total$lang)
  if (add_exp & !is.null(exposition)) output <- list(fit = total$fit, modelname = total$modelname, data = total$data,  pvalue_measure= total$pvalue_measure,
                                                     pvalue_trend = total$pvalue_trend,  pvalue_interaction = total$pvalue_interaction,
                                                     test_overdisp = total$test_overdisp, plot = total$plot, plot_all = plot_all, cases = len,
                                                     cases_exp = len_exp, plot_KI = total$plot_KI, conf_limit = total$conf_limit,
                                                     lang = total$lang, plot_exposition = total$plot_exposition)
  class(output) <- "class_effectiveness_multi"
  return(output)
}


#' @method print class_effectiveness_multi
#' @export

"print.class_effectiveness_multi" <- function(object, plot = TRUE)
{
  if (!inherits(object, "class_effectiveness_multi"))
  {
    stop("Not a effectiveness_multi object")
  }
  modelname <- c("measure and trend effect", "trend effect", "measure effect and trend",
                 "measure effect", "trend", "no effect")
  reliability <- c("not reliable, no effect proven", "highly reliable", "well reliable", "weakly reliable")
  measure <- "Effect of measures"
  min_model <- which(object$modelname==modelname)
  if (object$lang == "en"){
    locations <- paste("Evaluation of the combined measure effect from", object$cases, "locations.")
    exp_locations <- paste0("(", object$cases_exp, " locations with exposition data.)")
    model <- "model"
    nb <- "Negative binomial"
    pv <- "p-value"
    od <- "overdispersion"
  }
  if (object$lang == "de"){
    modelname <- c("Massnahmen- und Trendeffekt", "Trendeffekt", "Massnahmeneffekt und Trend",
                   "Massnahmeneffekt", "Trend", "kein Effekt")
    reliability <- c("nicht verlaesslich, keine Wirkung nachgewiesen", "stark verlaesslich", "gut verlaesslich", "schwach verlaesslich")
    measure <- "Massnahmeneffekt"
    locations <- paste("Evaluation des kombinierten Massnahmeneffektes von", object$cases, "Standorten.")
    exp_locations <- paste0("(", object$cases_exp, " Standorte mit Expositionsdaten.)")
    model <- "Modell"
    nb <- "Negative Binomial"
    pv <- "p-Wert"
    od <- "Overdispersion"
  }
  if (object$lang == "fr"){
    modelname <- c("Effet des mesures et de la tendance", "Effet de tendance", "Effet des mesures et tendance",
                   "Effet des mesures", "Tendance", "aucun effet")
    reliability <- c("pas fiable, pas d'effet prouve", "tres fiable", "assez fiable", "faiblement fiable")
    measure <- "Effet des mesures"
    locations <- paste("Evaluation of the combined measure effect from", object$cases, "locations.")
    exp_locations <- paste0("(", object$cases_exp, " locations with exposition data.)")
    model <- "model"
    nb <- "binomiale negative"
    pv <- "valeur p"
    od <- "overdispersion"
  }
  if (object$lang == "it"){
    modelname <- c("Misure ed effetto tendenza", "Effetto tendenza", "Effetto delle misure ed tendenza",
                   "Effetto delle misure", "Tendenza", "nessun effett")
    reliability <- c("non affidabile, nessun effett provato", "altamente affidabile", "altamente affidabile",
                     "debolmente affidabile")
    locations <- paste("Evaluation of the combined measure effect from", object$cases, "locations.")
    exp_locations <- paste0("(", object$cases_exp, " locations with exposition data.)")
    measure <- "Effetto delle misure"
    model <- "modello"
    nb <- "binomiale negativa"
    pv <- "Valore p"
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
  cat(locations)
  if (object$cases_exp > 0) cat(exp_locations)
  cat("\n", modelname[min_model])
  if (!is.null(object$fit$theta))
  {
    cat("\n", paste(nb, model, "(Theta =", round(object$fit$theta, 2), ")"))
  }
  if (is.null(object$fit$theta))
  {
    cat("\n", paste(object$fit$family$family, model))
  }
  cat("\n", paste(pv, od, round(object$test_overdisp, 3)))
  if (!is.na(object$conf_limit)) cat("\n", paste0(measure,": ", reliability[k], " (",object$pvalue_measure, ")"))
  if (is.na(object$conf_limit)) cat("\n", paste0(measure,": ", reliability[k]))
  cat("\n")
}

#' @method plot class_effectiveness_multi
#' @export

"plot.class_effectiveness_multi" <- function(object)
{
  if (!inherits(object, "class_effectiveness_multi"))
  {
    stop("Not a effectiveness_multi object")
  }
  print(object$plot)
}

#' @method summary class_effectiveness_multi
#' @export

"summary.class_effectiveness_multi" <- function(object)
{
  if (!inherits(object, "class_effectiveness_multi"))
  {
    stop("Not a effectiveness_multi object")
  }
  sum_output <- list(model = object$fit, modelname=object$modelname, data = object$data,
                     p_value_measure = object$pvalue_measure, p_value_trend = object$pvalue_trend,
                     p_value_interaction = object$pvalue_interaction, p_value_test_overdispersion = object$test_overdisp)
  sum_output
}
