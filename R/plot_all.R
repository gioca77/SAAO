#' Function to plot all the single case of an evaluation of a multiple location effectiveness evaluation
#'
#' @param object output of effectiveness_multi-Function
#' @export
#' @examples
#'   multi1 <- effectiveness_multiple(accidents = list(example_no_effect, example_measure_effect),
#'   measure_start = c('1.1.2011', '1.1.2012'), measure_end = c('1.1.2011', '1.1.2012'))
#'   plot_all(multi1)
#'
plot_all <- function(object)
{
  if (!inherits(object, "class_effectiveness_multi"))
  {
    stop("Not a effectiveness_multi object")
  }
  do.call(gridExtra::grid.arrange, object$plot_all)
}
