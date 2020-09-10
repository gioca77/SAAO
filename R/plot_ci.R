#' Function to directly plot confidence interval of measure
#'
#' @param object output of effectiveness-Function
#' @export
#' @examples
#'   # example
#'   ex1 <- effectiveness(accidents = example_no_effect, measure_start = '1.1.2011', measure_end = '1.1.2011')
#'   # plot_ci(ex)
plot_ci <- function(object)
{
  if (!inherits(object, "class_effectiveness"))
  {
    stop("Not a effectiveness object")
  }
  if(!is.null(object$plot_KI)){
    print(object$plot_KI)
  } else {print("no confidence interval plot available")}
}
