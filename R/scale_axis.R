#' Scales values for use on a secondary axis in ggplot.
#'
#' This function, combined with [scale_value()], makes it easy to use a secondary axis in ggplot. Simply supply the possible values for both axes to rescale values for the secondary axis.
#' This function calculates the axis labels by shifting the values (subtracting the minimum value of the second set from the minimum of the first set) and rescaling them by the difference in variance between the two sets.
#' Simply put: `axis value = shift - value * scale * -1`
#'
#' @param value The value requiring rescaling
#' @param first List of values that determines the primary axis
#' @param second List of values that determines the secondary axis
#'
#' @return Rescaled and transformed values that will fit on the primary axis
#' @import ggplot2
#' @import lubridate
#' @export
#' @seealso [scale_value()], [sec_axis()]
#'
#' @examples
#' library(ggplot2)
#' library(lubridate)
#' dataset = data.frame(t=seq(ymd("2023-01-01"), ymd("2023-01-10"), 1),
#'                      primary=1:10,
#'                      secondary=log(1:10))
#' ggplot(dataset) +
#'   geom_point(aes(x=t, y=primary)) +
#'   geom_line(aes(x=t, y=scale_value(secondary, primary, secondary))) +
#'   scale_y_continuous(sec.axis=sec_axis(~scale_axis(., dataset$primary, dataset$secondary),
#'                                        name="Log"))
#'
scale_axis = function (value, first, second) {
  max_first  <- max(first, na.rm=T)
  max_second <- max(second, na.rm=T)
  min_first  <- min(first, na.rm=T)
  min_second <- min(second, na.rm=T)

  scale = (min_second - max_second)/(min_first - max_first)
  if (scale == 0) scale = 0.1
  shift = min_first - min_second

  return((shift - value*scale) * -1)
}
