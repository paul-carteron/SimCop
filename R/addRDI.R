#' add_RDI_line
#'
#' @param RDI numeric between 0 and 1
#' @param coeff_traj coefficient a, b, a0, and b0 of the mortality trajectory (cf : Ningre et al "Trajectoires d auto-eclaircie du Douglas en France)
#' @param ... other argument passed on to layer (ex : color, linetype, ...)
#'
#' @return ggplot objetc correpsonding to an RDI line
#' @export
#'
#' @importFrom ggplot2 geom_text aes geom_line last_plot
#'
#' @examples
#' \dontrun{add_RDI_line(RDI = 0.15, linetype = "dashed", color = "red")}
#'
add_RDI_line = function(RDI = 0.15,
                        coeff_traj = c(a = 13.532,
                                       b = -1.461,
                                       a0 = 14.21,
                                       b0 = -1.79),
                        ...){

   max_x = last_plot()$coordinates$limits$x[2]
   min_x = last_plot()$coordinates$limits$x[1]
   max_y = last_plot()$coordinates$limits$y[2]

   a = coeff_traj["a"]
   b = coeff_traj["b"]

   if (RDI*exp(a)*min_x^b > max_y) {
      x_text = exp((log(max_y) - a - log(RDI))/b)
      y_text = max_y
      nudge_y_text = 0.05
      nudge_x_text = 0.03
   }else{
      x_text = min_x
      y_text = RDI*exp(a)*min_x^b
      nudge_y_text = -0.02
      nudge_x_text = 0.09
   }

   RDI_line = list(geom_line(aes(x = min_x:max_x,
                                  y = RDI*exp(a)*c(min_x:max_x)^b), ...),
                    geom_text(aes(x = x_text, y = y_text, label = RDI),
                              nudge_y = nudge_y_text, nudge_x = nudge_x_text, ...))

   return(RDI_line)
}
