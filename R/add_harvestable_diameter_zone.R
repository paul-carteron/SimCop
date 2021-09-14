#' add_harvestable_diameter_zone
#'
#' @param diam_min numeric corresponding of the minimal harvestable diameter
#' @param diam_max numeric corresponding of the minimal harvestable diameter
#' @param coeff_traj coefficient a, b, a0, and b0 of the mortality trajectory (cf : Ningre et al "Trajectoires d auto-eclaircie du Douglas en France)
#' @param ... other argument passed on to layer (ex : color, linetype, ...)
#'
#' @return ggplot object highlighting zone of harvestable diameter
#' @export
#'
#' @importFrom ggplot2 geom_polygon aes
#'
#' @examples
#' \dontrun{add_harvestable_diameter(diam_min = 40, diam_max = 45, fill = "orange", alpha = 0.5)}
#'
add_harvestable_diameter_zone = function(diam_min,
                                         diam_max,
                                         coeff_traj = c(a = 13.532,
                                                 b = -1.461,
                                                 a0 = 14.21,
                                                 b0 = -1.79),
                                         ...){

   cg_min = pi * diam_min
   cg_max = pi * diam_max

   res = list(geom_polygon(data = NULL,
                           aes(x = c(cg_min, cg_max, cg_max, cg_min),
                               y = c(exp(-Inf),
                                     exp(-Inf),
                                     cg_max ^ coeff_traj["b"] * exp(coeff_traj["a"]),
                                     cg_min ^  coeff_traj["b"] * exp(coeff_traj["a"]))),
                           ...)
   )
   return(res)
}

