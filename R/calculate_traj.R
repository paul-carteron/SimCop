#' calculate_traj
#'
#' @param cg_range Range of calculation for the trajectory
#' @param density_init A vector of initial densities (numeric)
#' @param coeff_traj Coefficient a, b, a0, and b0 of the trajectory (cf : Ningre et al "Trajectoires d’autoéclaircie du Douglas en France)
#'
#' @return Data.frame with trajectory calculated for the cg_range
#' @export
#'
#' @examples
#' \dontrun{calculate_traj()}
#'
calculate_traj = function(cg_range,
                    density_init = 1000,
                    coeff_traj = c(a = 13.532,
                                   b = -1.461,
                                   a0 = 14.21,
                                   b0 = -1.79)){

   a = coeff_traj["a"]
   b = coeff_traj["b"]
   a0 = coeff_traj["a0"]
   b0 = coeff_traj["b0"]

   Cg0 = (density_init/exp(a0))^(1/b0)
   RDI0 = density_init/(exp(a)*(density_init/exp(a0))^(b/b0))

   LNCG1 = (2 * (log(density_init) - a) - b * log(Cg0)) / b
   Cg1 = exp(LNCG1)
   p = log(density_init) + (b ^ 2 * log(Cg0) ^ 2) / (4 * log(RDI0))
   q = -(b ^ 2 * log(Cg0)) / (2 * log(RDI0))
   r = (b ^ 2) / (4 * log(RDI0))

   traj = ifelse(cg_range <= Cg0,
                     exp(log(density_init)), # Plateau de la trajectoire
                ifelse(cg_range <= Cg1,
                       exp(p + q * log(cg_range) + r * log(cg_range) ^ 2), # Partie polynomiale
                           exp(a + b * log(cg_range)))) # Asymptote

   return(traj)
}
