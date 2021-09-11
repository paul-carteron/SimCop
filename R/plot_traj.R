#' plot_traj
#'
#' @param density_init A vector of initial densities (numeric)
#' @param coeff_traj Coefficient a, b, a0, and b0 of the mortality trajectory (cf : Ningre et al "Trajectoires d auto-eclaircie du Douglas en France)
#' @param scale A vector compose of xmin, xmax, ymin and ymax
#' @param ... Param to give to th ggplot (color, linetype, alpha, ...)
#' @param remove_theme If TRUE, already created theme for the graph is remove
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom tidyr tibble
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes annotation_logticks coord_cartesian element_text geom_path ggplot labs scale_x_continuous scale_y_continuous theme theme_bw unit
#' @importFrom purrr map_df
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{plot_traj()}
#'
plot_traj = function(density_init = c(100,200,500,1000,2000,5000),
                     coeff_traj = c(a = 13.532,
                                    b = -1.461,
                                    a0 = 14.21,
                                    b0 = -1.79),
                     scale = c(xmin = 15,
                               xmax = 500,
                               ymin = 50,
                               ymax = 5000),
                     ...,
                     remove_theme = FALSE){

   # BINDING VARIABLES
   cg <- nha <- NULL

   # RANGE OF CALCULATION
   cg_range = scale["xmin"]:scale["xmax"]

   # CALCUL DES TRAJECTOIRES POUR CHAQUE DENSITE
   traj = purrr::map_df(.x = density_init,
                        .f = ~ dplyr::tibble(cg = cg_range) %>%
                           dplyr::mutate(nha = calculate_traj(cg,.x,coeff_traj),
                                         density_init = as.factor(.x)))

   # CREATION DU GRAPHIQUE
   breaks = c(10^(-10:10),2*10^(-10:10),5*10^(-10:10))
   minor_breaks = rep(1:9, 21)*(10^rep(-10:10, each=9))

   res = ggplot()+
      geom_path(data = traj,
                aes(x = cg, y = nha, group = density_init),
                ...)+
      coord_cartesian(xlim = c(scale["xmin"],scale["xmax"]),
                      ylim = c(scale["ymin"],scale["ymax"]))+
      scale_x_continuous(trans = "log10",
                         breaks = breaks,
                         minor_breaks = minor_breaks)+
      scale_y_continuous(trans = "log10",
                         breaks = breaks,
                         minor_breaks = minor_breaks)+
      annotation_logticks()

   if (remove_theme == FALSE){
      res = res +
         labs(title = "Diagramme de gestion de la densite",
              x = "Cg [cm]",
              y = "Nha [tiges/ha]",
              subtitle = "Donnees issues de SimCop")+
         theme_bw() +
         theme(axis.text = element_text(size = 12),
               axis.title = element_text(size = 12),
               plot.title = element_text(hjust = 0.5, size = 20),
               plot.subtitle = element_text(hjust = 0.5),
               legend.position = "bottom",
               legend.key.size = unit(1,'cm'),
               legend.text = element_text(size = 12),
               legend.title = element_text(size = 12))
   }

   # RENVOI DES RESULTATS
   return(res)
}

