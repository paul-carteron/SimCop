#' add_isolines
#'
#' @param stand_data data.frame correponding to the "stand_data.csv" output of SimCop
#' @param variable one variable from the stand_data (ex : Hdom, Gha, ...)
#' @param iso_values a vector of isoline values (ex : c(10,20,30,40,50))
#' @param label_position choose the position of the label with "top", "bottom", "none"
#' @param nudge_label more accurate positionning of labels by offsetting them in x and y direction (ex : -0.1,0.2)
#' @param show_point if TRUE point used to find isolines are shown
#' @param ... other argument passed on to layer (ex : color, linetype, ...)
#'
#' @return ggplot object with isolines
#' @export
#'
#' @importFrom rlang ensym
#' @importFrom dplyr arrange desc group_by mutate pull slice ungroup
#' @importFrom ggplot2 aes geom_point stat_smooth
#' @importFrom ggrepel geom_text_repel
#' @importFrom purrr map_df
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{add_isolines()}
#'
add_isolines = function(stand_data,
                  variable = Hdom,
                  iso_values = c(26,33,40),
                  label_position = "top",
                  nudge_label = c(x = 0, y = 0),
                  show_point = FALSE,
                  ...) {

   density <- repetitions <- fertility <- Nha <- Cg <- iso_value <- Hdom <- NULL

   variable <- ensym(variable)

   extract_iso_value <- function(stand_data, variable, iso_value) {

      if (as.character(variable) %in% colnames(stand_data)) {
         variable <- ensym(variable)
         tolerance = pull(stand_data,!!variable) %>% max() / 100

         reduce_stand_data <-
            stand_data[stand_data[variable] < iso_value + tolerance &
                         stand_data[variable] > iso_value - tolerance, ]

         if (nrow(reduce_stand_data) == 0) {
            stop(paste(
                  "Pas de donnees simulees pour",
                  variable,
                  "=",
                  iso_value,
                  "+/-",
                  round(tolerance, 2)
               )
            )
         }

         iso_value_extraction = reduce_stand_data %>%
            group_by(density, repetitions, fertility) %>%
            slice(which.min(abs(!!variable - iso_value)))

         return(iso_value_extraction)
      }else{
         stop(paste0("La variable \"", as.character(variable), "\" n'existe pas."))
      }
   }

   extracted_iso_values = map_df(
      .x = iso_values,
      .f = ~ extract_iso_value(stand_data, variable, .x) %>%
         mutate(iso_value = as.factor(.x))
   )

   # ---- gestion des labels ----
   if (label_position == "top") {
      label_coord = extracted_iso_values %>%
         arrange(desc(Nha),Cg) %>%
         group_by(iso_value) %>%
         slice(1) %>%
         ungroup()
   }else if (label_position == "bottom") {
      label_coord = extracted_iso_values %>%
         arrange(Nha,desc(Cg)) %>%
         group_by(iso_value) %>%
         slice(1) %>%
         ungroup()
   }else{
      label_coord = data.frame(Cg = NA, Nha = NA, iso_value = NA)
   }

   # ---- graphique ----
   res = list(stat_smooth(data = extracted_iso_values,
                          geom = "line",
                          aes(x = Cg, y = Nha, group = iso_value),
                          se = FALSE,
                          method = "loess",
                          span = 2,
                          orientation = "y",
                          ...),
              geom_text_repel(
                 data = label_coord,
                 aes(x = Cg, y = Nha, label = iso_value),
                 segment.color = NA,
                 direction = "y",
                 nudge_x = nudge_label[1],
                 nudge_y = nudge_label[2],
                 ...
              ))

   # ---- options ----
   if(show_point == TRUE){
      res = c(res,
              geom_point(data = extracted_iso_values,
                         aes(x = Cg, y = Nha, group = interaction(iso_value,fertility),
                             color = as.factor(fertility))))
   }

   return(res)
}

