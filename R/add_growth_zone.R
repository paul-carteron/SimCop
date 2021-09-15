#' add_growth_zone
#'
#' @param stand_data data.frame from the use of "import_stand_data" function
#' @param variable variable on which the growth will be calculated
#' @param growth_type should be "annual" or "average", which mean from the start of the stand
#' @param reduction_rate numerical value between 0 and 1 to characterize the width of the zone in relation to the maximum. For example, 0.05 corresponds to a zone of 5\% around the maximum.
#' @param zone_color color of the zone
#' @param grab_force force apply to connect point together. Defaults to 2
#' @param coeff_traj Coefficient a, b, a0, and b0 of the mortality trajectory (cf : Ningre et al "Trajectoires d auto-eclaircie du Douglas en France)
#' @param show_point if TRUE point used to find isolines are shown
#'
#' @importFrom dplyr group_by left_join mutate rename select slice_max ungroup filter all_of arrange slice n
#' @importFrom rlang ensym
#' @importFrom ggplot2 aes geom_point geom_line scale_linetype_manual scale_size_manual
#' @importFrom broom augment
#' @importFrom purrr imap_dfr map
#' @importFrom ggforce geom_mark_hull
#'
#' @return a ggplot object corresponding to target zone
#' @export
#'
add_growth_zone <- function(stand_data,
                            variable = "Vha",
                            growth_type = "annual",
                            reduction_rate = 0.10,
                            zone_color = "purple",
                            grab_force = 2,
                            coeff_traj = c(a = 13.532, b = -1.461, a0 = 14.21, b0 = -1.79),
                            show_point = FALSE){

   .fitted <- Cg <- Nha <- Nha_DTDM <- density <- fertility <- id <- NULL
   `log(Cg)` <- na.omit <- name <- NULL
   parameters_id <- repetitions <- stand_age <- stand_age_max <- NULL

   variable = ensym(variable)
   growth_type = ensym(growth_type)

   var_name = paste0("acc_", as.character(variable), "_", as.character(growth_type))
   age_max = max(stand_data$stand_age)

   # calcul des accroissements ####
   growth_data = stand_data %>%
      calculate_growth(!!variable,!!growth_type) %>%
      na.omit() %>%
      mutate(id = paste(parameters_id,repetitions,sep = "."), .before = "parameters_id")

   # Trouver les accroissements max ####

   max_growth = growth_data  %>%
      group_by(density,fertility,repetitions) %>%
      slice_max(get(var_name)) %>%
      ungroup() %>%
      filter(stand_age != age_max) %>%
      rename(stand_age_max = stand_age) %>%
      rename(max_growth = all_of(var_name))

   reduction_growth_before_max = growth_data %>%
      filter(id %in% unique(max_growth$id)) %>%
      left_join(max_growth %>% select(id,stand_age_max,max_growth), by = "id") %>%
      filter(stand_age < stand_age_max) %>%
      filter(get(var_name) <= max_growth * (1 - reduction_rate)) %>%
      slice_max(get(var_name)) %>%
      ungroup()

   reduction_growth_after_max = growth_data %>%
      filter(id %in% unique(max_growth$id)) %>%
      left_join(max_growth %>% select(id,stand_age_max,max_growth), by = "id") %>%
      filter(stand_age > stand_age_max) %>%
      filter(get(var_name) <= max_growth * (1 - reduction_rate)) %>%
      slice_max(get(var_name)) %>%
      ungroup() %>%
      filter(stand_age < age_max)

   all_three_growth_data = list(max = max_growth,
                                before = reduction_growth_before_max,
                                after = reduction_growth_after_max)

   # ---- Takes into account the case where there is only one simulation ----
   if(length(unique(stand_data$density)) == 1){
      return(map(.x = all_three_growth_data,
                 .f = ~ geom_point(data = .x,
                                   aes(x = Cg, y = Nha))))
   }

   # ---- regression for each set of data ----
   zone_boundary_models = imap_dfr(.x = all_three_growth_data,
                            .f = ~ loess(log(Nha) ~ log(Cg), data = .x) %>%
                               augment() %>%
                               mutate(name = names(all_three_growth_data[.y]))) %>%
      mutate(Nha = exp(.fitted), Cg = exp(`log(Cg)`)) %>%
      mutate(Nha_DTDM = Cg^coeff_traj["b"] * exp(coeff_traj["a"])) %>%
      filter(Nha <= Nha_DTDM)

   extreme_max_point = zone_boundary_models %>%
      filter(name == "max") %>%
      arrange(Cg) %>%
      slice(c(1,n()))

   mark_hull_data = zone_boundary_models %>%
      filter(name != "max") %>%
      bind_rows(extreme_max_point)

   # ---- Preparation of ggplot object to return ----

   res = list(geom_line(data = zone_boundary_models,
                        aes(x = Cg, y = Nha, linetype = name, size = name)),
              scale_size_manual(values = c(0.5,0.5,1)),
              scale_linetype_manual(values = c("solid","solid","dashed")),
              geom_mark_hull(data = mark_hull_data,
                             aes(x = Cg, y = Nha),
                             concavity = grab_force,
                             expand = 0,
                             radius = 0,
                             fill = zone_color,
                             color = NA))

   if(show_point == TRUE){
      res = c(res,
              map(.x = all_three_growth_data,
                  .f = ~ geom_point(data = .x,
                                    aes(x = Cg, y = Nha, color = as.factor(fertility))))
      )

   }
   return(res)
}



