#' import_stand_data
#'
#' @description Function to import and format raw data from SimCop. The function takes into account
#' the single simulation or the simulations from the script mode
#'
#' @param filepath filepath of the output folder from SimCop. You need at least "stand_data.csv"
#' and "parameter_scenarii.csv"
#'
#' @return data.frame of stand informations
#' @export
#'
#' @importFrom vroom vroom
#' @importFrom dplyr group_by mutate ungroup left_join rename filter bind_cols
#'
import_stand_data = function(filepath){

   Nha <- Vha <- Vha_dead <- douglas.hDom50 <- horStemSpacingM <- parameters_id <- repetitions <- NULL
   fertility <- density <- stand_age <- Vha_thinned <- name <- plotDimXM <- NULL

   rawdata = vroom(file = file.path(filepath,"stand_data.csv"),
                   col_select = c("parameters_id","thin_parameters_id","repetitions","rotations",
                                  "stand_age","Nha","Gha","Vha","Hdom","Hg","Cdom","Cg","Vha_dead",
                                  "Dg","Ddom","K","Nha_thinned","Vha_thinned","Gha_thinned",
                                  "Total_crown_cover_ha","Out_of_cover_crown_cover_ha"),
                   col_types = c(.default = "d"))

   # ---- Allows to take into account the use of the script mode or not ----
   if ("parameter_scenarii.csv" %in% list.files(filepath)){
      parameters = vroom(file = file.path(filepath,"parameter_scenarii.csv"),
                         col_select = c("parameters_id","plotDimXM","horStemSpacingM",
                                        "douglas.hDom50"),
                         col_types = c(.default = "d"))
      stand_data = left_join(rawdata,parameters, by = "parameters_id")

   }else{
      parameters = vroom(file = file.path(filepath,"generated_scenario.csv"),
                         col_select = c("plotDimXM","horStemSpacingM","douglas.hDom50"),
                         col_types = c(.default = "d"))
      stand_data = bind_cols(rawdata,parameters)
   }

   # ---- import of thinning data if ecl data exist----
   if (!is.na(readLines(file.path(filepath,"forestry_scenario.csv"))[2])){
      ecl = vroom(file = file.path(filepath,"forestry_scenario.csv"),
                  col_select = c("thin_parameters_id","name","criteria_trigger","criteria_value_type",
                                 "criteria_value", "nb_trees_ha", "size_class_weights", "distance_factors",
                                 "average_distance_method", "integer_bounds"),
                  col_types = c(thin_parameters_id = "i",
                                name = "c",
                                criteria_trigger = "c",
                                criteria_value_type = "c",
                                criteria_value = "d",
                                nb_trees_ha = "d",
                                size_class_weights = "c",
                                distance_factors = "c",
                                average_distance_method = "c",
                                integer_bounds = "l")) %>%
         rename(ecl_name = name)

      stand_data = left_join(stand_data, ecl, by = ("thin_parameters_id"))
   }

   # ---- calculation of new variable and renaming ----
   stand_data = stand_data  %>%
      mutate(density = as.integer(10000/horStemSpacingM^2)) %>%
      rename(fertility = douglas.hDom50, plot_dimension = plotDimXM, initial_spacing = horStemSpacingM) %>%
      group_by(parameters_id,repetitions) %>%
      mutate(Vha_Tot = Vha + cumsum(Vha_dead) + cumsum(Vha_thinned), .after = Vha_dead) %>%
      ungroup() %>%
      mutate(Vha_mean = Vha/Nha, .after = Vha_dead)

   return(stand_data)
}
