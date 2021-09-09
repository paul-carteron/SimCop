#' import_stand_data
#'
#' @param filepath filepath of the output folder from SimCop. You need at least "stand_data.csv" and "parameter_scenarii.csv"
#'
#' @return data.frame of stand informations
#' @export
#'
#' @importFrom vroom vroom
#' @importFrom dplyr group_by mutate ungroup left_join rename filter
#' @importFrom purrr map_df
#'
import_stand_data = function(filepath){

   Nha <- Vha <- Vha_dead <- douglas.hDom50 <- horStemSpacingM <- parameters_id <- repetitions <- NULL
   fertility <- density <- stand_age <- NULL

   rawdata = vroom(file = file.path(filepath,"stand_data.csv"),
                   col_select = base::c("parameters_id","repetitions","rotations","stand_age",
                                        "Nha","Gha","Vha","Hdom","Hg","Cdom","Cg","Vha_dead","Dg","Ddom",
                                        "Total_crown_cover_ha","Out_of_cover_crown_cover_ha"),
                   col_types = c(.default = "d")) %>%
      group_by(parameters_id,repetitions) %>%
      mutate(Vha_dead = cumsum(Vha_dead)) %>%
      ungroup() %>%
      mutate(Vha_mean = Vha/Nha,
             Vha_Tot = Vha + Vha_dead)

   parameters = vroom(file = file.path(filepath,"parameter_scenarii.csv"),
                      col_select = base::c("parameters_id","plotDimXM","horStemSpacingM",
                                           "douglas.hDom50"),
                      col_types = c(.default = "d")) %>%
      mutate(density = as.integer(10000/horStemSpacingM^2))

   stand_data = left_join(rawdata,parameters, by = "parameters_id") %>%
      map_df(~ base::as.double(.)) %>%
      rename(fertility = douglas.hDom50)

   return(stand_data)
}
