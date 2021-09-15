#' plot_thinning_coefficient
#'
#' @param stand_data data.frame from the use of "import_stand_data" function AND add_ecl_data set to TRUE
#' @param remove_theme If TRUE, already created theme for the graph is remove
#'
#' @return ggplot object representing variation of K parameters
#' @export
#'
#' @importFrom dplyr group_by mutate filter summarise
#' @importFrom ggplot2 aes element_text geom_line geom_text ggplot labs theme geom_violin theme_minimal
#'
plot_thinning_coefficient = function(stand_data, remove_theme = FALSE){

   K <- label <- repetitions <- thin_parameters_id <- NULL

   # ---- Securite ----
   `%notin%` = Negate(`%in%`)

   if("ecl_name" %notin% names(stand_data)){
      stop("stand_data should be imported from \"import_stand_data\" function with add_ecl_data set to TRUE")
   }

   stand_data = stand_data %>%
      filter(K != 0 & thin_parameters_id != 0)

   mean_K = stand_data %>%
      group_by(thin_parameters_id) %>%
      summarise(mean = mean(K), max = max(K)) %>%
      mutate(label = paste0("Mean :\n", round(mean,2)))

   res = ggplot(data = stand_data,
          aes(y = K, x = as.factor(thin_parameters_id)))+
      geom_line(data = stand_data,
                aes(x = as.factor(thin_parameters_id), y = K, group = repetitions),
                alpha =0.2) +
      geom_violin(fill=NA, color="darkred", size = 1)+
      geom_text(data = mean_K,
                aes(x = as.factor(thin_parameters_id), y = max+0.06, label = label))

   if (remove_theme == FALSE){
      res = res +
         theme_minimal() +
         labs(x = "Thinning number",
           title = "Variation of thinning coefficient K") +
         theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))+
         theme(legend.position = "none")
   }

   return(res)
}
