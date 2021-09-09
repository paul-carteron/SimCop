#' calculate_growth
#'
#' @param stand_data data.frame from the use "import_stand_data" function
#' @param variable variable on which the growth will be calculated
#' @param growth_type should be "annual" or "average" which mean from the start of the stand
#'
#' @importFrom dplyr group_by mutate filter
#' @importFrom rlang ensym ":="
#'
#' @return add a column to data.frame with the calculated growth
#' @export
#'
calculate_growth = function(stand_data, variable, growth_type){

   repetitions <- fertility <- density <- stand_age <- lag <-  NULL

   variable = ensym(variable)
   growth_type = ensym(growth_type)

   `%notin%` <- Negate(`%in%`)

   if (as.character(growth_type) %notin% c("annual","average")){
      stop("growth_type should be \"annual\" or \"average\"")
   }

   var_name = paste0("acc_",as.character(variable),'_',as.character(growth_type))

   if(as.character(growth_type) == "annual"){
      data_acc = stand_data %>%
         mutate({{var_name}} := !!variable-lag(!!variable)) %>%
         group_by(repetitions,fertility,density) %>%
         filter(!duplicated(stand_age, fromLast = FALSE))

   }else if(as.character(growth_type) == "average"){
      data_acc = stand_data %>%
         mutate({{var_name}} := !!variable/stand_age) %>%
         filter(!duplicated(stand_age, fromLast = FALSE))
   }

   data_acc = data_acc %>%
      mutate({{var_name}} := replace(get(var_name), which(get(var_name) < 0), NA))

   return(data_acc)
}

