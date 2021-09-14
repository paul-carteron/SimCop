#' stand_data_example
#'
#' A dataset containing 180 simulations from SimCop corresponding to :
#' * Initial densities stem/ha : 5102, 2500, 1111, 625, 277, 100
#' * Fertility : 40, 33, 26 (dominant height at 50 year)
#' * 10 repetitions are made for each pair of modalities
#'
#' @format A data frame with 19078 rows and 24 variables:
#' \describe{
#'   \item{parameters_id}{unique id for each density-fertility couple i.e. 12*3 = 36 id}
#'   \item{repetitions}{number id of the actual repetitions between 0 and 15 for this dataset}
#'   \item{rotations}{rotation time of the forest stand}
#'   \item{stand_age}{age of the stand during simulation}
#'   \item{Nha}{number of stem per hectare}
#'   \item{Gha}{basal area in square meters per hectare}
#'   \item{Vha}{volmue of alive trees in cubic meters per hectare}
#'   \item{Hdom}{average height of the hightest trees in the stand (100 hightest) in meters}
#'   \item{Hg}{average height of trees in the stand in meters}
#'   \item{Cdom}{quadratic mean circonference of the largest trees in the stand (100 largest) in centimeters}
#'   \item{Cg}{quadratic mean circonference of trees in the stand in centimeters}
#'   \item{Vha_dead}{volume of dead trees in cubic meters per hectare per years}
#'   \item{Dg}{quadratic mean diameter of trees in the stand in centimeters}
#'   \item{Ddom}{quadratic mean diameter of the largest trees in the stand (100 largest) in centimeters}
#'   \item{Total_crown_cover_ha}{sum of all crowns area in square meters}
#'   \item{Out_of_cover_crown_cover_ha}{sum of all crowns area out of cover in square meters}
#'   \item{Nha_thinned}{number of thinned tree per hectare}
#'   \item{Vha_thinned}{volume of thinned tree the year of the thinning in cubic meters per hectare}
#'   \item{Vha_Tot}{cumulative volume of alive, thinned and dead trees in cubic meters per hectare}
#'   \item{Vha_mean}{average volume in cubic meters per tree}
#'   \item{plotDimXM}{dimension of the plot for the simulation in meters}
#'   \item{horStemSpacingM}{Spacing between stem in meters}
#'   \item{fertility}{stand height at 50m in meters}
#'   \item{density}{initial density of the stand in stem per hectare}
#' }
"stand_data_example"
