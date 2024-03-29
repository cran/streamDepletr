apportion_inverse <- function(reach_dist, w, max_dist = Inf, min_frac = 0, reach_name = NULL, dist_name = NULL) {
  #' Distribute streamflow depletion within a stream network using inverse distance weighting.
  #'
  #' @param reach_dist data frame with two columns: \code{reach}, which is a grouping variable with
  #' the name of each stream reach, and \code{dist} which is the distance of that stream reach to
  #' the well of interest. There can be more than one \code{dist} per \code{reach}; the function
  #' will automatically find the minimum. Columns can either be named exactly as defined here, or
  #' set using \code{reach_name} and \code{dist_name}.

  #' @param w inverse distance weighting factor; 1 for inverse distance, 2 for inverse distance squared.
  #' @param max_dist the maximum distance of a stream to be depleted; defaults to \code{Inf}, which means all reaches will be considered.
  #' @param min_frac the minimum \code{frac_depletion} to be returned; defaults to \code{0}, which means all reaches will be considered.
  #' If \code{min_frac > 0} and some reaches have an estimated \code{frac_depletion < min_frac}, depletion in those reaches will be set to 0
  #' and that depletion will be reallocated based on the proportional depletion in the remaining reaches.
  #' @param reach_name The name of the column in \code{reach_dist} indicating your stream reach grouping variable. If set to \code{NULL} (default), it will assume that the column name is \code{reach}.
  #' @param dist_name The name of the column in \code{reach_dist} indicating your distance variable. If set to \code{NULL} (default), it will assume that the column name is \code{dist}.
  #' @details Since analytical models assume the presence of 1 (or sometimes 2) linear streams, the \code{apportion_*} functions
  #' can be used to distribute that depletion to various reaches within a real stream network. These geometric functions are described
  #' in Zipper et al (2018), which found that \code{apportion_web} a weighting factor (\code{w}) of 2 produced the best results.
  #' @return A data frame with two columns:
  #' \describe{
  #'   \item{reach}{the grouping variable input in \code{reach_dist}}
  #'   \item{frac_depletion}{the proportion of streamflow depletion from the well occurring in that reach.}
  #' }
  #' @references
  #' Zipper, SC, T Dallemagne, T Gleeson, TC Boerman, A Hartmann (2018). Groundwater Pumping Impacts
  #' on Real Stream Networks: Testing the Performance of Simple Management Tools. Water Resources Research.
  #' doi:10.1029/2018WR022707.
  #' @examples
  #' reach_dist <- data.frame(reach = seq(1,5), dist = c(100, 150, 900, 300, 200))
  #' apportion_inverse(reach_dist, w = 2)
  #' apportion_inverse(reach_dist, w = 2, max_dist = 500)
  #'
  #' reach_dist <- data.frame(reach = c("A", "A", "A", "B", "B"), dist = c(100, 150, 900, 300, 200))
  #' apportion_inverse(reach_dist, w = 1)
  #' apportion_inverse(reach_dist, w = 1, max_dist = 500)
  #' @export

  # set column names in data frame if necessary
  if (!is.null(reach_name)) names(reach_dist)[names(reach_dist) == reach_name] <- "reach"
  if (!is.null(dist_name)) names(reach_dist)[names(reach_dist) == dist_name] <- "dist"

  # apportion
  df_out <-
    reach_dist |>
    subset(dist <= max_dist) |>
    dplyr::group_by(reach) |>
    dplyr::summarize(dist_min = min(dist)) |>
    transform(frac_depletion = (1 / dist_min^w) / sum((1 / dist_min^w))) |>
    dplyr::select(reach, frac_depletion)

  # screen for depletion below min_frac
  if (min(df_out$frac_depletion) < min_frac) {
    depl_low <- sum(df_out$frac_depletion[df_out$frac_depletion < min_frac])
    df_out <- subset(df_out, frac_depletion >= min_frac)
    df_out$frac_depletion <- df_out$frac_depletion + depl_low * (df_out$frac_depletion / (1 - depl_low))
    return(df_out)
  } else {
    return(df_out)
  }
}
