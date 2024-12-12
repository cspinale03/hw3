#' @description
#' Calculate uncertainity interval width
#'
#' @param est: tibble which contains mCPR estimates. Columns: “Country or area”, iso, Year, Median, U95, L95
#' @param iso_code: country iso code
#' @param coverage confidence intervals to be plotted. Options can be: 80, 95, or NA (no CI plotted)
#'
#' @return
#' tibble with two columns year and interval width
#' @export
#'
#' @examples
#' get_width_ci(est, iso_code = 4, coverage = 95)
#' get_width_ci(est, iso_code = 404, coverage = 80)

get_width_ci <- function(est, iso_code, coverage = 95) {

  est2 <- est %>%
    filter(iso == iso_code)


  if (coverage == 80) {
    est2 <- est2 %>%
      mutate(Width = U80 - L80)
  } else if (coverage == 95) {
    est2 <- est2 %>%
      mutate(Width = U95 - L95)
  } else {
    stop("Use 80 or 95 for coverage.")
  }

  return(est2 %>% select(Year, Width))
}
