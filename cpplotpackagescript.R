library(dplyr)
library(ggplot2)

#'  @description
#'  Plot mCPR data and estimates
#'
#' @param dat: tibble which contains mCPR observations. Columns: iso, year, cp
#' @param est: tibble which contains mCPR estimates. Columns: “Country or area”, iso, Year, Median, U95, L95
#' @param iso_code: country iso code
#' @param CI: confidence intervals to be plotted. Options can be: 80, 95, or NA (no CI plotted)
#'
#' @usage plot_cp(dat, est, iso_code, CI = 95)
#'
#' @return
#' ggplot object with data and estimates
#' @export
#'
#' @examples
#' plot_cp(dat, est, iso_code = 4)
#' plot_cp(dat, est, iso_code = 4, CI = NA)
#' plot_cp(dat, est, iso_code = 404, CI = 80)
#'
#'
#'
plot_cp <- function(dat, est, iso_code, CI = 95) {

   # check dat columns for iso, year and cp
  if (!all(c("iso", "Year", "cp") %in% colnames(dat))) {
    missing_cols <- setdiff(c("iso", "Year", "cp"), colnames(dat))
    stop(paste("Input data file 'dat' must contain variable(s):", paste(missing_cols, collapse = ", ")))
  }

  # check est for iso, year, median, u95 and l95
  if (!all(c("iso", "Year", "Median", "U95", "L95") %in% colnames(est))) {
    missing_cols <- setdiff(c("iso", "Year", "Median", "U95", "L95"), colnames(est))
    stop(paste("Input estimates file 'est' must contain variable(s):", paste(missing_cols, collapse = ", ")))
  }

  # check if iso_code is in dat and iso
  if (!iso_code %in% dat$iso) {
    stop(paste("The provided iso_code:", iso_code, "is not found in 'dat'."))
  }
  if (!iso_code %in% est$iso) {
    stop(paste("The provided iso_code:", iso_code, "is not found in 'est'."))
  }

  # chekc if cp is numeric
  if (!is.numeric(dat$cp)) {
    stop("'cp' in data file 'dat' must be numeric.")
  }

  # check ci level
  if (!is.na(CI) && !(CI %in% c(80, 95))) {
    stop("CI must be one of 80, 95, or NA.")
  }

  est2 <- est %>%
    filter(iso == iso_code)

  # default na
  lower <- NA
  upper <- NA

  # set ci based on arg
  if (!is.na(CI)) {
    if (CI == 80) {
      lower <- est2$L80
      upper <- est2$U80
    } else if (CI == 95) {
      lower <- est2$L95
      upper <- est2$U95
    }
  }

  # initialize plot w/o ci
  chart <- est2 %>%
    ggplot(aes(x = Year, y = Median)) +
    geom_line() +
    labs(x = "Time", y = "Modern use (%)", title = est2$`Country or area`[1]) +
    geom_point(data = dat %>% filter(iso == iso_code), aes(x = Year, y = cp))

  # add ci if lower/upper not NA
  if (!all(is.na(lower)) && !all(is.na(upper))) {
    chart <- chart + geom_smooth(stat = "identity", aes(ymax = lower, ymin = upper))
  }

  print(chart)
}
getwd()
