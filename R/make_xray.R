#' Make data xray by a grouping variable
#'
#' Create comprehensive tibble of variable metadata using Hmisc::describe as engine, with option for grouping
#'
#' @param data A data frame.
#' @param by Optional name of grouping ("by") variable as character string.
#'
#' @importFrom Hmisc describe
#' @import dplyr
#'
#' @return A tibble containing variable metadata with 1 row per group.
#' @export
#'
#'
#' @examples
#'
#' diamonds <- ggplot2::diamonds
#' make_xray(diamonds)
#'
#' make_xray(diamonds, by = 'cut')
#'
make_xray <- function(data, by = NULL){

  if (!is.null(by)){
    data %>%
      nest_by(.data[[by]]) %>%
      mutate(data_xray_result = list(make_xray_core(data))) %>%
      select(-data) %>%
      unnest(cols = data_xray_result) %>% 
      ungroup
  } else {
    make_xray_core(data)
  }

}
