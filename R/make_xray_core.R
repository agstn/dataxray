#' Make data xray - core function
#'
#' Create comprehensive tibble of variable metadata using Hmisc::describe as engine.
#'
#' @param data A data frame.
#'
#' @importFrom Hmisc describe
#' @import dplyr
#' @import tidyr
#' @import forcats
#' @import purrr
#'
#' @return A tibble containing variable metadata.
#' @export
#'
#'
#' @examples
#'
#' diamonds <- ggplot2::diamonds
#' make_xray_core(diamonds)
#'
make_xray_core <- function(data){

  stopifnot(is.data.frame(data))

  data_nest <- data %>%
    as.list %>%
    tibble(VAR = names(.),
           x = .) %>%
    mutate(ORDER = row_number()) %>%
    rowwise %>%
    mutate(attributes = list(attributes(x) %>%
                               magrittr::extract(c("label","units","format.sas")) %>%
                               discard(., ~ is.null(.x)) %>%
                               as_tibble %>%
                               bind_rows(
                                 tibble(label = character(),
                                        units = character(),
                                        `format.sas` = character())
                               ) %>%
                               unique %>%
                               rename(LABEL = label, UNITS = units, FORMAT = `format.sas`))) %>%
    unnest(cols=attributes, keep_empty = TRUE) %>%
    rowwise %>%
    mutate(
      TYPE = case_when(
        is.character(x) | is.factor(x) | is.logical(x) ~ "CHAR",
        lubridate::is.Date(x) | lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x) | hms::is_hms(x) ~ "DT/TIME",
        is.numeric(x) ~ "NUM"
      ),
      describe = Hmisc::describe(x, digits = 3, exclude.missing = FALSE) %>% list(),
      counts   = describe$counts %>% list(),
      values   = describe$values %>% list(),
      extremes = describe$extremes %>% list()) %>%
    mutate( n        = counts %>% purrr::pluck('n')                       %>% as.numeric(),
            missing  = counts %>% purrr::pluck('missing',  .default = NA) %>% as.numeric(),
            distinct = counts %>% purrr::pluck('distinct', .default = NA) %>% as.numeric() )%>%
    ungroup %>%
    mutate(values =  map2(values, x, ~ if(is.null(.x)) {
      tab <- table(.y)
      if (is.numeric(.y)){
        list(value = names(tab) %>% as.numeric,
             frequency = tab %>% as.vector)
      } else{
        list(value = names(tab),
             frequency = tab %>% as.vector) 
      }
      } else {
        .x } ))%>%
    rowwise %>%
    mutate(spike_hist = list(create_hist(x, counts, values))) %>%
    select(-x)


  data_nest_describe <- data_nest %>%
    arrange(ORDER) %>%
    mutate( counts_df = tryCatch(
      counts[-c(1:2)] %>%
        enframe(name = 'statistic', value = 'value') %>%
        pivot_wider(values_from = value,
                    names_from = statistic) %>%
        list(),
      error = function(e) NULL %>% list()
    ),

    extremes_df = tryCatch(
      extremes %>%
        enframe(name = 'extreme', value = 'value') %>%
        pivot_wider(values_from = value,
                    names_from = extreme) %>%
        list(),
      error = function(e) NULL %>% list()
    ),

    values_df = tryCatch(
      values %>%
        data.frame() %>%
        arrange(desc(frequency)) %>%
        pivot_wider(values_from = frequency,
                    names_from = value) %>%
        list(),
      error = function(e) NULL %>% list()
    )
    )

  return(data_nest_describe%>%
           select(ORDER, TYPE, VAR, LABEL, FORMAT, UNITS, n, missing, distinct,
                  counts_df, values_df, extremes_df,
                  spike_hist) )
}

