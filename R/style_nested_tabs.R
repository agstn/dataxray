
#' Theme for nested tables
#'
#' @import reactablefmtr
#'
#' @return Object of class reactableTheme
#' @keywords internal
nested_tab_theme <- function(){reactableTheme(
  color = "black",
  backgroundColor = "#ffffff",
  borderWidth = "1px",
  borderColor = "#dddddd",
  stripedColor = "#dddddd",
  highlightColor = "#f0f0f0",
  cellPadding = "2px",
  tableStyle = list(
    fontFamily =  "Helvetica",
    fontSize = 12,
    borderBottom = "1px solid #dddddd"
  ),
  headerStyle = list(
    borderWidth = "1px",
    paddingTop = "2px",
    verticalAlign = "bottom",
    textAlign = "bottom",
    background = "#ffffff",
    textTransform = "uppercase",
    borderColor = "#dddddd",
    color = "#000000",
    "&:hover" = list(background = "#dddddd"),
    "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#5b5e5f", color = "#dddddd"),
    borderColor = "#333",
    fontSize = 12,
    fontFamily = "Helvetica"
  ))}

nested_extreme_theme <- function(){reactableTheme(
  color = "black",
  backgroundColor = "#ffffff",
  borderWidth = "1px",
  borderColor = "#dddddd",
  stripedColor = "#dddddd",
  highlightColor = "#f0f0f0",
  cellPadding = "2px",
  tableStyle = list(
    fontFamily =  "Helvetica",
    fontSize = 12,
    borderBottom = "1px solid #dddddd"
  ),
  headerStyle = list(
    #display = "none"
    background = "#00FF00",
    position =  "absolute",
    width    = "1px",
    height   =  '1px',
    padding  =  "0",
    margin   = '-1px',
    overflow = 'hidden',
    clip     = 'rect(0, 0, 0, 0)',
    border   = '0'
  ))}

#' Create nested table display for values
#'
#' @param values_df Dataframe of VALUES from Hmisc::describe
#'
#' @import reactable
#' @import dplyr
#'
#' @keywords internal
nested_tab_values <- function(values_df){
  bind_rows(
    values_df,
    values_df %>%
      mutate(across(everything(), ~round( .x / sum(c_across(everything())), 3)))
  ) %>%
    mutate(name = c('FREQ','PROP')) %>%
    relocate(name) %>%
    reactable( compact    = TRUE,
               bordered = TRUE,
               highlight  = TRUE,
               fullWidth  = FALSE,
               sortable   = FALSE,
               theme = nested_tab_theme(),
               defaultColDef = colDef(align = 'center',
                                      minWidth = 100),
               columns = list( name = colDef(name = ''))
    )
}

#' Create nested table display for counts
#'
#' @param counts_df Dataframe of COUNTS from Hmisc::describe
#'
#' @import reactable
#'
#' @keywords internal
nested_tab_counts <- function(counts_df){
  counts_df %>%
    reactable(compact    = TRUE,
              bordered = TRUE,
              highlight  = TRUE,
              fullWidth = FALSE,
              theme = nested_tab_theme(),
              defaultColDef = colDef(sortable = FALSE,
                                     align = 'center',
                                     minWidth  = 75,
                                     maxWidth  = 150),
              columns = list( `.05` = list(name = 'Q<sub>5</sub>',  html = TRUE),
                              `.10` = list(name = 'Q<sub>10</sub>', html = TRUE),
                              `.25` = list(name = 'Q<sub>25</sub>', html = TRUE),
                              `.50` = list(name = 'med'),
                              `.75` = list(name = 'Q<sub>75</sub>', html = TRUE),
                              `.90` = list(name = 'Q<sub>90</sub>', html = TRUE),
                              `.95` = list(name = 'Q<sub>95</sub>', html = TRUE)
              )
    )
}

#' Create nested table display for extremes
#'
#' @param extremes_df Dataframe of EXTREMES from Hmisc::describe
#'
#' @import reactable
#' @importFrom tibble enframe
#' @import dplyr
#' @import tidyr
#'
#' @keywords internal
nested_tab_extremes <- function(extremes_df){
  extremes_df %>%
    unlist %>%
    enframe(name = 'extreme', value = 'value') %>%
    separate(extreme, into = c('type','num'), sep = 1) %>%
    mutate(num = as.numeric(num),
           num = ifelse(type == 'H', max(num)+1 - num, num),
           type = factor(type, labels = c("HIGHEST","LOWEST"))) %>%
    pivot_wider(values_from = value,
                names_from = num) %>%
    reactable(compact    = TRUE,
              bordered = TRUE,
              highlight  = TRUE,
              fullWidth = FALSE,
              sortable = FALSE,
              class = "hidden-column-headers",
              theme = nested_extreme_theme(),
              defaultColDef = colDef(align = 'center',
                                     name = '')
    )
}
