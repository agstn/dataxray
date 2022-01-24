#' Get information on metrics
#'
#' @import dplyr
#' @importFrom stringr str_glue
#' @import reactable
#' @importFrom reactablefmtr fivethirtyeight
#' @import htmltools
#' @importFrom gt local_image
#'
#' @return Summary table of metrics details
#'
#' @examples
#' get_metrics_info()
#'
#' @export
get_metrics_info <- function(){

  metrics %>%
    mutate(PNG = ifelse(PNG != "",
                        system.file(file.path("images",PNG), package = "describer"),
                        system.file("images/blank_crop.png", package = "describer")))%>%
    mutate(PNG = gt::local_image(PNG, height = 130)) %>%
    reactable(
      defaultSorted = c("METRIC"),
      columns = list(
        METRIC = colDef(
          width = 125,
          cell = function(value, index) {
            htmltools::tags$a(href = .[index, "LINK"], target = "_blank", as.character(value))
          }
        ),
        PNG = colDef(name = 'visual aid',
                     sortable = FALSE,
                     html = TRUE,
                     width = 350,
                     style = list(padding = "0px 0px", margins = "0px 0px")
        ),
        LINK = colDef( show = FALSE),
        DEFINITION = colDef( width = 250,
                             sortable = FALSE)
      ),
      searchable = FALSE,
      pagination = FALSE, # all one page,
      compact = TRUE,
      highlight = TRUE,
      defaultExpanded = FALSE,
      height = 850,
      theme = reactablefmtr::fivethirtyeight()
    )
}
