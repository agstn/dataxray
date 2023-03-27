
#' @importFrom htmltools tags
#' @keywords internal
with_tooltip <- function(value, tooltip) {
  htmltools::tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                       title = tooltip, value)
}

#' @importFrom shiny icon
#' @importFrom htmltools span
#' @keywords internal
type_indicator <- function(value = c("NUM", "CHAR", "DT/TIME")) {
  value <- match.arg(value)
  label <- switch(value, NUM = "NUM", CHAR = "CHAR", `DT/TIME` = "DT/TIME")
  # Add img role and tooltip/label for accessibility
  args <- list(role = "img", title = label)
  if (value == "NUM") {
    args <- c(args, list(shiny::icon("list-ol", "fa-2x"), style = "color: #7f7f7f; font-weight: 500"))
  } else if (value == "CHAR") {
    args <- c(args, list(shiny::icon("font",    "fa-2x"), style = "color: #7f7f7f; font-weight: 500"))
  } else if (value == "DT/TIME") {
    args <- c(args, list(shiny::icon("calendar","fa-2x"), style = "color: #7f7f7f; font-weight: 500"))
  } else {
    args <- c(args, list(shiny::icon("circle"),           style = "color: #7f7f7f; font-weight: 500"))
  }
  do.call(htmltools::span, args)
}



#' Create interactive table using Hmisc::describe + reactable
#'
#' @param data_xray Output of `make_xray()`
#' @param data_xray_shared [Optional] `data_xray` converted to a `SharedData` object using crosstalk, for use with linked widgets.
#' @param elementId Unique element ID for the table
#' @param by Optional name of group by variable as character string
#'
#' @import reactable
#' @importFrom reactablefmtr fivethirtyeight
#' @importFrom purrr map
#' @import htmltools
#'
#' @return Reactable display
#' @export
#'
#' @examples
#'
#' diamonds <- ggplot2::diamonds
#' 
#' diamonds %>% 
#'  make_xray() %>% 
#'  view_xray()
#'
#' diamonds %>% 
#'  make_xray(by = 'cut') %>% 
#'  view_xray(by = 'cut')
#'
view_xray <- function(data_xray, data_xray_shared = NULL, by = NULL, elementId = NULL){

  stopifnot(is.data.frame(data_xray))

  if (!is.null(data_xray_shared)){
    stopifnot(is.SharedData(data_xray_shared))
  }

  if (is.null(data_xray_shared)){
    data_xray_shared <- data_xray
  }

  if (is.null(elementId)){
    elementId <- "describer-table-1"
  }

  tbl <- reactable(
    data_xray_shared,
    elementId = elementId,
    groupBy = by,
    searchable = FALSE,
    pagination = FALSE,
    compact = TRUE,
    highlight = TRUE,
    fullWidth = TRUE,
    defaultExpanded = FALSE,
    # height = 850,
    theme = reactablefmtr::fivethirtyeight(),

    defaultColDef = colDef(vAlign = 'top',
                           sortable = FALSE,
                           width  = 275),

    columnGroups = list(
      colGroup(name = 'Variable',
               columns = c('ORDER','TYPE','LABEL')),
      colGroup(name = 'Completeness',
               columns = c('n', 'missing', 'distinct')),
      colGroup(name = 'Interactive Figure',
               columns = c('spike_hist'))
    ),

    columns = list(

      ORDER = colDef(name = 'No',
                     header = with_tooltip('No', 'Order of Variable in data'),
                     width = 45,
                     sortable = TRUE,
                     align = 'center'),
      
      TYPE  = colDef(name = 'TYPE',
                     header = with_tooltip('TYPE', 'Variable Type (Character, Date/Time, Numeric'),
                     html = TRUE,
                     width = 75,
                     sortable = TRUE,
                     align = 'center',
                     cell = function(value) type_indicator(value)),

      VAR = colDef(show = TRUE,
                   name = '',
                   sortable = F, 
                   width = 0,
                   cell = function(){''}),
      
      LABEL = colDef(
        name = 'Name - Label',
        header = with_tooltip('Name - Label', 'Variable Name (Labels, Formats & Units if present'),
        width = 275,
        sortable = TRUE,
        style = list(borderRight = "1px solid #eee"),
        cell = function(value, index) {
          htmltools::tagList(
            htmltools::div(style = list(fontWeight= 'bold'), data_xray$VAR[index]),
            htmltools::div(style = list(fontSize = 12, color = "#999"), ifelse(!is.na(value), value, '')),
            htmltools::div(style = list(fontSize = 12, color = "#999"), ifelse(!is.na(data_xray$FORMAT[index]), paste("fmt:",data_xray$FORMAT[index]), '')),
          )
        }),

      FORMAT = colDef(show = FALSE),

      UNITS = colDef(show = FALSE),

      n = colDef(name = 'Observed',
                 width = 150,
                 sortable = TRUE,
                 style = list(fontFamily = "monospace", whiteSpace = "pre"),
                 cell = data_bars(data_xray,
                                  max_value = max(data_xray$n),
                                  text_size = 14,
                                  text_position = 'outside-base',
                                  fill_color = '#2780e3',
                                  fill_opacity = 0.5,
                                  box_shadow = TRUE, 
                                  background = ifelse(min(data_xray$n)==max(data_xray$n),
                                                      "#2780E380",
                                                      'lightgrey'))
      ),

      missing = colDef(name = '<small>Missing</small>',
                       html = TRUE,
                       width = 75,
                       sortable = TRUE,
                       align = 'right',
                       cell = function(value, index){
                         paste0(value, "<br><small>", round(100*value/(value + data_xray$n[[index]]),0), "%</small>")
                       }),

      distinct = colDef(name = '<small>Distinct</small>',
                        html = TRUE,
                        width = 75,
                        sortable = TRUE,
                        align = 'right'),


      spike_hist = colDef(name = '<small>
                           <span style="color:#000000;">Mean</sub>&#8231;</span> &nbsp;
                           <span style="color:#FF0000;">Q<sub>0.05</sub>&mid;</span>
                           <span style="color:#0000FF;">Q<sub>0.25</sub>&mid;</span>
                           <span style="color:#000000;">Median</sub>&mid;</span>
                           <span style="color:#0000FF;">Q<sub>0.75</sub>&mid;</span>
                           <span style="color:#FF0000;">Q<sub>0.95</sub>&mid;</span>
                           </small>',
                          html = TRUE,
                          footer = '<small>
                           <span style="color:#000000;">Mean</sub>&#8231;</span> &nbsp;
                           <span style="color:#FF0000;">Q<sub>0.05</sub>&mid;</span>
                           <span style="color:#0000FF;">Q<sub>0.25</sub>&mid;</span>
                           <span style="color:#000000;">Median</sub>&mid;</span>
                           <span style="color:#0000FF;">Q<sub>0.75</sub>&mid;</span>
                           <span style="color:#FF0000;">Q<sub>0.95</sub>&mid;</span>
                           </small>',
                          footerStyle = "font-weight: bold; font-size: 12px; text-transform: uppercase;",
                          cell  = function(x){return(htmltools::div(x))},
                          align = 'center',
                          width  = 275),
      
      counts_df = colDef(show = FALSE),
      
      values_df = colDef(show = FALSE),
      
      extremes_df = colDef(show = FALSE)
    ),
    details = function(index){
      cols <- c("counts","values","extremes")

      d <- data_xray
      create_div <- function(data, index, col){
        d_col <- data[[paste0(col, "_df")]][[index]]
        table_fun <- match.fun(paste0("nested_tab_", col))
        if (!is.null(d_col) && nrow(d_col)>0){
          if ((col=="values" & ncol(d_col)<=12) | (col=="counts" & ncol(d_col)>1) | col=="extremes"){
            htmltools::div(style = "padding: 2px 25px 2px 0px;",table_fun(d_col))
          }
        }
      }
      divs <- map(cols, ~create_div(data=d, index = index, col = .x))
      htmltools::div(divs)
    }
  ) #%>% 
    #reactablefmtr::google_font("Roboto")

  tagList(
      div(
        style = "display: grid; grid-template-columns: 1fr 2fr; grid-gap: 20px; margin-bottom: 12px",
        div(
          tags$button(
            "Expand/collapse all",
            onclick = paste0("Reactable.toggleAllRowsExpanded('", elementId, "')")
          )
        ),
        div(
          tags$input(
            type = "text",
            placeholder = "Search NAME | LABEL",
            style = "padding: 4px 8px; width: 50%",
            oninput = paste0("Reactable.setSearch('", elementId, "', this.value)")
          )
        )
      ),
      tbl
    ) %>%
    htmltools::browsable()

}
