#' Create X-ray report
#'
#' @param data A data frame.
#' @param by  Optional name of grouping ("by") variable as character string.
#' @param data_name Name of dataset to be displayed in report as character string.
#' @param study Name of study to be displayed in report as character string.
#' @param loc Directory to save the rmd and html output. Defaults to current working directory.
#'
#' @return
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' diamonds <- ggplot2::diamonds %>% 
#'  mutate(price = structure(price, label = 'price in US dollars'),
#'         carat = structure(carat, label = 'weight of the diamond'),
#'         cut = structure(cut, label = 'quality of the cut (Fair, Good, Very Good, Premium, Ideal)'),
#'         color = structure(color, label = 'diamond colour, from D (best) to J (worst)'),
#'         clarity = structure(clarity, label = 'a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))'),
#'         x = structure(x, label = 'length in mm'),
#'         y = structure(y, label = 'width in mm'),
#'         z = structure(z, label = 'depth in mm'),
#'         depth = structure(depth, label = 'total depth percentage = z / mean(x, y) = 2 * z / (x + y)'),
#'         table = structure(table, label = 'width of top of diamond relative to widest point'))
#'         
#' diamonds %>% 
#'  report_xray(data_name = 'Diamonds', study = 'ggplot2', loc = getwd())
#'  
#' diamonds %>% 
#'  report_xray(data_name = 'Diamonds', by = 'cut', study = 'ggplot2', loc = getwd())
#'
#' }
#' 
report_xray <- function(data, by = NULL, data_name, study, loc = NULL){
  
  if (is.null(loc)) {
    loc <- getwd()
  } 
  
  if(!dir.exists(loc)) stop (paste0(loc," is not a valid directory"))
    
    report_template <- system.file("templates/report_xray.rmd", package = "dataxray")
    
    report_out <- file.path(loc, paste0(study,"_",data_name,"_xray")) 
    
    params_in <- list(data = data,
                      data_name = data_name,
                      study = study,
                      by = by)
  
  file.copy(report_template, paste0(report_out,".rmd"), overwrite = TRUE)
  
  
  rmarkdown::render(input = paste0(report_out,".rmd"), 
                    output_file = paste0(report_out,".html"),
                    params = params_in)
}