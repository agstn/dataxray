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
#'  report_xray(safetyData::adam_adsl, by = NULL, data_name = "ADSL", study = "CDISC_pilot", loc = getwd())
#'
#'  report_xray(safetyData::adam_adsl, by = "ARM", data_name = "ADSL", study = "CDISC_pilot", loc = getwd())
#' }
#' 
report_xray <- function(data, by = NULL, data_name, study, loc = NULL){
  
  if (is.null(loc)) {
    loc <- getwd()
  } 
  
  if(!dir.exists(loc)) stop (paste0(loc," is not a valid directory"))
  
  # if (!is.null(by)){
  #   report_template <- system.file("templates/report_xray_by.rmd", package = "dataxray")
  #   
  #   report_out <- file.path(loc, paste0(study,"_",data_name,"_xray_by_",by)) 
  #   
  #   params_in <- list(data = data,
  #                     data_name = data_name,
  #                     study = study,
  #                     by = by)
  # } else{
    
    report_template <- system.file("templates/report_xray.rmd", package = "dataxray")
    
    report_out <- file.path(loc, paste0(study,"_",data_name,"_xray")) 
    
    params_in <- list(data = data,
                      data_name = data_name,
                      study = study,
                      by = by)
    
 # }
  
  file.copy(report_template, paste0(report_out,".rmd"), overwrite = TRUE)
  
  
  rmarkdown::render(input = paste0(report_out,".rmd"), 
                    output_file = paste0(report_out,".html"),
                    params = params_in)
}