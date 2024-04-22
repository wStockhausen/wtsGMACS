#' 
#' @title Extract model convergence info 
#' @description Function to extract model configuration info from a 
#' gmacs_reslst or gmacs_par object. 
#' @param obj - the object to extract the info from 
#' @return dataframe 
#' @details The returned dataframe has convergence info by model 'case'.
#' @export
extractModelConvergenceInfo<-function(obj){
  if (inherits(obj,"gmacs_reslst")){
    dfr = extractModelConvergenceInfo(obj$dfrPars);
  } else if (inherits(obj,"gmacs_par")){
    names = c("number of parameters","objective function","max gradient");
    dfr = obj |> 
            dplyr::filter(name %in% names) |>
            dplyr::select(tidyselect::any_of(c("case","name","value"))) |> 
            tidyr::pivot_wider(names_from="name",values_from="value");
  } 
  return(dfr);
}
#extractModelConvergenceInfo(Ms)



