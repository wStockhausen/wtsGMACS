#' 
#' @title Extract dataframe of parameters-at-bounds from a gmacs object.
#' @description Function to extract a dataframe of parameters-at-bounds from a 
#' `gmacs_reslst`, `gmacs_repslst`, or `gmacs_rep1` object. 
#' @param obj - the object from which to extract the dataframe 
#' @return a dataframe 
#' @details The returned dataframe 
#' @importFrom dplyr filter 
#' @importFrom dplyr mutate 
#' @export 
#' 
extractParamsAtBounds<-function(obj){
  if (inherits(obj,"gmacs_reslst")){
    dfr = extractParamsAtBounds(obj$repsLst);
  } else if (inherits(obj,"gmacs_repslst")){
    lst = list();
    for (case_ in names(obj)){
      message(paste0("Processing case '",case_,"'"))
      rep = obj[[case_]];
      dfr = extractParamsAtBounds(rep) |> 
              dplyr::mutate(case=case_);
      lst[[case_]] = dfr;
    }
    dfr = dplyr::bind_rows(lst);
  } else if (inherits(obj,"gmacs_rep1")){
    dfr = obj$`Estimated parameters` |> 
            dplyr::filter(abs(as.numeric(status))==1);
  } 
  return(dfr);
}

#dfr = extractParamsAtBounds(Ms);

