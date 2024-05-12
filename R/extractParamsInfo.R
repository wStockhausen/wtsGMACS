#' 
#' @title Extract dataframe of parameter information from `gmacs_rep1` objects.
#' @description Function to extract a dataframe of parameter information from `gmacs_rep1` 
#' objects packaged as a `gmacs_reslst`, `gmacs_repslst`, or a single `gmacs_rep1` object. 
#' @param obj - the object from which to extract the dataframe 
#' @return a dataframe 
#' @details If results from multiple models are included in `obj`, the returned dataframe 
#' has a "case" column appended to it that identifies model cases.
#' @importFrom dplyr filter 
#' @importFrom dplyr mutate 
#' @export 
#' 
extractParamsInfo<-function(obj){
  if (inherits(obj,"gmacs_reslst")){
    dfr = extractParamsInfo(obj$repsLst);
  } else if (inherits(obj,"gmacs_repslst")){
    lst = list();
    for (case_ in names(obj)){
      message(paste0("Processing case '",case_,"'"))
      rep = obj[[case_]];
      dfr = extractParamsInfo(rep) |> 
              dplyr::mutate(case=case_);
      lst[[case_]] = dfr;
    }
    dfr = dplyr::bind_rows(lst);
  } else if (inherits(obj,"gmacs_rep1")){
    dfr = obj$`Estimated parameters` |> 
           dplyr::mutate(
             dplyr::across(!par_type,as.numeric),
             description=stringr::str_replace_all(par_type,"_"," ")
           ) |> 
           dplyr::select(id=par_count,par=est_par_cnt,est,se,phase=phz,lb,ub,status,description);
  } 
  return(dfr);
}

#dfr = extractParamsInfo(Ms);

