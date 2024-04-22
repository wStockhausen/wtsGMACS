#' 
#' @title Extract an object from `gmacs_rep1` objects. 
#' @description Function to extract an object from `gmacs_rep1` objects. 
#' @param obj - a gmacs_reslst, gmacs_repslst, or gmacs_rep1 object 
#' @param objname - name of object to extract 
#' @returns If the object to extract is a dataframe, then the returned is a dataframe 
#' with an appended column 'case' indicating the 
#' originating model case. Otherwise a list of extracted objects named by case.
#' @details If the object to extract is a dataframe, then the returned is a dataframe 
#' with an appended column 'case' indicating the 
#' originating model case. Otherwise a list of extracted objects named by case.
#' 
#' @importFrom dplyr bind_rows 
#' 
#' @export 
#' 
extractRep1Results<-function(obj,
                             objname){
  if (class(obj)[1]=="gmacs_reslst"){
    #--a gmacs_reslsts object
    res = extractRep1Results(obj$repsLst,objname);
    return(res);
  }
  if (class(obj)[1]=="gmacs_repslst"){
    #--a list of gmacs_rep1 objects
    reslst = list();
    cases = names(obj);
    dfrm = TRUE;
    for (case in cases){
      message(paste0("Processing list case '",case,"' for '",objname,"'."));
      res = extractRep1Results(obj[[case]],objname);
      dfrm = dfrm && inherits(res,"data.frame");
      if (inherits(res,"data.frame")) {
        res$case = case;
      }
      reslst[[case]] = res;
    }
    if (dfrm) reslst = dplyr::bind_rows(reslst);
    return(reslst);
  }
  if (class(obj)[1]=="gmacs_rep1"){
    res = obj[[objname]];
    return(res);
  }
}

#res = extractRep1Results(Ms,"R_y");


