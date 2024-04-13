#' 
#' @title Extract an object from a `rep1` list object. 
#' @description Function to extract an object from a `rep1` list object. 
#' @param lst - named list of rep1 objects 
#' @param objname - name of object to extract 
#' @returns If the object to extract is a dataframe, the returned is a dataframe 
#' with an appended column 'case' indicating the 
#' originating model case. Otherwise a list of extracted objects named by case.
#' 
#' @importFrom dplyr bind_rows 
#' 
#' @export 
extractRep1Results<-function(lst,
                             objname){
  objlst = list();
  if (class(lst)[1]=="list"){
    cases = names(lst);
    dfrm = TRUE;
    for (case in cases){
    message(paste0("Processing list case '",case,"' for '",objname,"'."));
      obj = extractRep1Results(lst[[case]],objname);
      dfrm = dfrm && inherits(obj,"data.frame");
      if (inherits(obj,"data.frame")) {
        obj$case = case;
      }
      objlst[[case]] = obj;
    }
    if (dfrm) objlst = dplyr::bind_rows(objlst);
    return(objlst);
  }
  if (inherits(lst,"rep1")){
    obj = lst[[objname]];
    return(obj);
  }
}

#res = extractRep1Results(lst,"R_y");


