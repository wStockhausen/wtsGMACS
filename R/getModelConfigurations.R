#' 
#' @title Extract model configuration info 
#' @description Function to extract model configuration info from a 
#' gmacs_reslst, gmacs_repslst, or gmacs_rep1 object. 
#' @param obj - the object to extract the info from 
#' @return list of lists 
#' @details list of lists, one for each model
#' @export
getModelConfigurations<-function(obj){
  if (inherits(obj,"gmacs_reslst")){
    lst = getModelConfigurations(obj$repsLst);
  } else if (inherits(obj,"gmacs_repslst")){
    lst = list();
    for (case_ in names(obj)){
      message(paste0("Processing case '",case_,"'"))
      rep1 = obj[[case_]];
      lst1 = getModelConfigurations(rep1);
      lst[[case_]] = lst1;
    }
  } else if (inherits(obj,"gmacs_rep1")){
    lst = list();
    strs = c(stock="Stock being assessed",
             uwgt="Weight unit",
             unum="Numbers unit",
             yrng="Year_range",
             nssns="Number of seasons",
             nflts="Number_of_fleets",
             flts="Fleets",
             nSXs="Number of sexes",
             nSCs="Number of shell conditions",
             nMSs="Number of maturity states",
             nZCs="Number of size classes",
             mxZCs="Max size classes",
             zCs="size_breaks",
             zBs="size_midpoints");
    for (str in names(strs)){lst[[str]]=obj[[strs[str]]];}
    lst[["lbls"]] = strs;
  } 
  return(lst);
}
#lst = getModelConfigurations(Ms)

