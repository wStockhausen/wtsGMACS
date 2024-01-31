#' 
#' @title GMACS constants
#' @description Get lists of GMACS constants 
#' @return list with constants by category
#' @export
#' 
getConstants<-function(){
  lst = list();
  lst[["xs"]] = list("undetermined"=0,"all"=0,"male"=1,"female"=2);
  lst[["ms"]] = list("undetermined"=0,"all"=0,"immature"=1,"mature"=2)
  lst[["ss"]] = list("undetermined"=0,"all"=0,"new shell"=1,"old shell"=2)
  lst[["unitsTypes"]]=list(biomass=1,abundance=2);
  lst[["catchTypes"]]=list(total=0,retained=1,disscard=2);
  return(lst);
}