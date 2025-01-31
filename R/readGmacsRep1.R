#' 
#' @title Read a Gmacs gmacs.rep1 file
#' @description Read a Gmacs .rep1 file and return a list of report objects.
#' @param fn - path to gmacs.rep1 file 
#' @param verbose - flag to print diagnostic info 
#' @return a named list of class `gmacs_rep1` 
#' @details The returned object has class \code{gmacs_rep1}` and inherits from \code{list}. 
#' 
#' The function uses a counter, `iln`, in the enclosing environment. If it does not exist already,
#' it will be created and set to 1 initially. If it already exists when this function is called,
#' it will  be reset to 1 initially.
#' 
#' @examplesIf FALSE
#' # example code
#'   fn = "../../ModelRuns/TannerCrab24_01/gmacs.rep1";
#'   rep = readGmacsRep1(fn,verbose=FALSE);
#' 
#' @import stringr
#' 
#' @export
#' 
readGmacsRep1<-function(fn,verbose=FALSE){
  if (!file.exists(fn)){
    warning(paste0("File not found:\n\t",fn));
    return(NULL);
  }
  if ((!stringr::str_starts(basename(fn),"gmacs"))||
      (!stringr::str_ends(basename(fn),".rep1"))){
    warning(paste0("Filename should start with 'gmacs' and end with '.rep1' but is:\n\t",
                   basename(fn)));
    return(NULL);
  }
  lns = readLines(fn);#--read file
  nls = length(lns);
  
  out = list();
  if (!exists("iln")){
    assign("iln",1,inherits=TRUE);
  } else {
    iln<<-1;#--reset to 1
  }
  ln = stringr::str_trim(lns[iln]);
  while (iln<nls){
    while((nchar(ln)==0)||stringr::str_starts(ln,"#")){
      advance();
      ln = stringr::str_trim(lns[iln]);
    }
    splt = stringr::str_trim(stringr::str_split_1(ln,":"));
    if (isKeyWord(splt[2])){
      objname = stringr::str_remove(splt[1],"--");
      advance();
      if (splt[2]=="list"){
        out[[objname]] = parseList(lns,verbose);
      } else if (splt[2]=="vector") {
        out[[objname]] = parseVector(lns,verbose);
      } else if (splt[2]=="matrix") {
        out[[objname]] = parseMatrix(lns,verbose);
      } else if (splt[2]=="dataframe") {
        out[[objname]] = parseDataframe(lns,verbose);
      }
    } else {
      out[[splt[1]]] = parseVal(splt[2]);
    }
    advance();
    if (verbose) cat("in parseAllout",iln," ");
    if (iln<nls) {
      ln = stringr::str_trim(lns[iln]);
      if (verbose) cat(ln,"\n")
    }
  }#--while (iln<nls)
  class(out) <- c("gmacs_rep1",class(out));
  return(out);
}
