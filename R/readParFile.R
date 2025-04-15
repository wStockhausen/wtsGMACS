#'
#'@title Create a `gmacs_par` dataframe from GMACS ADMB par files.
#'
#'@description Function to create a `gmacs_par` dataframe from GMACS ADMB par files.
#'
#'@param parfn = filename of par file (or a list or vector of such)
#'
#'@returns a `gmacs_par` object (a [tibble] dataframe of class 'gmacs_par') corresponding to the par file(s).
#'
#'@details The returned tibble will have columns
#'\itemize{ 
#'  \item{item - parameter counter}
#'  \item{name - full parameter name (i.e., 'name[xx]'), or 'number of parameters','objective function','max gradient'}
#'  \item{name - parsed parameter name (i.e., 'name'), or 'number of parameters','objective function','max gradient')}
#'  \item{par_idx - parameter number_vector index (i.e., 'xx' as number)}
#'  \item{idx - parameter vector_vector index (i.e., index into parameter vector)}
#'  \item{value - parameter estimate or convergence quantity value}
#'  \item{case - model case name (or number). Note: only included if parfn is a list or vector}
#'}
#' The ADMB convergence information is given in the first three rows of the tibble, 
#' associated with the names 'number of parameters','objective function',and 'max gradient'. 
#' 
#'Note that parameters in `gmacs_par` and `gmacs_std` objects can be linked together by 
#'equating `param` and `idx` across both the two objects.
#' 
#' Note also that this is not the same format as returned by [gmr::readGMACSpar()], which 
#' contains more structured information based on the model run's dat and ctl files.
#' 
#' @importFrom tibble tibble 
#' 
#'@export
#' 
readParFile<-function(parfn=NULL){
  lstAll = list();
  if (is.list(parfn)||(length(parfn)>1)){
    parfn = as.list(parfn);
    for (i in 1:length(parfn)){
      dfr = readParFile(parfn[[i]]);
      if (!is.null(dfr)){
        if (!is.null(names(parfn)[i])) {
          dfr$case = names(parfn)[i];
        } else {
          dfr$case = paste("case",i);
        }
        lstAll[[i]] = dfr;
      }
      rm (dfr);
    }
    dfrAll = dplyr::bind_rows(lstAll);
    return(dfrAll);
  } else {
    if (!file.exists(parfn)) {
      warning(paste0("Par file '",parfn,"' does not exist. Returning NULL."));
      return(NULL);
    }
      
    r1<-readLines(con=parfn);
    lst = list();
    
    #parse first line
    str <- strsplit(r1[1],'[[:blank:]]');#split based on blanks
    num <- as.numeric(str[[1]]);         #convert to numeric; non-numerics will be NAs
    objfun <- num[!is.na(num)];          #extract numeric elements (num par, obj fun, max gradient)
    names(objfun)<-c('number of parameters','objective function','max gradient');
    lst[[1]] = tibble::tibble(index=NA_real_,param=names(objfun),name=names(objfun),par_idx=NA_real_,idx=NA_real_,value=objfun);
    
    #parse remaining lines
    nr<-length(r1);
    ctr = 0;
    for (r in 1:((nr-1)/2)){
      #--testing: r = 1;
      if (stringr::str_detect(r1[2*r],stringr::fixed("["))){
        param_ = stringr::str_sub(r1[2*r],3,-2);
        str <- stringr::str_split_1(stringr::str_sub(param_,1,-2),"\\[");
        nam = str[1]; par_idx_ = as.numeric(str[2]);
      } else {
        param_ = stringr::str_sub(r1[2*r],3,-1);
        nam = param_; par_idx_ = 1;
      }
      str = stringr::str_split_1(stringr::str_trim(r1[2*r+1]),'[[:blank:]]');
      val  = as.numeric(str);
      idx_ = 1:length(val);
      ctr  = ctr + idx_;
      lst[[r+1]] = tibble::tibble(index=ctr,param=param_,name=nam,par_idx=par_idx_,idx=idx_,value=val);
      ctr = max(ctr);
    }
    dfr = dplyr::bind_rows(lst)|> dplyr::mutate(value=unname(value,force=TRUE));
    class(dfr)<-c("gmacs_par",class(dfr));
    
    return(dfr)
  }
}
# parfn="testing/example_results/gmacs.par";
# dfrPar = readParFile(list(test1=parfn,test2=parfn));
# str(dfrPar$value)


