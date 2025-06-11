#'
#'@title Create a `gmacs_std` dataframe from an ADMB std file.
#'
#'@description Function to create a `gmacs_std` dataframe from an ADMB std file.
#'
#'@param stdfn - (possibly named) path/filename of std file, or a list or (possibly named) vector
#'
#'@returns a `gmacs_std` object (a [tibble] dataframe of class 'gmacs_std') corresponding to the std file(s)
#'
#'@details The returned tibble will have columns
#'\itemize{ 
#'  \item{case - model case name (taken from the list or vector element name) or number. Value is 'gmacs' if only an unnamed std file is given.}
#'  \item{index - index of the parameter value in the std file}
#'  \item{param - name of the parameter as given in the std file}
#'  \item{name - parsed parameter name, dropping any `[...]` index notation}
#'  \item{idx - the `...` within any `[...]` in the parameter name, or NA if none}
#'  \item{est - parameter estimate or convergence quantity value}
#'  \item{std - estimated (delta-method) standard error}
#'} 
#'
#'Note that parameters in `gmacs_par` and `gmacs_std` objects can be linked together by 
#'equating `param` and `idx` across both the two objects.
#' 
#' @import dplyr
#' @import stringr
#' @importFrom tibble tibble 
#' 
#'@export
#' 
readStdFile<-function(stdfn=NULL){
  lstAll = list();
  if (is.list(stdfn)||(length(stdfn)>1)){
    stdfn = as.list(stdfn);
    for (i in 1:length(stdfn)){
      dfr = readStdFile(stdfn[[i]]);
      if (!is.null(dfr)){
        if (!is.null(names(stdfn)[i])) {
          dfr$case = names(stdfn)[i];
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
      if (!file.exists(stdfn)) {
        warning(paste0("Std file '",stdfn,"' does not exist. Returning NULL."));
        return(NULL);
      }
    
      case_ = names(stdfn);
      if (is.null(case_)) case_ = "gmacs";
      
      r1<-readLines(con=stdfn);
      #parse first line, which gives column names
      colnames <- stringr::str_split_1(stringr::str_trim(r1[1]),"[:blank:]+");#split based on blanks
      if (any(colnames!=c("index","name","value","std.dev"))){
        str = paste0("'",stdfn,"' is not an ADMB std file");
        stop(str);
      }
  
      #parse remaining lines
      lst = list();
      nlns = length(r1);#--number of rows for output dataframe
      last_par = "";
      for (r in 2:nlns){
        #--testing: r = 2;
        str = stringr::str_split_1(stringr::str_trim(r1[r]),"[:blank:]+");#--split based on blanks
        param_ = str[2];
        if (param_ !=last_par) {
          idx_ = 1;
        } else {
          idx_ = idx_ + 1;
        }
        #--split param name to get name and index
        ps1 = (param_ |> stringr::str_split_1("\\["));
        name_ = ps1[1];
        par_idx = NA;
        if (length(ps1)>1) par_idx_  = (ps1[2]|> stringr::str_split_1("\\]"))[1];
        lst[[r-1]] = tibble::tibble(index=str[1],
                                    param=param_,
                                    name = name_,
                                    par_idx  = par_idx_,
                                    idx = idx_,
                                    est  =str[3],
                                    std  =str[4]);
        last_par = param_;
      }#--r
      dfr = dplyr::bind_rows(lst) |> 
              dplyr::mutate(
                dplyr::across(c(1,4:7),as.numeric)
              ) |> 
              dplyr::mutate(case=case_,.before=1);
      class(dfr)<-c("gmacs_std",class(dfr));
      
      return(dfr)
  }
}
# stdfn  = "testing/example_results/gmacs.std";
# dfrStd = readStdFile(stdfn);
# dfrStd = readStdFile(c(test05=stdfn));
# dfrStd = readStdFile(list(test25=stdfn));
# dfrStd = readStdFile(c(stdfn,stdfn));
