#'
#' @title Extract natural mortality rates from a gmacs and a tcsam02 model run
#' @description Function to extract natural mortality rates from a gmacs and a tcsam02 model run.
#' @param resGMACS -
#' @param resTCSAM02 -
#' @return dataframe
#' @details Return a dataframe in TCSAM02 standard canonical format.
#' @import dplyr
#' @import rCompTCMs
#' @import tidyr
#' 
#' @export
#'
extractNaturalMortality<-function(resGMACS,resTCSAM02=NULL){
  rep = resGMACS[[1]]$rep;
  zBs = rep$size_midpoints;
  nZBs = length(zBs);
  #--get gmacs M rates----
  dfrM = rep$`Natural_mortality-by-class` |> chkNames() |>
                 tidyr::pivot_longer(cols=4+(1:nZBs),names_to="z",values_to="val") |>
                 dplyr::rename(y=year,x=sex,m=maturity,s=shell_cond) |>
                 dplyr::mutate(dplyr::across(c(1,5,6),as.numeric),
                               case=paste("gmacs",names(resGMACS)[1])) |>
                 rCompTCMs::getMDFR.CanonicalFormat();
  if (!is.null(resTCSAM02)){
    dfrMt = rCompTCMs::extractMDFR.Pop.NaturalMortality(resTCSAM02,type="M_yxmsz") |> 
              dplyr::mutate(z=as.numeric(z));
    if (length(unique(dfrM$x))==1) {
      cols = names(dfrMt)[!(names(dfrMt) %in% c("x","val"))];
      dfrMt = dfrMt |> dplyr::mutate(x="undetermined") |> 
                dplyr::group_by(!!!rlang::syms(cols)) |> 
                dplyr::summarize(val=mean(val)) |> 
                dplyr::ungroup() |> 
                dplyr::mutate(x="undetermined");
    }
    if (length(unique(dfrM$m))==1) {
      cols = names(dfrMt)[!(names(dfrMt) %in% c("m","val"))];
      dfrMt = dfrMt |> dplyr::mutate(m="undetermined") |> 
                dplyr::group_by(!!!rlang::syms(cols)) |> 
                dplyr::summarize(val=mean(val)) |> 
                dplyr::ungroup() |> 
                dplyr::mutate(m="undetermined");
    }
    if (length(unique(dfrM$s))==1) {
      cols = names(dfrMt)[!(names(dfrMt) %in% c("s","val"))];
      dfrMt = dfrMt |> dplyr::mutate(s="undetermined") |> 
                dplyr::group_by(!!!rlang::syms(cols)) |> 
                dplyr::summarize(val=mean(val)) |> 
                dplyr::ungroup() |> 
                dplyr::mutate(s="undetermined");
    }
    dfrM = dplyr::bind_rows(dfrM,dfrMt);
  }
  return(dfrM);
}

chkNames<-function(dfr){
  names(dfr) = tolower(names(dfr));
  if (!("year" %in% names(dfr)))       dfr$year       = "all";
  if (!("sex" %in% names(dfr)))        dfr$sex        = "undetermined";
  if (!("maturity" %in% names(dfr)))   dfr$maturity   = "undetermined";
  if (("shell" %in% names(dfr)))       names(dfr)[names(dfr)=="shell"] = "shell_cond";
  if (!("shell_cond" %in% names(dfr))) dfr$shell_cond = "undetermined";
  cols_strt = c("year","sex","maturity","shell_cond");
  cols_othr = names(dfr)[!(names(dfr) %in% cols_strt)];
  dfr = dfr |> dplyr::select(tidyr::any_of(c(cols_strt,cols_othr)));
  return(dfr);
}
