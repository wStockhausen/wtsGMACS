#'
#' @title Extract selectivity/retention functions from a gmacs and a tcsam02 model run
#' @description Funciton to extract selectivity/retention functions from a gmacs and a tcsam02 model run.
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
extractSelFcns<-function(resGMACS,resTCSAM02=NULL){
  rep = resGMACS[[1]]$rep;
  zBs = rep$size_midpoints;
  nZBs = length(zBs);
  #--get gmacs sel functions
  dfrSel = rep$selfcns |>
             tidyr::pivot_longer(cols=4+(1:nZBs),names_to="z",values_to="val") |>
             dplyr::rename(y=year,x=sex) |>
             dplyr::mutate(dplyr::across(c(2,5,6),as.numeric),
                           case="gmacs") |>
             dplyr::filter(type!="discard") |>
             rCompTCMs::getMDFR.CanonicalFormat();
  if (!is.null(resTCSAM02)){
    dfrSelt = dplyr::bind_rows(
                rCompTCMs::extractMDFR.Fisheries.RetFcns(resTCSAM02) |> dplyr::mutate(type="retained"),
                rCompTCMs::extractMDFR.Fisheries.SelFcns(resTCSAM02) |> dplyr::mutate(type="capture"),
                rCompTCMs::extractMDFR.Surveys.SelFcns(resTCSAM02) |> dplyr::mutate(type="capture")
              ) |> dplyr::mutate((dplyr::across(c(y,z,val),as.numeric))) |>
              dplyr::filter(!stringr::str_starts(fleet,"SBS NMFS")) |>
              dplyr::mutate(m=ifelse(m=="all maturity","undetermined",m),
                            fleet=ifelse(stringr::str_starts(fleet,"NMFS"),"NMFS",fleet),
                            fleet=ifelse(stringr::str_starts(fleet,"SBS BSFRF"),"BSFRF",fleet),
                            fleet=ifelse(stringr::str_starts(fleet,"GF All"),"GFA",fleet));
    dfrSel = dplyr::bind_rows(dfrSel,dfrSelt);
  }
  return(dfrSel);
}
