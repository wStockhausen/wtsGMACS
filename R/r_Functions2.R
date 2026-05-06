#'
#' @title Get fits to index time series
#' @description Functio to get fits to index time series from GMACS and TCSAM02 models.
#' @param lstGs - list with named GMACS repslst objects from model runs
#' @param lstT - TCSAM02 reslst object
#' @param type - index type to extract ('biomass' or 'abundance')
#' @return dataframe 
#' @details For GMACS results, the ln-scale `sdobs` is calculated from the `cv`,
#' the `zscore` is taken from the pearson's residual (`prsn_res`), NLLs for 
#' individual observations/predictions are recalculated assuming a lognormal 
#' likelihood and the associated `sdobs` and `zscore`. the `residual` is calculated 
#' on the ln scale as $log(observed) - log(predicted)$.
#' 
#' For TCSAM02 results, the `residual` is calculated as above, the `cv` is calculated from the 
#' `sdobs`, the NLL is recalculated for individual observations consistent with that 
#' for GMACS above.
#' 
#' For all, the 90% CIs are calculated for the observations based on the CVs.
#' 
#' @export
getFitsToIndexTimeSeries<-function(lstGs,lstT=NULL,type="biomass"){
  lstGSDs = list();
  for (case_ in names(lstGs$repsLst)){
    dfrIFS = lstGs$repsLst[[case_]]$Index_fit_summary;
    lstGSDs[[case_]] = dfrIFS |> dplyr::filter(units==type) |> 
                        dplyr::select(y=year,fleet,x=sex,m=maturity,
                                      observed=obs,cv=actual_CV,predicted=prd,zscore=prsn_res) |> 
                        dplyr::mutate(dplyr::across(c(y,observed,cv,predicted,zscore),as.numeric),
                                      sdobs=sqrt(log(1+cv^2)),
                                      nll=0.5*log(2*pi*sdobs^2) + (0.5*zscore^2),
                                      residual=log(observed)-log(predicted)) |> 
                        dplyr::mutate(case=case_,.before=1);
  }
  if (!is.null(lstT)){ 
    if (type=="biomass"){
      dfrT = rTCSAM02::getMDFR.AllScores.Biomass(lstT,
                                                 fleet.type="survey",
                                                 catch.type="index");
    } else  {
      dfrT = rTCSAM02::getMDFR.AllScores.Abundance(lstT,
                                                 fleet.type="survey",
                                                 catch.type="index");
    }
    dfrT = dfrT |> 
            tidyr::pivot_wider(names_from=type,values_from=val) |> 
            dplyr::filter(observed > 0, !stringr::str_starts(.data$fleet,"SBS NMFS")) |> 
            dplyr::mutate(cv=sqrt(exp(sdobs^2)-1),
                          residual=log(observed)-log(predicted),
                          zscore=`z-score`,
                          nll=0.5*log(2*pi*sdobs^2) + (0.5*zscore^2),
                          m=ifelse(stringr::str_starts(m,"all"),"undetermined",m),
                          fleet=ifelse(stringr::str_starts(fleet,"NMFS"),"NMFS",fleet),
                          fleet=ifelse(stringr::str_starts(fleet,"SBS BSFRF"),"BSFRF",fleet),
                          case="tcsam") |> 
            dplyr::select(case,fleet,y,x,m,observed,cv,predicted,residual,zscore,nll);
    lstGSDs[["tscam"]] = dfrT;
  }
  dfr = dplyr::bind_rows(lstGSDs) |> 
          dplyr::mutate(xm=paste(m,x),
                        lwr=qlnorm(0.05,log(observed),sdlog=sdobs),
                        upr=qlnorm(0.95,log(observed),sdlog=sdobs));
  return(dfr);
}

#' @title Compare fits to index time series
#' @description Function to compare fits to index time series.
#' @param dfr - dataframe created using [getFitsToIndexTimeSeries]. 
#' @param f_ - fleet name (as specific for GMACS runs)
#' @return [ggplot2](ggplot) object
#' @export
#' 
plotFitsToIndexTSs <- function(dfr,f_="NMFS"){
                          dfrp = dfr |> dplyr::filter(fleet==f_);
                          p = ggplot(dfrp,aes(x=y,y=predicted,colour=case)) + 
                                geom_ribbon(aes(ymin=lwr,ymax=upr),data=dfrp |> dplyr::filter(!is.na(lwr)),fill="light gray",colour=NA) + 
                                geom_point(aes(y=observed)) + 
                                geom_line() + 
                                geom_hline(yintercept=0,linetype=3) + 
                                facet_grid(xm~.,scales="free_y") + 
                                wtsPlots::getStdTheme() + wtsPlots::noXT() + 
                                theme(legend.position="inside",
                                      legend.position.inside=c(1,1),
                                      legend.justification=c(1,1));
                          return(p);
                        }


####--fits to aggregated size comps----
#' @title Get fits to size comps from GMACS and (possibly) TCSAM02 models
#' @description Function to get fits to size comps from GMACS and (possibly) TCSAM02 models.
#' @param lstGs - list with named GMACS repslst objects from model runs
#' @param lstT - TCSAM02 reslst object
#' @param type - index type to extract ('biomass' or 'abundance')
#' @return dataframe 
#' @export
getFitsToZCs<-function(lstGs,lstT=NULL){
  lstGSDs = list();
  for (case_ in names(lstGs$repsLst)){
    dfrIFS = lstGs$repsLst[[case_]]$Size_fit_summary;
    lstGSDs[[case_]] = dfrIFS |> 
                        dplyr::select(comp_type,y=year,fleet,x=sex,m=maturity,s=shell,z=size,
                                      inpSS=inpSS,observed=aggObs,predicted=aggPrd,residual=aggRes) |> 
                        dplyr::mutate(dplyr::across(c(y,z,inpSS,observed,predicted,residual),as.numeric)) |> 
                        dplyr::mutate(case=case_,.before=1);
  }
  if (!is.null(lstT)){
    lstT1 = list(tcsam=lstT);
    dfrT = dplyr::bind_rows(
               rCompTCMs::extractFits.SizeComps(lstT1,fleet.type="survey", catch.type="index")    |> dplyr::mutate(comp_type="total"),
               rCompTCMs::extractFits.SizeComps(lstT1,fleet.type="fishery",catch.type="retained") |> dplyr::mutate(comp_type="retained"),
               rCompTCMs::extractFits.SizeComps(lstT1,fleet.type="fishery",catch.type="total")    |> dplyr::mutate(comp_type="total"),
               rCompTCMs::extractFits.ZScores.PrNatZ(lstT1,fleet.type="survey", catch.type="index",   residuals.type="pearsons") |> dplyr::mutate(comp_type="total"),
               rCompTCMs::extractFits.ZScores.PrNatZ(lstT1,fleet.type="fishery",catch.type="retained",residuals.type="pearsons") |> dplyr::mutate(comp_type="retained"),
               rCompTCMs::extractFits.ZScores.PrNatZ(lstT1,fleet.type="fishery",catch.type="total",   residuals.type="pearsons") |> dplyr::mutate(comp_type="total")
            ) |> dplyr::mutate(val=ifelse((sign==">0")|(is.na(sign)),val,-1*val)) |> 
            dplyr::select(case,comp_type,y,fleet,x,m,s,z,type,val) |> 
            tidyr::pivot_wider(names_from="type",values_from="val") |> 
            dplyr::filter(!stringr::str_starts(.data$fleet,"SBS NMFS")) |> 
            dplyr::mutate(m=ifelse(stringr::str_starts(.data$m,"all"),"undetermined",m),
                          s=ifelse(stringr::str_starts(.data$s,"all"),"undetermined",s),
                          fleet=ifelse(stringr::str_starts(.data$fleet,"NMFS"),"NMFS",fleet),
                          fleet=ifelse(stringr::str_starts(.data$fleet,"SBS "),"BSFRF",fleet),
                          residual=pearsons);
    lstGSDs[["tscam"]] = dfrT;
  }
  dfr = dplyr::bind_rows(lstGSDs) |> 
          dplyr::mutate(fleet=stringr::str_replace_all(.data$fleet,"_"," "),
                        xm=paste(m,x));
  return(dfr);
}

#' @title Compare fits to size composition data
#' @description Function to compare fits to size composition.
#' @param dfr - dataframe created using [getFitsToZCs]. 
#' @return [ggplot2](ggplot) object
#' @export
#' 
plotFitsToZCs<-function(dfr){
  p = ggplot(dfr,aes(x=z,y=predicted,color=case)) + 
        geom_point(aes(y=observed),data=dfr,size=1) +  #--data
        geom_point(data=dfr |> dplyr::filter(case=="g1"),size=1) +        #--g1 pred as pts
        geom_line() +                                                     #--pred as line
        facet_wrap(~y) + 
        labs(x="size (mm CW)") + 
        wtsPlots::getStdTheme();
  return(p);
}

#' @title Compare size composition data
#' @description Function to compare size composition.
#' @param dfr - dataframe created using [getFitsToZCs]. 
#' @return [ggplot2](ggplot) object
#' @export
#' 
plotDataToZCs<-function(dfr,points="g1"){
  p = ggplot(dfr,aes(x=z,y=observed,color=case)) + 
        geom_point(data=dfr |> dplyr::filter(case==points),size=1) +      #--case to plots as pts
        geom_line() +                                                     #--pred as line
        facet_wrap(~y) + 
        labs(x="size (mm CW)") + 
        wtsPlots::getStdTheme();
  return(p);
}

#' @title Compare residuals for fits to size composition data
#' @description Function to compare residuals for fits to size composition.
#' @param dfr - dataframe created using [getFitsToZCs]. 
#' @return [ggplot2](ggplot) object
#' @export
#' 
plotResidualsToZCs<-function(dfr){
  p = ggplot(dfr,aes(x=z,y=residual,color=case,shape=case)) + 
        geom_line() + geom_point(size=1) + 
        facet_wrap(~y) + 
        labs(x="size (mm CW)") + 
        wtsPlots::getStdTheme();
  return(p);
}
