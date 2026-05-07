##--use wtsGMACS::readModelResults(folders) to read GMACS results into lstGMACS

#--extract recruitment-----
#'
#'@title Extract the recruitment time series from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract the recruitment time series from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractRecruitment<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractRecruitment(lstGMACS$repsLst[[nm]]) |> 
                    dplyr::mutate(case=nm);
    }
    dfrG = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    zBs = rep$size_midpoints;
    nZBs = length(zBs);
    #--get gmacs recruitment----
    dfrG = rep$R_y |> dplyr::select(y=year,val=est) |> 
             dplyr::mutate(case="gmacs",.before=1) |> 
             dplyr::mutate(y=as.numeric(y),
                           val=2*as.numeric(val));    #--gmacs recruitment is 0.5*tcsam recruitment
  }
  if (!is.null(lstTCSAM02)) {
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
    #--getTCSAM02 recruitment
    dfrT = rCompTCMs::extractMDFR.Pop.Recruitment(lst) |> 
             dplyr::select(case,y,val);
    dfrG = dplyr::bind_rows(dfrG,dfrT);
  }
  return(dfrG);
}

#'
#'@title Extract size-at-recruitment from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract size-at-recruitment from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractSizeAtRecruitment<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractSizeAtRecruitment(lstGMACS$repsLst[[nm]]) |> 
                    dplyr::mutate(case=nm);
    }
    dfrG = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    #--get gmacs size at recruitment----
    dfrG = rep$R_z |> dplyr::select(x=sex,z=size,val=est) |> 
             dplyr::mutate(case="gmacs",.before=1) |> 
             dplyr::mutate(y="all",
                           z=as.numeric(z),
                           val=as.numeric(val));
  }
  if (!is.null(lstTCSAM02)) {
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
    #--getTCSAM02 recruitment
    dfrT = rCompTCMs::extractMDFR.Pop.RecSizeDistribution(lst) |> 
             dplyr::select(case,y,x,z,val);
    dfrG = dplyr::bind_rows(dfrG,dfrT);
  }
  return(dfrG);
}

#--extract cohort progression----
#'
#'@title Extract the cohort progression from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract the cohort progression from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@param cast - string describing output level of aggregation (default="x+m+s+z")
#'@param gmacsType - gmacs cohort progression type to extract (1: pop starts in first size bin; 2: uses recruitment -at-size distribution)
#'@return dataframe 
#'@export
#'
extractCohortProgression<-function(lstGMACS,lstTCSAM02=NULL,cast="x+m+s+z",gmacsType=2){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractCohortProgression(lstGMACS$repsLst[[nm]],NULL,cast=cast,gmacsType=gmacsType) |> 
                    dplyr::mutate(case=nm);
    }
    dfrG = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    zBs = rep$size_midpoints;
    nZBs = length(zBs);
    gcast = c("y",stringr::str_split_1(cast,"\\+"));
    gcast = rlang::syms(gcast);
    #--get gmacs recruitment----
    type = paste0("CohortProgression",gmacsType)
    dfrG = rep[[type]] |> 
             dplyr::filter(season=="1") |>
             tidyr::pivot_longer(cols=5+1:nZBs,names_to="z",values_to="val") |> 
             dplyr::select(y=year,x=sex,m=maturity,s=shell,z,val) |> 
             dplyr::mutate(y=as.numeric(y)-1,
                           s=paste(s,"shell"),
                           val=as.numeric(val)) |>
             dplyr::group_by(!!!gcast) |> 
             dplyr::summarize(val=sum(val)) |> 
             dplyr::ungroup() |> 
             dplyr::mutate(case="gmacs",.before=1); 
  }
  if (!is.null(lstTCSAM02)) {
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
    #--getTCSAM02 population abundance
    tcast = c("case","y",stringr::str_split_1(cast,"\\+"),"val")
    tcast = rlang::syms(tcast);
    dfrT = rCompTCMs::extractMDFR.Pop.CohortProgression(lst,cast=cast) |> 
             dplyr::select(!!!tcast);
    dfrG = dplyr::bind_rows(dfrG,dfrT);
  }
  return(dfrG)
}

#--extract population abundance----
#'
#'@title Extract the population abundance time series from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract the population abundance time series from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@param cast - string describing output level of aggregation (default="x+m+s+z")
#'@return dataframe 
#'@export
#'
extractPopAbd<-function(lstGMACS,lstTCSAM02=NULL,cast="x+m+s+z"){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractPopAbd(lstGMACS$repsLst[[nm]],cast=cast) |> 
                    dplyr::mutate(case=nm);
    }
    dfrG = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    zBs = rep$size_midpoints;
    nZBs = length(zBs);
    gcast = c("y",stringr::str_split_1(cast,"\\+"));
    gcast = rlang::syms(gcast);
    dfrG = rep$N_YXMSZ |> tidyr::pivot_longer(cols=4+1:nZBs,names_to="z",values_to="val") |> 
             dplyr::select(y=year,x=sex,m=maturity,s=shell,z,val) |> 
             dplyr::mutate(y=as.numeric(y),
                           s=paste(s,"shell"),
                           val=as.numeric(val)) |>
             dplyr::group_by(!!!gcast) |> 
             dplyr::summarize(val=sum(val)) |> 
             dplyr::ungroup() |> 
             dplyr::mutate(case="gmacs",.before=1); 
  }
  if (!is.null(lstTCSAM02)) {
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
    #--getTCSAM02 population abundance
    tcast = c("case","y",stringr::str_split_1(cast,"\\+"),"val")
    tcast = rlang::syms(tcast);
    dfrT = rCompTCMs::extractMDFR.Pop.Abundance(lst,cast=cast) |> 
             dplyr::select(!!!tcast);
    dfrG = dplyr::bind_rows(dfrG,dfrT);
  }
  return(dfrG)
}

#--extract population biomass----
#'
#'@title Extract the population biomass time series from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract the population biomass time series from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@param cast - string describing output level of aggregation (default="x+m+s+z")
#'@return dataframe 
#'@export
#'
extractPopBio<-function(lstGMACS,lstTCSAM02=NULL,cast="x+m+s+z"){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractPopBio(lstGMACS$repsLst[[nm]],cast=cast) |> 
                    dplyr::mutate(case=nm);
    }
    dfrG = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    zBs = rep$size_midpoints;
    nZBs = length(zBs);
    gcast = c("y",stringr::str_split_1(cast,"\\+"));
    gcast = rlang::syms(gcast);
    #--get gmacs population biomass----
    dfrG = rep$B_YXMSZ |> tidyr::pivot_longer(cols=4+1:nZBs,names_to="z",values_to="val") |> 
             dplyr::select(y=year,x=sex,m=maturity,s=shell,z,val) |> 
             dplyr::mutate(y=as.numeric(y),
                           s=paste(s,"shell"),
                           val=as.numeric(val)) |>
             dplyr::group_by(!!!gcast) |> 
             dplyr::summarize(val=sum(val)) |> 
             dplyr::ungroup() |> 
             dplyr::mutate(case="gmacs",.before=1); 
  }
  if (!is.null(lstTCSAM02)) {
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
    #--getTCSAM02 recruitment
    tcast = c("case","y",stringr::str_split_1(cast,"\\+"),"val")
    tcast = rlang::syms(tcast);
    dfrT = rCompTCMs::extractMDFR.Pop.Biomass(lst,cast=cast) |> 
             dplyr::select(!!!tcast);
    dfrG = dplyr::bind_rows(dfrG,dfrT);
  }
  return(dfrG)
}

#--extract mean growth
#'
#'@title Extract mean growth from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract mean growth from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractMeanGrowth<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractMeanGrowth(lstGMACS$repsLst[[nm]]) |> 
                    dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    #--get gmacs mean post-molt size by premolt size----
    dfr = rep$`Mean growth` |> 
            dplyr::select(x=sex,z=premolt_size,val=mean_postmolt_size) |>
                  dplyr::mutate(case=paste("gmacs"),
                                z=as.numeric(z),
                                val=as.numeric(val)) |>
                  rCompTCMs::getMDFR.CanonicalFormat();
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
    dfrT = rCompTCMs::extractMDFR.Pop.MeanGrowth(lst);
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#--extract growth matrices
#'
#'@title Extract growth matrices from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract growth matrices from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractGrowthMatrices<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractGrowthMatrices(lstGMACS$repsLst[[nm]]) |> 
                    dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    #--get gmacs growth matrices----
    dfr = rep$`growth_matrix` |> 
            tidyr::pivot_longer(4:ncol(rep$`growth_matrix`)) |> 
            dplyr::select(x=sex,pc=block,z=premolt_size,zp=name,val=value) |>
                  dplyr::mutate(case=paste("gmacs"),
                                m="immature",
                                z=as.numeric(z),
                                zp=as.numeric(zp),
                                val=as.numeric(val)) |>
                  rCompTCMs::getMDFR.CanonicalFormat();
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
    dfrT = rCompTCMs::extractMDFR.Pop.GrowthMatrices(lst);
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#--extract probability of molt to maturity
#'
#'@title Extract the probability of the molt-to-maturity from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract the molt-to-maturity from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@param cast - string describing output level of aggregation (default="x+m+s+z")
#'@return dataframe 
#'@export
#'
extractPrM2M<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractPrM2M(lstGMACS$repsLst[[nm]]) |> 
                    dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    #--get maturation probabilities
    dfr = rep$`prMature` |> 
            tidyr::pivot_longer(3:ncol(rep$`prMature`)) |> 
            dplyr::select(x=sex,y=year,z=name,val=value) |>
                  dplyr::mutate(case=paste("gmacs"),
                                m="immature",
                                z=as.numeric(z),
                                val=as.numeric(val)) |>
                  rCompTCMs::getMDFR.CanonicalFormat();
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
    dfrT = rCompTCMs::extractMDFR.Pop.PrM2M(lst);
    if ((class(dfrT$y)=="character")||(class(dfr$y)=="character")){
      dfrT$y = as.character(dfrT$y);
      dfr$y  = as.character(dfr$y);
    }
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#--extract fishing mortality rates----
#'
#'@title Extract fishing mortality rates from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract fishing mortality rates from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@param seasons - integer vector seasons to extract fishery info for (or "all"; default=2)
#'@return dataframe with TCSAM02 canonical columns + `season`.
#'@details The dataframe column `season` will be NA for TCSAM02 model results.
#'@export
#'
extractFisheryFs<-function(lstGMACS,lstTCSAM02=NULL,seasons=2){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFisheryFs(lstGMACS$repsLst[[nm]],seasons=seasons);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |> dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    dfrp = lstGMACS$`Fully-selected_FM_by_season_sex_and_fishery`;
    if (is.null(dfrp)) {
      dfrp = lstGMACS$`Fully-selected_capture_rate_by_season_sex_and_fishery`;
      if (is.null(dfrp)) return(NULL);
    }
    #--get gmacs fishery Fs----
    ssns = ncol(dfrp)-3; #--number of season
    if ((!is.numeric(seasons)&&(tolower(seasons)=="all")))
      seasons = 1:ssns;
    dfr = dfrp |> tidyr::pivot_longer(c(3+1:ssns),names_to="season",values_to="val") |>
            dplyr::mutate(dplyr::across(c(season,val),as.numeric))  |> 
            dplyr::filter(season %in% seasons);
    dfr  = dfr |> dplyr::select(fleet,pc=season,y=year,x=sex,val) |>
                  dplyr::mutate(y=as.numeric(y),
                                case=paste("gmacs")) |>
                  rCompTCMs::getMDFR.CanonicalFormat() |> 
                  dplyr::rename(season=pc);#--need to rename "pc" to ""season"
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
    dfrT = rCompTCMs::extractMDFR.Fisheries.Catchability(lst);
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#--extract total fishing mortality----
#'
#'@title Extract total fishing mortality from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract total fishing mortality (biomass) from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractTotalFishingMortality<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractTotalFishingMortality(lstGMACS$repsLst[[nm]]) |> 
                    dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    dfr = rep$Summary |> dplyr::select(y=year,val=tot_mortality) |> 
             dplyr::mutate(y=as.numeric(y),
                           val=as.numeric(val),
                           case="gmacs");
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
    dfrT = rCompTCMs::extractMDFR.Fisheries.CatchBiomass(lst,
                                                         category="total mortality",
                                                         cast="y") |> 
             dplyr::filter(type=="predicted") |> 
             dplyr::group_by(case,y) |> 
             dplyr::summarize(val=sum(val,na.rm=TRUE)) |> 
             dplyr::ungroup() |>
             dplyr::select(case,y,val);
    dfr = dplyr::bind_rows(dfr,dfrT); 
  }
  return(dfr)
}

#--extract retained catch mortality----
#'
#'@title Extract the retained catch mortality from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract the retained catch mortality from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractRetainedCatchMortality<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractRetainedCatchMortality(lstGMACS$repsLst[[nm]]) |> 
                    dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    dfr = rep$Summary |> dplyr::select(y=year,val=ret_mortality) |> 
             dplyr::mutate(y=as.numeric(y),
                           val=as.numeric(val),
                           case="gmacs");
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
      dfrT = rCompTCMs::extractMDFR.Fisheries.CatchBiomass(list(tcsam=lstT),
                                                           category="retained",
                                                           cast="y") |> 
               dplyr::filter(type=="predicted",category=="retained") |> 
               dplyr::select(case,y,val);
      dfr = dplyr::bind_rows(dfr,dfrT); 
  }
  return(dfr)
}

#--extract discard catch mortality----
#'
#'@title Extract the discard catch mortality from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract the discard catch mortality from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractDiscardCatchMortality<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractDiscardCatchMortality(lstGMACS$repsLst[[nm]]) |> 
                    dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    rep = lstGMACS;
    flts = rep$Fleets;
    tdys = c("year",flts,"ret_mortality");
    # dfr = rep$Summary |> dplyr::select(y=year,TCF,SCF,RKF,GFA,ret_mortality) |> 
    #          dplyr::mutate(dplyr::across(c(y,TCF,SCF,RKF,GFA),as.numeric)) |> 
    #          dplyr::mutate(TCF=as.numeric(TCF)-as.numeric(ret_mortality)) |> dplyr::select(!ret_mortality) |> 
    #          tidyr::pivot_longer(c(TCF,SCF,RKF,GFA),names_to="fleet",values_to="val") |> 
    #          dplyr::mutate(case="gmacs");
    dfr = rep$Summary |> dplyr::select(tidyselect::all_of(tdys)) |> 
             dplyr::mutate(dplyr::across(tidyselect::all_of(tdys),as.numeric)) |> 
             dplyr::mutate(y=year,
                           TCF=TCF-ret_mortality) |> 
             dplyr::select(!c(ret_mortality,year)) |> 
             tidyr::pivot_longer(tidyselect::all_of(flts),names_to="fleet",values_to="val") |> 
             dplyr::mutate(case="gmacs");
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02)
      dfrT = rCompTCMs::extractMDFR.Fisheries.CatchBiomass(list(tcsam=lstT),
                                                           category="discard mortality",
                                                           cast="y") |> 
               dplyr::filter(type=="predicted",category=="discard mortality") |> 
               dplyr::select(case,fleet,y,val);
      dfr = dplyr::bind_rows(dfr,dfrT); 
  }
  return(dfr)
}

#--extract fishery capture biomass----
#'
#'@title Extract the total fishery capture biomass from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract the total fishery capture biomass from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFisheryCaptureBiomass<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFisheryCaptureBiomass(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    dfrp = lstGMACS$`Predicted_capture_biomass-at-size`;
    if (is.null(dfrp)) return(NULL);
    dfr = dfrp |>
             tidyr::pivot_longer(4:ncol(dfrp),
                                 names_to="z",values_to="val") |> 
             dplyr::rename(y=year,x=sex) |> 
             dplyr::mutate(y=as.numeric(y),
                           z=as.numeric(z),
                           val=as.numeric(val)) |> 
             dplyr::group_by(y,fleet,x) |> 
             dplyr::summarize(val=sum(val,na.rm=TRUE)) |> 
             dplyr::ungroup() |> 
             dplyr::mutate(case="gmacs");
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
      dfrT = rCompTCMs::extractMDFR.Fisheries.CatchBiomass(list(tcsam=lstT),
                                                           category="captured",
                                                           cast="y+x") |> 
               dplyr::filter(type=="predicted",category=="captured") |> 
               dplyr::select(case,fleet,y,x,val);
      dfr = dplyr::bind_rows(dfr,dfrT); 
  }
  return(dfr)
}

#--extract fishery capture abundance----
#'
#'@title Extract the fishery capture abundance from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract the fishery capture abundance from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFisheryCaptureAbundance<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFisheryCaptureAbundance(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    dfrp = lstGMACS$`Predicted_capture_abundance-at-size`;
    if (is.null(dfrp)) return(NULL);
    dfr = dfrp |>
             tidyr::pivot_longer(4:ncol(dfrp),
                                 names_to="z",values_to="val") |> 
             dplyr::rename(y=year,x=sex) |> 
             dplyr::mutate(y=as.numeric(y),
                           z=as.numeric(z),
                           val=as.numeric(val)) |> 
             dplyr::group_by(y,fleet,x) |> 
             dplyr::summarize(val=sum(val,na.rm=TRUE)) |> 
             dplyr::ungroup() |> 
             dplyr::mutate(case="gmacs");
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
      dfrT = rCompTCMs::extractMDFR.Fisheries.CatchAbundance(list(tcsam=lstT),
                                                           category="captured",
                                                           cast="y+x") |> 
               dplyr::filter(type=="predicted",category=="captured") |> 
               dplyr::select(case,fleet,y,x,val);
      dfr = dplyr::bind_rows(dfr,dfrT); 
  }
  return(dfr)
}

#--extract predicted survey biomass----
#'
#'@title Extract predicted survey biomass from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract predicted survey biomass from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@param fleetG - gmacs name for fleet to extract
#'@param fleetT - tcsam02 name for fleet to extract (default = same as gmacs name)
#'@return dataframe 
#'@export
#'
extractPredictedSurveyBiomass<-function(lstGMACS,lstTCSAM02=NULL,fleetG=NULL,fleetT=fleetG){
  if (is.null(fleetG)) stop("must supply a survey fleet name");
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractPredictedSurveyBiomass(lstGMACS$repsLst[[nm]],fleetG=fleetG);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    dfrp = lstGMACS$`Index_fit_summary`;
    if (is.null(dfrp)) return(NULL);
    dfr = dfrp |>
           dplyr::filter(stringr::str_starts(fleet,fleetG),units %in% c("biomass","biommass")) |> 
           dplyr::select(y=year,x=sex,m=maturity,val=prd) |> 
           dplyr::mutate(y=as.numeric(y),val=as.numeric(val),m=ifelse(m=="undetermined","all",m)) |> 
           dplyr::mutate(case="gmacs");
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
      dfrT1 = rCompTCMs::extractMDFR.Surveys.Biomass(lst,cast="x+m") |> 
               dplyr::filter(stringr::str_starts(fleet,fleetT),x=="female") |> 
               dplyr::select(case,y,x,m,val);
      dfrT2 = rCompTCMs::extractMDFR.Surveys.Biomass(lst,cast="x") |> 
               dplyr::filter(stringr::str_starts(fleet,fleetT),x=="male") |> 
               dplyr::select(case,y,x,m,val);
      dfr = dplyr::bind_rows(dfr,dfrT1,dfrT2); 
  }
  return(dfr)
}

#--fits to fishery total catch biomass----
#'
#'@title Extract fits to fishery catch biomass from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract fishery catch biomass from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFitsToFisheryRetainedCatchBiomass<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFitsToFisheryRetainedCatchBiomass(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    ##--dfrG part
    dfrGCDs = lstGMACS$Catch_fit_summary |> 
                dplyr::filter(fleet="TCF",type=="retained",units=="biomass") |> 
                dplyr::select(y=year,fleet,observed=obs,cv,predicted=prd,rsd,nll) |> 
                dplyr::mutate(across(c(y,observed,cv,predicted,rsd,nll),as.numeric),
                              fleet=stringr::str_replace(fleet,"_"," "),
                              zscore=rsd/sqrt(log(1+cv^2)),
                              case="gmacs") |> 
                dplyr::select(case,fleet,y,observed,cv,predicted,residual=rsd,zscore,nll);
    dfr = dfrG;
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
    ##--dfrT part
    dfrT = rTCSAM02::getMDFR.AllScores.Biomass(lstT,
                                               fleet.type="fishery",
                                               catch.type="retained") |> 
            tidyr::pivot_wider(names_from=type,values_from=val) |> 
            dplyr::filter(x!="female") |> 
            dplyr::mutate(cv=sqrt(exp(sdobs^2)-1),
                          residual=log(observed)-log(predicted),
                          nll=-dnorm(residual,sd=stdv,log=TRUE),
                          case="tcsam") |> 
            dplyr::select(case,fleet,y,observed,cv,predicted,residual,zscore=`z-score`,nll);
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#--fits to fishery total catch biomass----
#'
#'@title Extract fits to fishery catch biomass from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract fishery catch biomass from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFitsToFisheryTotalCatchBiomass<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFitsToFisheryTotalCatchBiomass(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    ##--dfrG part
    dfrGCDs = lstGMACS$Catch_fit_summary |> 
                dplyr::filter(type=="total",units=="biomass") |> 
                dplyr::select(y=year,fleet,observed=obs,cv,predicted=prd,rsd,nll) |> 
                dplyr::mutate(across(c(y,observed,cv,predicted,rsd,nll),as.numeric),
                              fleet=stringr::str_replace(fleet,"_"," "),
                              zscore=rsd/sqrt(log(1+cv^2)),
                              case="gmacs") |> 
                dplyr::select(case,fleet,y,observed,cv,predicted,residual=rsd,zscore,nll);
    dfr = dfrG;
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
    ##--dfrT part
    dfrT = rTCSAM02::getMDFR.AllScores.Biomass(lstT,
                                               fleet.type="fishery",
                                               catch.type="total") |> 
            tidyr::pivot_wider(names_from=type,values_from=val) |> 
            dplyr::mutate(cv=sqrt(exp(sdobs^2)-1),
                          residual=log(observed)-log(predicted),
                          nll=-dnorm(residual,sd=stdv,log=TRUE),
                          case="tcsam") |> 
            dplyr::select(case,fleet,y,observed,cv,predicted,residual,zscore=`z-score`,nll);
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#--fits to fishery total catch abundance----
#'
#'@title Extract fits to fishery catch abundance from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract fishery catch abundance from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFitsToFisheryCatchTotalAbundance<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFitsToFisheryTotalCatchAbundance(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    ##--dfrG part
    dfrGCDs = lstGMACS$Catch_fit_summary |> 
                dplyr::filter(type=="total",units=="numbers") |> 
                dplyr::select(y=year,fleet,observed=obs,cv,predicted=prd,rsd,nll) |> 
                dplyr::mutate(across(c(y,observed,cv,predicted,rsd,nll),as.numeric),
                              fleet=stringr::str_replace(fleet,"_"," "),
                              zscore=rsd/sqrt(log(1+cv^2)),
                              case="gmacs") |> 
                dplyr::select(case,fleet,y,observed,cv,predicted,residual=rsd,zscore,nll);
    dfr = dfrG;
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
    ##--dfrT part
    dfrT = rTCSAM02::getMDFR.AllScores.Abundance(lstT,
                                               fleet.type="fishery",
                                               catch.type="total") |> 
            tidyr::pivot_wider(names_from=type,values_from=val) |> 
            dplyr::filter(fleet=="GF All") |> 
            dplyr::mutate(cv=sqrt(exp(sdobs^2)-1),
                          residual=log(observed)-log(predicted),
                          nll=-dnorm(residual,sd=stdv,log=TRUE),
                          case="tcsam") |> 
            dplyr::select(case,fleet,y,observed,cv,predicted,residual,zscore=`z-score`,nll);
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#--fits to survey biomass----
#'
#'@title Extract fits to survey biomass from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract fits to survey biomass from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFitsToSurveyBiomass<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFitsToSurveyBiomass(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    ##--dfrG part
    dfrGSDs = lstGMACS$Index_fit_summary |> dplyr::filter(units=="biomass") |> 
                dplyr::select(y=year,fleet,x=sex,m=maturity,
                              observed=obs,cv=actual_CV,predicted=prd,zscore=prsn_res) |> 
                dplyr::mutate(dplyr::across(c(y,observed,cv,predicted,zscore),as.numeric),
                              sdobs=sqrt(log(1+cv^2)),
                              nll=0.5*log(2*pi*sdobs^2) + (0.5*zscore^2),
                              residual=log(observed)-log(predicted)) |> 
                dplyr::mutate(case="gmacs",.before=1);
    dfr = dfrG;
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
    ##--dfrT part
    dfrT = rTCSAM02::getMDFR.AllScores.Biomass(lstT,
                                               fleet.type="survey",
                                               catch.type="index") |> 
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
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#'
#'@title Extract summary info on fits to fishery size comps from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract summary info on fits to fishery size comps from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFitsToFisherySizeComps<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFitsToFisherySizeComps(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    ##--dfrG part
    dfrGs = lstGMACS$Likelihood_summary;
    dfrG = dfrGs |> dplyr::filter(term=="Size_data") |>
                dplyr::filter(!fleet %in% c("NMFS","BSFRF"),type!="summary") |>
                dplyr::mutate(case="gmacs",s="undetermined",nll.type="unknown") |>
                dplyr::select(!c(term,emphasis)) |>
                dplyr::select(case,fleet,catch.type=type,nll.type,x=sex,m=maturity,s,nll,objfun=objfun_value) |>
                dplyr::mutate(dplyr::across(c(nll,objfun),as.numeric),
                              fleet=ifelse(fleet=="GF_All","GF All",fleet),
                                           catch.type=paste(catch.type,"catch")) |>
                dplyr::group_by(case,fleet,catch.type) |>  #--summarize over x,m,s
                dplyr::summarize(nll=sum(nll),
                                 objfun=sum(objfun)) |>
                dplyr::ungroup();
    dfr = dfrG;
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
    ##--dfrT part
    dfrT = rTCSAM02::getMDFR.OFCs.FleetData(lstT,category="fisheries") |>
             dplyr::filter((data.type=="n.at.z")) |>
             dplyr::group_by(fleet,catch.type) |>              #--summarize over x,m,s
             dplyr::summarize(nll=sum(nll,na.rm=TRUE),
                              objfun=sum(objfun,ma.rm=TRUE)) |>
             dplyr::ungroup() |>
             dplyr::mutate(case="tcsam",.before=1);
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#'
#'@title Extract summary info on fits to survey size comp data from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract summary info on fits to survey size comp data from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFitsToSurveySizeComps<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFitsToSizeComps(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    ##--dfrG part
    dfrGs = lstGMACS$Likelihood_summary;
    dfrG = dfrGs |> dplyr::filter(term=="Size_data") |>
                dplyr::filter(fleet %in% c("NMFS","BSFRF")) |>
                dplyr::mutate(case="gmacs",s="undetermined",nll.type="unknown") |>
                dplyr::select(!c(term,type,emphasis)) |>
                dplyr::select(case,fleet,nll.type,x=sex,m=maturity,s,nll,objfun=objfun_value) |>
                dplyr::mutate(dplyr::across(c(nll,objfun),as.numeric));
    dfr = dfrG |> 
             dplyr::select(!fit.type) |>
             dplyr::mutate(xm=paste(x,m));
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
    ##--dfrT part
    dfrT = rTCSAM02::getMDFR.OFCs.FleetData(lstT,category="surveys") |>
             dplyr::filter((data.type=="n.at.z") &
                          (!(fleet |> stringr::str_starts("SBS NMFS")))) |>
             dplyr::group_by(fleet,fit.type,nll.type,x,m,s) |>
             dplyr::summarize(nll=sum(nll,na.rm=TRUE),
                              objfun=sum(objfun,ma.rm=TRUE)) |>
             dplyr::ungroup() |>
             dplyr::mutate(x=tolower(x),
                           m=ifelse(m=="ALL_MATURITY","undetermined",tolower(m)),
                           s=ifelse(s=="ALL_SHELL","undetermined",tolower(s)),
                           fleet=ifelse(fleet |> stringr::str_starts("NMFS"),"NMFS",fleet),
                           fleet=ifelse(fleet |> stringr::str_starts("SBS"),"BSFRF",fleet)) |>
             dplyr::mutate(case="tcsam",.before=1) |> 
             dplyr::select(!fit.type) |>
             dplyr::mutate(xm=paste(x,m));
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#'
#'@title Extract fits to growth data from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract fits to growth data from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFitsToGrowthData<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFitsToGrowthData(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    ##--dfrG part
    dfrGs = lstGMACS$Likelihood_summary;
    dfrG = dfrGs |> dplyr::filter(term=="Growth_data",type=="growth_data") |>
                dplyr::filter(type!="summary") |>
                dplyr::select(x=sex,nll,wgt=emphasis,objfun=objfun_value) |>
                dplyr::mutate(case="gmacs",.before=1) |>
                dplyr::mutate(dplyr::across(c(nll,wgt,objfun),as.numeric));
    dfr = dfrG;
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
    ##--dfrT part
    dfrT = rTCSAM02::getMDFR.OFCs.GrowthData(lstT) |>
             dplyr::select(x,nll,wgt,objfun) |>
             dplyr::mutate(case="tcsam",.before=1);
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#'
#'@title Extract male maturity ogives from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract male maturity ogives from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFitsToMMOs<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFitsToMMOs(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    ##--dfrG part
    dfrGs = lstGMACS$Likelihood_summary;
    dfrG = dfrGs |> dplyr::filter(term=="MMOD_data",type=="mmod_data") |> 
              dplyr::mutate(dplyr::across(c(nll,emphasis,objfun_value),as.numeric)) |> 
              dplyr::filter(type!="summary",nll>0) |>
              dplyr::select(y=year,nll,wgt=emphasis,objfun=objfun_value) |>
              dplyr::mutate(case="gmacs",.before=1) |>
              dplyr::mutate(dplyr::across(c(y,nll,wgt,objfun),as.numeric),
                            objfun=ifelse(objfun==0,NA,objfun));
    dfr = dfrG;
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
    ##--dfrT part
    dfrT = rTCSAM02::getMDFR.OFCs.MaturityOgiveData(lstT) |>
             dplyr::select(y,nll,wgt,objfun) |>
             dplyr::mutate(case="tcsam",.before=1);
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#'
#'@title Extract details of fits to male maturity ogives from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract details of fits to male maturity ogives from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'@export
#'
extractFitsToMMOs.Details<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFitsToMMOs.Details(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    dfrG = lstGMACS$likeSummaryMMOsP |> 
              dplyr::select(y=year,z=zb,n=ss,obs,prd,res,zscr,nll) |>
                dplyr::mutate(dplyr::across(c(y,z,n,obs,prd,res,zscr,nll),as.numeric)) |> 
                dplyr::mutate(case="gmacs",.before=1);
    dfr = dfrG;
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
    dfrT = rTCSAM02::getMDFR.Fits.MaturityOgiveData(lstT) |> 
             dplyr::select(y,type,z,val)|>
             dplyr::mutate(case="tcsam",.before=1) |> 
             tidyr::pivot_wider(names_from="type",values_from="val") |> 
             dplyr::select(case,y,z,n,obs=observed,prd=predicted,zscr=zscores,nll=nlls) |> 
             dplyr::mutate(res=obs-prd);
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}

#'
#'@title Extract management quantities from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract management quantities from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'#export
#'
extractMgtQtys<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractMgtQtys(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    ##--dfrG part
    nflt = lstGMACS$Number_of_fleets;
    dfr  = lstGMACS$`Derived_quantities` |> 
             dplyr::select(type=param,val=est) |> 
             dplyr::mutate(dplyr::across(c(val),as.numeric)) |> 
             dplyr::filter(dplyr::row_number() %in% c(1:7,(7+nflt))) |> 
             tidyr::pivot_wider(names_from="type",values_from="val") |> 
             dplyr::mutate(case="gmacs",
                           avgRec=male_spr_rbar+female_spr_rbar,
                           OFL=`OFL(tot)`,
                           Fofl=`Fofl(1)`,
                           Fmsy=`Fmsy(1)`,
                           curB=NA,
                           prjB=`Bcurr/BMSY`*BMSY,
                           MSY=NA,
                           B100=male_spr_rbar*`SSSB/R(F=0)`/1000) |>  #--think SSB/R(F=0) is for males; units are grams, need KT so x 1000 if rec in millions
             dplyr::select(case,OFL,Fofl,prjB,curB,Fmsy,Bmsy=BMSY,MSY,B100,avgRec) |> 
             tidyr::pivot_longer(cols=c(-1),names_to="type",values_to="val");
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
      dfrT = rTCSAM02::getMDFR.ManagementQuantities(lst) |> 
               dplyr::mutate(case="tcsam") |> 
               dplyr::select(case,type,val)  |> 
               tidyr::pivot_wider(names_from="type",values_from="val") |> 
               dplyr::mutate(Bmsy=0.35*B100) |> 
               tidyr::pivot_longer(cols=c(-1),names_to="type",values_to="val");
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}


#--FUNCTION TEMPLATE----
#'
#'@title Extract [description] from (possibly several) GMACS and TCSAM model runs.
#'@description Function to extract [description] from (possibly several) GMACS and TCSAM model runs.
#'@param lstGMACS - gmacs_reslst or gmacs_replst object, or NULL
#'@param lstTCSAM - tcsam02.resLst object, or NULL
#'@return dataframe 
#'#export
#'
extractFUNCTIONTEMPLATE<-function(lstGMACS,lstTCSAM02=NULL){
  if (inherits(lstGMACS,"gmacs_reslst")) {
    lst=list();
    for (nm in names(lstGMACS$repsLst)){
      #--testing: nm = names(lstGMACS$repsLst)[1];
      lst[[nm]] = extractFUNCTIONTEMPLATE(lstGMACS$repsLst[[nm]]);
      if (!is.null(lst[[nm]])) lst[[nm]] = lst[[nm]] |>  dplyr::mutate(case=nm);
    }
    dfr = dplyr::bind_rows(lst);
    rm(lst);
  } else if (inherits(lstGMACS,"gmacs_rep1")) {
    ##--dfrG part
    dfr = dfrG;
  }
  if (!is.null(lstTCSAM02)){
    if (inherits(lstTCSAM02,"tcsam02.resLst")) lst = list(tcsam=lstTCSAM02);
    ##--dfrT part
    dfr = dplyr::bind_rows(dfr,dfrT);
  }
  return(dfr);
}
