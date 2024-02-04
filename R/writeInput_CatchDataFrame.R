#' 
#' @title Write fishery catch dataframe to a connection in GMACS alternative 1 input format
#' 
#' @description Function to write a catch dataframe to a connection in GMACS alternative 1 input format.
#' 
#' @param con : connection to use (default=stdout)
#' @param dfr : aggregate catch dataframe to write to connection 
#' @param fleet_name : name of fleet
#' @param fleet_names : vector of fleet_names (order corresponds to input order to GMACS)
#' @param sex : sex 
#' @param catchType : catch type ("RETAINED","DISCARD",or "TOTAL")
#' @param discard_mortality : assumed discard (handling) mortality rate
#' @param unitsType : units type ("ABUNDANCE" or "BIOMASS")
#' @param unitsIn : input catch units (on of "ONES","MILLIONS","KG","THOUSANDS_MT","MILLIONS_LBS","MT","LBS")
#' @param unitsOut : output catch units (should be "MILLIONS" or "THOUSANDS_MT")
#' 
#' @return Invisibly returns the connection \code{con} to allow piping.
#' 
#' @details The input dataframe should have the following column names
#' \itemize{
#'   \item{year - observation year}
#'   \item{sex - string indicating sex}
#'   \item{season - integer indicating season of observation}
#'   \item{value - observation value}
#'   \item{cv - observation cv}
#'   \item{type - units type ("ABUNDANCE" or "BIOMASS")}
#'   \item{effort - fishing effort}
#' }
#' 
#' @export
#' 
writeInput_CatchDataFrame<-function(conn=stdout(),
                                    dfr=NULL,
                                    fleet_name,
                                    fleet_names=NULL,
                                    gear=c("all","fixed","trawl","crab pot"),
                                    sex=c("male","female","undetermined"),
                                    maturity=c("immature","mature","undetermined"),
                                    shell=c("new shell","old shell","undetermined"),
                                    catchType=c("RETAINED","DISCARD","TOTAL"),
                                    discard_mortality=0,
                                    unitsType=c("ABUNDANCE","BIOMASS"),
                                    unitsIn=ifelse(toupper(unitsType[1])=="ABUNDANCE",
                                                 c("ONES","MILLIONS"),
                                                 c("KG","THOUSANDS_MT","MILLIONS_LBS","MT","LBS")),
                                    unitsOut=ifelse(toupper(unitsType[1])=="ABUNDANCE",
                                                 c("MILLIONS","ONES"),
                                                 c("THOUSANDS_MT","MILLIONS_LBS","MT","KG","LBS"))){
  #--make sure all these are scalars
  gear      = gear[1];
  sex       = sex[1];
  maturity  = maturity[1];
  shell     = shell[1];
  catchType = catchType[1];
  unitsType = unitsType[1];
  unitsIn   = unitsIn[1];
  unitsOut  = unitsOut[1];
  # cat("## CATCH DATA\n",file=conn);
  # cat("## Sex:            1 = male,               2 = female,  0 = both","\n",file=conn);
  # cat("## Type of catch:  1 = retained,           2 = discard, 0 = total","\n",file=conn);
  # cat("## Units of catch: 1 = biomass (1,000s t), 2 = numbers (millions)","\n",file=conn);
  # cat("## Multiplier:     1= no scaling,          2 = multiply value by next number (e.g., lbs to 1,000's t)\n",file=conn);
  # cat(paste0("##--fleet:",fleet_name,"; sex:",sex,"; catch_type:",catchType,
  #            "; units_type: ",unitsType,"; units:",unitsOut[1],".\n"),file=conn);
  # cat("#year season fleet sex value cv catch_type catch_units multiplier effort discard_mortality","\n",file=conn);
  cat(paste0("##--fleet: ",fleet_name,"; gear: ",gear,"; catch_type: ",catchType,"; units: ",unitsOut[1],".\n"),file=conn);
  cat(toupper(unitsType), "       #--units type\n",file=conn);
  cat(catchType, "       #--catch type\n",file=conn);
  cat(fleet_name,"       #--fleet\n",file=conn);
  cat(sex,       "       #--sex (male(s), female(s), undetermined)\n",file=conn);
  cat(maturity,  "       #--maturity (immature, mature, undetermined)\n",file=conn);
  cat(shell,     "       #--shell condition (new shell, old shell, undetermined)\n",file=conn);
  cat(nrow(dfr), "       #--number of rows in dataframe\n",file=conn);
  cat("#year season value cv multiplier effort discard_mortality","\n",file=conn);
  if ((is.null(dfr))||(nrow(dfr)==0)) {
    cat("##--------\n",file=conn);
    return(invisible(conn));
  }
  
  cs = getConstants();
  scl = ifelse(toupper(unitsType)=="ABUNDANCE",
                tcsamFunctions::getScaleForAbundance(unitsIn,unitsOut),
                tcsamFunctions::getScaleForBiomass(unitsIn,unitsOut));
  for (rw in 1:nrow(dfr)){
    #--testing: rw = 1;
    cat(dfr$year[rw],dfr$season[rw],scl*dfr$value[rw],dfr$cv[rw],
        1,dfr$effort[rw],discard_mortality[1],"\n",file=conn);
  }
  return(invisible(conn));
}

#' 
#' @title apply assumed CVs to catch dataframe
#' @param dfr - catch dataframe with columns year,type,value
#' @param dfrCVs - dataframe with columns cv, minErr, and possibly year and/or type
#' @return dataframe with cv as column, and otherwise same columns as dfr 
#' @details Adds column \code{cv} to \code{dfr} (if not already present) consistent 
#' with values in \code{dfrCVs}. \code{dfrCVs} has columns
#' \itemize{
#'  \item{cv - default value for cv}
#'  \item{minErr - minimum standard deviation allowed}
#'  \item{sclF - multiplicative scale factor to convert minErr to value units}
#'  \item{year - (optional) corresponding year} 
#'  \item{type - (optional) corresponding value type (ABUNDANCE or BIOMASS)}
#' }
#' @export
#' 
applyAssumedCVs<-function(dfr,dfrCVs,sclF=1){
  getCV<-function(year_,type_,value_,dfrCVs,sclF){
    rwCVs = dfrCVs;
    if ("year" %in% names(dfrCVs))
      rwCVs = rwCVs |> dplyr::filter(year==year_);
    if ("type" %in% names(dfrCVs))
      rwCVs = rwCVs |> dplyr::filter(tolower(type)==tolower(type_));
    cv = max(rwCVs$cv,sclF*rwCVs$minErr/value_);#--minErr and value_ must be on same scale
  }
  dfr = dfr |> dplyr::rowwise() |> 
          dplyr::mutate(cv = getCV(year,type,value,dfrCVs,sclF)) |> 
          dplyr::ungroup();
  return(dfr);
}
