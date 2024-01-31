#' 
#' @title Write size comps dataframe to a connection in GMACS input format
#' 
#' @description Function to write a size comps dataframe to a connection in GMACS input format.
#' 
#' @param con : connection to use (default=stdout)
#' @param dfrZCs : size comps dataframe to write to connection 
#' @param fleet_name : name of fleet
#' @param fleet_names : vector of fleet_names (order corresponds to input order to GMACS)
#' @param sex : sex 
#' @param catchType : catch type ("RETAINED","DISCARD","TOTAL")
#' @param unitsIn : input catch units (on of "ONES","MILLIONS")
#' @param unitsOut : output catch units (should be "MILLIONS" for input file)
#' 
#' @return Invisibly returns the connection \code{con} to allow piping.
#' 
#' @details The input dataframe should have the following column names
#' \itemize{
#'   \item{year - observation year}
#'   \item{sex - string indicating sex}
#'   \item{season - integer indicating season of observation}
#'   \item{value - observation value}
#' }
#' 
#' @export
#' 
writeInput_ZCsDataFrame<-function(conn=stdout(),
                                    dfr=NULL,
                                    fleet_name,
                                    fleet_names=NULL,
                                    gear=c("all","fixed","trawl","pot"),
                                    sex=c("MALE","FEMALE","ALL"),
                                    catchType=c("RETAINED","DISCARD","TOTAL"),
                                    unitsIn=c("ONES","MILLIONS"),
                                    unitsOut="MILLIONS"){
  #--make sure some inputs are scalars
  sex = sex[1];
  catchType = catchType[1];
  unitsIn   = unitsIn[1];
  unitsOut   = unitsOut[1];
  #--get constants for conversion from string to id
  cs = getConstants();
  fleet_id  = ifelse(is.null(fleet_names),fleet_name,which(fleet_name==fleet_names));
  #--pivot size bins wider
  dfr = dfr |>
          tidyr::pivot_wider(id_cols=c("year","season","gear","ss","x","m","s"),
                             names_from="z",
                             values_from="value");

  cat("##	Sex: 1=male; 2=female; 0=undetermined(?)                                                    \n",file=conn);
  cat("##	Type of	composition: 1=retained;  2=discard;   0=total catch                                \n",file=conn);
  cat("##	Maturity	state:	   1=mature;    2=immature;  0=undetermined                               \n",file=conn);
  cat("##	Shell	condition:	   1=new shell; 2=old shell; 0=undetermined                               \n",file=conn);
  cat("##--                                                                                           \n",file=conn);
  cat(paste0("##--fleet: ",fleet_name,"; gear: ",gear,"; sex: ",sex,"; catch_type: ",catchType,"; units: ",unitsOut[1],".\n"),file=conn);
  cat("#Year  Season  Fleet  Sex  Type  Shell  Maturity  Nsamp  ",file=conn);
  if ((is.null(dfr))||(nrow(dfr)==0)) {
    cat("\n",file=conn);
    return(invisible(conn));
  }
  
  cs = getConstants();
  scl = tcsamFunctions::getScaleForAbundance(unitsIn,unitsOut);
  nc  = ncol(dfr); #--number of columns in dataframe
  cat(names(dfr[1,])[8:nc],"\n",file=conn);
  cat("## number of rows:",nrow(dfr),"number of size bins:",nc-7,"\n",file=conn);
  for (rw in 1:nrow(dfr)){
    #--testing: rw = 1;
    cat(dfr$year[rw],dfr$season[rw],fleet_id,cs$xs[[tolower(dfr$x[rw])]],cs$catchType[[tolower(catchType)]],
        cs$ss[[tolower(dfr$s[rw])]],cs$ms[[tolower(dfr$m[rw])]],dfr$ss[rw],
        paste(scl*as.matrix(dfr[rw,8:nc,drop=TRUE])),"\n",file=conn);
  }
  return(invisible(conn));
}
