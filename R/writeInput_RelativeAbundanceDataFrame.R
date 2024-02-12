#' 
#' @title Write a relative abundance dataframe to a connection in GMACS alternative 1 input format
#' 
#' @description Function to write a relative abundance dataframe to a connection in GMACS alternative 1 input format.
#' 
#' @param con : connection to use (default=stdout)
#' @param dfr : an abundance indices dataframe to write to connection 
#' @param fleet_name : name of fleet
#' @param sex : "male","female",or "undetermined"
#' @param maturity - "immature","mature", or "undetermined"
#' @param shell - "new shell","old shell",or "undetermined"
#' @param indexType : index type ("sel" [selectivity] or "sel+ret" [selectivity+retention])
#' @param unitsType : units type ("abundance" or "biomass")
#' @param unitsIn : input catch units (on of "ONES","MILLIONS","KG","THOUSANDS_MT","MILLIONS_LBS","MT","LBS")
#' @param unitsOut : output catch units (should be "MILLIONS" or "THOUSANDS_MT")
#' 
#' @return Invisibly returns the connection \code{con} to allow piping.
#' 
#' @details The input dataframe should have the following column names
#' \itemize{
#'   \item{q_index - index for q parameter}
#'   \item{year - observation year}
#'   \item{season - integer indicating season of observation}
#'   \item{value - observation value}
#'   \item{cv - observation cv}
#'   \item{timing - relative timing within season}
#' }
#' 
#' @export
#' 
writeInput_RelativeAbundanceDataFrame<-function(conn=stdout(),
                                    dfr=NULL,
                                    fleet_name,
                                    sex=c("male","female","undetermined"),
                                    maturity=c("immature","mature","undetermined"),
                                    shell=c("new shell","old shell","undetermined"),
                                    indexType=c("selectivity","selectivity+retention"),
                                    unitsType=c("abundance","biomass"),
                                    unitsIn=ifelse(toupper(unitsType[1])=="ABUNDANCE",
                                                 c("ONES","MILLIONS"),
                                                 c("KG","THOUSANDS_MT","MILLIONS_LBS","MT","LBS")),
                                    unitsOut=ifelse(toupper(unitsType[1])=="ABUNDANCE",
                                                 c("MILLIONS","ONES"),
                                                 c("THOUSANDS_MT","MILLIONS_LBS","MT","KG","LBS"))){
  #--make sure all these are scalars
  sex       = tolower(sex[1]);
  maturity  = tolower(maturity[1]);
  shell     = tolower(shell[1]);
  indexType = tolower(indexType[1]);
  unitsType = tolower(unitsType[1]);
  unitsIn   = unitsIn[1];
  unitsOut  = unitsOut[1];
  cat("##----\n",file=conn);
  cat(paste0("##--fleet: ",fleet_name,"; index type: ",indexType,"; units type: ",unitsType,"; units: ",unitsOut[1],".\n"),file=conn);
  cat(indexType, "       #--index type ('selectivity' or 'selectivity+retention')\n",file=conn);
  cat(unitsType, "       #--units type ('abundance' or 'biomass')\n",file=conn);
  cat(fleet_name,"       #--fleet\n",file=conn);
  cat(sex,       "       #--sex (male(s), female(s), undetermined)\n",file=conn);
  cat(maturity,  "       #--maturity (immature, mature, undetermined)\n",file=conn);
  cat(shell,     "       #--shell condition (new shell, old shell, undetermined)\n",file=conn);
  cat(nrow(dfr), "       #--number of rows in dataframe\n",file=conn);
  cat("# q_index year season value cv multiplier CPUE_timing","\n",file=conn);
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
    cat(dfr$q_index[rw],dfr$year[rw],dfr$season[rw],scl*dfr$value[rw],dfr$cv[rw],
        1,dfr$timing[rw],"\n",file=conn);
  }
  return(invisible(conn));
}
