#--write relative abundance (survey indices, CPUE) data to GMACS dat file format 
#----by gear type and aggregated across gear types
options(stringsAsFactors=FALSE);
require(tcsamFunctions);
require(wtsSizeComps);
require(wtsUtilities);
source("R/getConstants.R")
source("R/writeInput_RelativeAbundanceDataFrame.R")

MILLION = 1000000;

#--get assessment setup info
#----define path to top folder of assessment
dirPrj = "/Users/williamstockhausen/Work/StockAssessments-Crab/Assessments/TannerCrab/2023-09_TannerCrab";
#----get list with assessment setup
s = wtsUtilities::getObj(file.path(dirPrj,"rda_AssessmentSetup.RData"));


#-------------------------------------------------------------------------------
#--NMFS EBS shelf survey
#----define assumed values
asmd_ssn = 1;  #--assumed season
asmd_tmg = 0;  #--assumed timing within season

#--define q indices
lstQs = list(tibble::tibble(q_index=1,year=1975:2023,x=  "male",m="undetermined",s="undetermined"),
             tibble::tibble(q_index=1,year=1975:2023,x="female",m=    "immature",s="undetermined"),
             tibble::tibble(q_index=1,year=1975:2023,x="female",m=      "mature",s="undetermined"));
dfrQs = dplyr::bind_rows(lstQs);

#----get NMFS survey info
lstNMFS = wtsUtilities::getObj(s$fnDataSrvNMFS.DBI);#--NMFS survey data
dfrNMFS = lstNMFS$lstABs$ABs$EBS |> 
            dplyr::select(year=YEAR,x=SEX,m=MATURITY,s=SHELL_CONDITION,
                          abundance=totABUNDANCE,abundance_cv=cvABUNDANCE,
                          biomass=totBIOMASS,biomass_cv=cvBIOMASS) |> 
            dplyr::mutate(x=tolower(x),
                          m=tolower(m),
                          s=tolower(s),
                          x=ifelse(x=="all","undetermined",x),
                          m=ifelse(m=="all","undetermined",m),
                          s=ifelse(s=="all","undetermined",s)) |> 
            tidyr::pivot_longer(cols=c("abundance","abundance_cv","biomass","biomass_cv"),
                                names_to="type",values_to="value") |> 
            dplyr::rowwise() |> 
            dplyr::mutate(unitsType=stringr::str_split_1(type,"_")[1],
                          valueType=ifelse(stringr::str_detect(type,"cv"),"cv","value")) |> 
            dplyr::ungroup() |> 
            dplyr::select(!type) |> 
            tidyr::pivot_wider(names_from="valueType",values_from="value") |> 
            dplyr::mutate(fleet="NMFS",
                          season=asmd_ssn,
                          timing=asmd_tmg,
                          indexType="sel") |>  #--"sel" or "sel+ret"
            dplyr::left_join(dfrQs,by=c("year","x","m","s")) |> 
            dplyr::select(indexType,q_index,fleet,year,season,timing,x,m,s,unitsType,value,cv);
rm(asmd_ssn,asmd_tmg,lstQs,dfrQs,lstNMFS);

#-------------------------------------------------------------------------------
#--BSFRF SBS survey
#----define assumed values
asmd_ssn = 1;  #--assumed season
asmd_tmg = 0;  #--assumed timing within season

#--define q indices
lstQs = list(tibble::tibble(q_index=2,year=1975:2023,x=  "male",m="undetermined",s="undetermined"),
             tibble::tibble(q_index=2,year=1975:2023,x="female",m=    "immature",s="undetermined"),
             tibble::tibble(q_index=2,year=1975:2023,x="female",m=      "mature",s="undetermined"));
dfrQs = dplyr::bind_rows(lstQs);

#----get BSFRF SBS survey info
lstSBS = wtsUtilities::getObj(s$fnDataSrvSBS.DBI);#--SBS survey data
dfrSBS_BSFRF = lstSBS$lstABs_BSFRF$ABs$EBS |> 
            dplyr::select(year=YEAR,x=SEX,m=MATURITY,s=SHELL_CONDITION,
                          abundance=totABUNDANCE,abundance_cv=cvABUNDANCE,
                          biomass=totBIOMASS,biomass_cv=cvBIOMASS) |> 
            dplyr::mutate(x=tolower(x),
                          m=tolower(m),
                          s=tolower(s),
                          x=ifelse(x=="all","undetermined",x),
                          m=ifelse(m=="all","undetermined",m),
                          s=ifelse(s=="all","undetermined",s)) |> 
            tidyr::pivot_longer(cols=c("abundance","abundance_cv","biomass","biomass_cv"),
                                names_to="type",values_to="value") |> 
            dplyr::rowwise() |> 
            dplyr::mutate(unitsType=stringr::str_split_1(type,"_")[1],
                          valueType=ifelse(stringr::str_detect(type,"cv"),"cv","value")) |> 
            dplyr::ungroup() |> 
            dplyr::select(!type) |> 
            tidyr::pivot_wider(names_from="valueType",values_from="value") |> 
            dplyr::mutate(fleet="BSFRF_SBS",
                          season=asmd_ssn,
                          timing=asmd_tmg,
                          indexType="sel") |>   #--"sel" or "sel+ret"
            dplyr::left_join(dfrQs,by=c("year","x","m","s")) |> 
            dplyr::select(indexType,q_index,fleet,year,season,timing,x,m,s,unitsType,value,cv);
rm(asmd_ssn,asmd_tmg,lstQs,dfrQs,lstSBS);

#--concatenate survey abundance data
dfrSD = dplyr::bind_rows(dfrNMFS,dfrSBS_BSFRF);
rm(dfrNMFS,dfrSBS_BSFRF)

#determine distinct categories
dfrDCs = dfrSD |> dplyr::distinct(indexType,unitsType,fleet,x,m,s);
################################################################################
#--write output to assessment data file for relative abundance data by fleet
conn = file("GMACS_InputRelativeAbundanceData.txt",open="w")
cat("##------------------------------------------------------------\n",file=conn);
cat("##--RELATIVE ABUNDANCE DATA-----------------------------------\n",file=conn);
cat("##------------------------------------------------------------\n",file=conn);
cat(1,"#--input format for catch data (0:old format; 1: new format)\n",file=conn);
cat(nrow(dfrDCs),"#--number of dataframes\n",file=conn);
for (rw in 1:nrow(dfrDCs)){
  #--testing rw=1;
  rwDC = dfrDCs[rw,];
  dfrSDp = dfrSD |> dplyr::filter(indexType==rwDC$indexType,
                                  unitsType==rwDC$unitsType,
                                  fleet==rwDC$fleet,
                                  x==rwDC$x,m==rwDC$m,s==rwDC$s);
  if (nrow(dfrSDp)>0){
    writeInput_RelativeAbundanceDataFrame(
      conn=conn,
      dfr=dfrSDp,
      fleet_name=rwDC$fleet,
      sex=rwDC$x,
      maturity=rwDC$m,
      shell=rwDC$s,
      indexType=rwDC$indexType,
      unitsType=rwDC$unitsType,
      unitsIn=ifelse(toupper(rwDC$unitsType)=="ABUNDANCE",
                   c("MILLIONS"),c("THOUSANDS_MT")),
      unitsOut=ifelse(toupper(rwDC$unitsType)=="ABUNDANCE",
                   c("MILLIONS"), c("THOUSANDS_MT"))
    );
  }
  rm(rwDC,dfrSDp);
}
close(conn);

