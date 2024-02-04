#--write size composition data in GMACS dat file format 
options(stringsAsFactors=FALSE);
require(tcsamFisheryDataAKFIN);
require(tcsamFunctions);
require(wtsSizeComps);
require(wtsUtilities);
source("R/getConstants.R")
source("R/writeInput_ZCsDataFrame.R")

MILLION = 1000000;

#--get assessment setup info
#----define path to top folder of assessment
dirPrj = "/Users/williamstockhausen/Work/StockAssessments-Crab/Assessments/TannerCrab/2023-09_TannerCrab";
#----get list with assessment setup
s = wtsUtilities::getObj(file.path(dirPrj,"rda_AssessmentSetup.RData"));

#------------------------------------------------------------------------------
#--get crab fisheries info
#----define assumed values
asmd_ssn = 2;  #--assumed season

lstCF = wtsUtilities::getObj(s$fnDataCFsA);

#--retained catch: aggregate over maturity state, shell condition
#----"all EBS" info is available for all years
#----input sample sizes
dfrSSs = lstCF$dfrRC_SSs_inp |> 
            dplyr::filter(area=="all EBS") |>
            dplyr::group_by(fishery,year,sex) |> 
            dplyr::summarize(ss=wtsUtilities::Sum(ss)) |> 
            dplyr::ungroup() |> 
            dplyr::select(fishery,year,x=sex,ss);
#----size comps
dfrRZCs = lstCF$dfrRC_ZCs_ByFAYXMS |> 
            dplyr::filter(area=="all EBS") |>
            dplyr::select(fishery,year,x=sex,m=maturity,s=`shell condition`,z=size,N=abundance) |>
            dplyr::group_by(fishery,year,x,z) |> 
            dplyr::summarize(value=wtsUtilities::Sum(N)) |> 
            dplyr::ungroup() |> 
            dplyr::inner_join(dfrSSs,by=c("fishery","year","x")) |> 
            dplyr::mutate(m="undetermined",s="undetermined",catchType="RETAINED");
rm(dfrSSs);

#--total catch: aggregate over maturity state, shell condition
#----"all EBS" info is available for all years
dfrSSs = lstCF$dfrTC_SSs_inp |> 
            dplyr::filter(area=="all EBS") |>
            dplyr::group_by(fishery,year,sex) |> 
            dplyr::summarize(ss=wtsUtilities::Sum(ss)) |> 
            dplyr::ungroup() |> 
            dplyr::select(fishery,year,x=sex,ss);
#----size comps
dfrTZCs = lstCF$dfrTC_ZCs_ByFAYXMS |> 
            dplyr::filter(area=="all EBS") |>
            dplyr::select(fishery,year,x=sex,m=maturity,s=`shell condition`,z=size,N=abundance) |>
            dplyr::group_by(fishery,year,x,z) |> 
            dplyr::summarize(value=wtsUtilities::Sum(N)) |> 
            dplyr::ungroup() |> 
            dplyr::inner_join(dfrSSs,by=c("fishery","year","x")) |> 
            dplyr::mutate(m="undetermined",s="undetermined",catchType="TOTAL");
rm(dfrSSs);

#--combine retained and total size comps
dfrCZCs = dplyr::bind_rows(dfrRZCs,dfrTZCs) |> 
            dplyr::mutate(season=asmd_ssn,
                          gear="crab pot",
                          fleet=fishery) |> 
            dplyr::select(catchType,fleet,gear,year,season,ss,x,m,s,z,value);
rm(asmd_ssn,dfrRZCs,dfrTZCs);

#------------------------------------------------------------------------------
#--get groundfish fishery info
#----define assumed values
asmd_ssn = 2;  #--assumed season

lstGF = wtsUtilities::getObj(s$fnDataGFs);

#--aggregate and scale input sample sizes
dfrSSs = lstGF$dfrSSs.tnYGAX |> 
                   dplyr::group_by(year,gear,sex) |>
                   dplyr::summarize(ss=sum(ss)) |>
                   dplyr::ungroup() |>
                   akfin.ScaleInputSSs(.ss_scl=lstCF$ss_scl,
                                       .ss_max=lstCF$ss_max) |> 
                   dplyr::select(year,gear,x=sex,ss);

#--aggregate size comps over maturity state, shell condition, add in input sample sizes
dfrZCs = lstGF$dfrZCs.tnYGAXZ |> 
              dplyr::select(year,gear,x=sex,m=maturity,s=`shell condition`,z=size,N) |>
              dplyr::group_by(year,gear,x,z) |> 
              dplyr::summarize(value=wtsUtilities::Sum(N)) |> 
              dplyr::ungroup() |> 
              dplyr::inner_join(dfrSSs,by=c("year","gear","x")) |> 
              dplyr::mutate(m="undetermined",s="undetermined");
#--add sum over known gear types by year to get values for undetermined
dfrTmp = dfrZCs |> dplyr::filter(gear %in% c("fixed","trawl")) |> 
           dplyr::group_by(year,x,m,s,z) |> 
           dplyr::summarize(ss=sum(ss),
                            value=sum(value)) |> 
           dplyr::ungroup() |>
           dplyr::mutate(gear="undetermined");
#--combine
dfrGZCs = dplyr::bind_rows(dfrZCs,dfrTmp) |> 
            dplyr::mutate(season=asmd_ssn,
                          gear=ifelse(gear=="undetermined","all",gear),
                          fleet=paste0("GF",stringr::str_sub(tolower(gear),1,1)),
                          catchType="TOTAL"
                          ) |> 
            dplyr::select(catchType,fleet,gear,year,season,ss,x,m,s,z,value);
rm(asmd_ssn,lstCF,lstGF,dfrSSs,dfrZCs,dfrTmp);

#------------------------------------------------------------------------------
#--get NMFS survey info
#----define assumed values
asmd_ssn = 1;  #--assumed season

lstNMFS = wtsUtilities::getObj(s$fnDataSrvNMFS.DBI);#--design-based indices

#--get sample sizes and size comps, aggregate over shell condition
#--sample sizes for all EBS
dfrSSs = lstNMFS$lstSSs$relSS$EBS |> 
           dplyr::select(year=YEAR,x=SEX,m=MATURITY,s=SHELL_CONDITION,ss=relSS);
#--size comps for all EBS
dfrNZCs = lstNMFS$lstZCs$EBS |> 
            dplyr::select(year=YEAR,x=SEX,m=MATURITY,s=SHELL_CONDITION,z=SIZE,value=totABUNDANCE) |> 
            dplyr::inner_join(dfrSSs,by=c("year","x","m","s")) |> 
            dplyr::group_by(year,x,m,z) |> 
            dplyr::summarize(ss=wtsUtilities::Sum(ss),
                             value=wtsUtilities::Sum(value)) |> 
            dplyr::ungroup() |> 
            dplyr::mutate(catchType="TOTAL",
                          fleet="NMFS",
                          x=tolower(x),
                          m=tolower(m),
                          s="undetermined",
                          gear="survey",
                          season=asmd_ssn) |>
            dplyr::select(catchType,fleet,gear,year,season,ss,x,m,s,z,value);
rm(asmd_ssn,lstNMFS,dfrSSs);

#------------------------------------------------------------------------------
#--get SBS surveys info
#----define assumed values
asmd_ssn = 1;  #--assumed season

lstSBS = wtsUtilities::getObj(s$fnDataSrvSBS.DBI);#--design-based indices

#--get sample sizes and size comps, aggregate over shell condition for BSFRF
#--sample size for SBS areas
dfrSSs = lstSBS$lstSSs_BSFRF$relSS$EBS |> 
           dplyr::select(year=YEAR,x=SEX,m=MATURITY,s=SHELL_CONDITION,ss=relSS);
#--size comps for SBS areas
dfrBZCs = lstSBS$lstZCs_BSFRF$EBS |> 
            dplyr::select(year=YEAR,x=SEX,m=MATURITY,s=SHELL_CONDITION,z=SIZE,value=totABUNDANCE) |> 
            dplyr::inner_join(dfrSSs,by=c("year","x","m","s")) |> 
            dplyr::group_by(year,x,m,z) |> 
            dplyr::summarize(ss=wtsUtilities::Sum(ss),
                             value=wtsUtilities::Sum(value)) |> 
            dplyr::ungroup() |> 
            dplyr::mutate(catchType="TOTAL",
                          fleet="BSFRF",
                          x=tolower(x),
                          m=tolower(m),
                          s="undetermined",
                          gear="survey",
                          season=asmd_ssn) |>
            dplyr::select(catchType,fleet,gear,year,season,ss,x,m,s,z,value);
rm(asmd_ssn,lstSBS,dfrSSs);

#------------------------------------------------------------------------------
#--combine all size comps
dfrZCs = dplyr::bind_rows(dfrCZCs,dfrGZCs,dfrNZCs,dfrBZCs);
rm(dfrCZCs,dfrGZCs,dfrNZCs,dfrBZCs);

#--determine distinct combinations
dfrDCs = dfrZCs |> dplyr::distinct(catchType,fleet,gear,x,m,s);
################################################################################
#--write output to assessment data files for catch by gear type
conn = file("GMACS_InputZCs.txt",open="w")
cat("##------------------------------------------------------------\n",file=conn);
cat("##--SIZE COMPS------------------------------------------------\n",file=conn);
cat("##------------------------------------------------------------\n",file=conn);
cat(1,"#--input format for size comps (0:old format; 1: new format)\n",file=conn);
cat(nrow(dfrDCs),"#--number of size comps dataframes\n",file=conn);
for (rw in 1:nrow(dfrDCs)){
  #--testing rw=1;
  rwDC = dfrDCs[rw,];
  dfrZCsp <- dfrZCs |> 
               dplyr::filter(catchType==rwDC$catchType,
                             fleet==rwDC$fleet,
                             gear==rwDC$gear,
                             x==rwDC$x,m==rwDC$m,s==rwDC$s);
  if (nrow(dfrZCsp)>0){
    writeInput_ZCsDataFrame(conn=conn,
                            dfr=dfrZCsp,
                            fleet_name=rwDC$fleet,
                            fleet_names=NULL,
                            gear=rwDC$gear,
                            sex=rwDC$x,
                            maturity=rwDC$m,
                            shell=rwDC$s,
                            catchType=rwDC$catchType,
                            unitsIn="ONES",
                            unitsOut="MILLIONS");
  }
  rm(rwDC,dfrZCsp);
}#--rw_
close(conn);
