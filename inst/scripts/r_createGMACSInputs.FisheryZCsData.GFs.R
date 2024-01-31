#--write groundfish fisheries size composition data in GMACS dat file format 
#----by gear type and aggregated across gear types
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

#--output folder
# dirOut<-s$dirTIFs;
# if(!dir.exists(dirOut)) dir.create(dirOut);

#--define discard mortality rates
dms = c(pot=s$hm_pot,fixed=s$hm_fxd,trawl=s$hm_trl,undetermined=s$hm_trl);

#--define assumed values
asmd_ssn = 2;  #--assumed season

#--get groundfish fishery info
lstGF = wtsUtilities::getObj(s$fnDataGFs);

#--get crab fisheries info (need for input sample sizes)
lstCF = wtsUtilities::getObj(s$fnDataCFsA);

#--calc input sample sizes
dfrSSs_inp.YGX = lstGF$dfrSSs.tnYGAX |> 
                   dplyr::group_by(year,gear,sex) |>
                   dplyr::summarize(ss=sum(ss)) |>
                   dplyr::ungroup() |>
                   akfin.ScaleInputSSs(.ss_scl=lstCF$ss_scl,
                                       .ss_max=lstCF$ss_max) |> 
                   dplyr::select(year,gear,x=sex,ss);

#--aggregate size comps, add in input sample sizes
#----aggregate over maturity state, shell condition
dfrZCs = lstGF$dfrZCs.tnYGAXZ |> 
              dplyr::select(year,gear,x=sex,m=maturity,s=`shell condition`,z=size,N) |>
              dplyr::group_by(year,gear,x,z) |> 
              dplyr::summarize(value=wtsUtilities::Sum(N)) |> 
              dplyr::ungroup() |> 
              dplyr::inner_join(dfrSSs_inp.YGX,by=c("year","gear","x")) |> 
              dplyr::mutate(m="undetermined",s="undetermined");
#--add sum over known gear types by year to get values for undetermined
dfrTmp = dfrZCs |> dplyr::filter(gear %in% c("fixed","trawl")) |> 
           dplyr::group_by(year,x,m,s,z) |> 
           dplyr::summarize(ss=sum(ss),
                            value=sum(value)) |> 
           dplyr::ungroup() |>
           dplyr::mutate(gear="undetermined");
dfrZCs = dplyr::bind_rows(dfrZCs,dfrTmp) |> 
           dplyr::mutate(season="&&season");

################################################################################
#--write output to assessment data files for catch by gear type
conn = file("ZCs_GroundfishFisheries.txt",open="w")
  years<-1973:(s$asmtYr-1);
    for (sex_ in unique(dfrZCs$x)){ 
      #--for testing: sex_ = "male";
      for (g in c("fixed","trawl","undetermined")){
        #--testing: g="fixed";
        dfrZCsp <- dfrZCs |> 
                     dplyr::filter(year %in% years,gear==g,x==sex_);
        gt = ifelse(g=="undetermined","All",stringr::str_to_sentence(g));
        flt_nm = paste0("GF_",stringr::str_sub(gt,1,1),stringr::str_sub(sex_,1,1));
        # fn <- file.path(dirOut,paste("Data.Fishery",unitsType,s$asmtYr,flt_nm,"inp",sep="."));
        if (nrow(dfrZCsp)>0){
          writeInput_ZCsDataFrame(conn=conn,
                                  dfr=dfrZCsp,
                                  fleet_name=flt_nm,
                                  fleet_names=NULL,
                                  gear=g,
                                  sex=sex_,
                                  catchType="TOTAL",
                                  unitsIn="ONES",
                                  unitsOut="MILLIONS");
        }
        rm(dfrZCsp,gt,flt_nm);
      }#--g
    }#--sex
close(conn);
