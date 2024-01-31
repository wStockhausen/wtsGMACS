#--write groundfish fisheries catch data in GMACS dat file format 
#----by gear type and aggregated across gear types
options(stringsAsFactors=FALSE);
require(tcsamFisheryDataAKFIN);
require(tcsamFunctions);
require(wtsSizeComps);
require(wtsUtilities);
source("R/getConstants.R")
source("R/writeInput_CatchDataFrame.R")

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
dfrCVs  = tibble::tribble(~type,~cv,~minErr,~units,
                           "ABDUNDANCE",0.2,1,"MILLIONS",
                           "BIOMASS",   0.2,1,"THOUSANDS_MT");#--assumed cvs

#--get groundfish fishery info
lstGF = wtsUtilities::getObj(s$fnDataGFs);

#--get crab fisheries info
lstCF = wtsUtilities::getObj(s$fnDataCFsA);

#--calc fraction of catch by sex from size comps for both abundance and biomass
dfrFC_YG = dplyr::bind_rows(
              lstGF$dfrZCs.tnYGAXZ |> dplyr::mutate(type="ABUNDANCE"),
              lstGF$dfrZCs.tnYGAXZ |> dplyr::mutate(N=N*calc.WatZ(size,sex,maturity="UNDETERMINED"),
                                                    type="BIOMASS")) |>
              dplyr::group_by(year,gear,type,sex) |> 
              dplyr::summarize(value=wtsUtilities::Sum(N)) |> 
              dplyr::ungroup() |>
              tidyr::pivot_wider(id_cols=c("year","gear","type"),
                                 names_from="sex",
                                 values_from="value") |> 
              dplyr::mutate(frac_female=female/(female+male),
                            frac_male  =  male/(female+male)) |> 
              dplyr::filter(is.finite(frac_female)) |> 
              dplyr::select(year,gear,type,female=frac_female,male=frac_male);
#--extract catch value by year, gear, sex, type, add season, effort
#----input abundance units: ones
#----input biomass units  : kg
dfrABs_YGXT = lstGF$dfrABs.YGAT |> 
                dplyr::group_by(year,gear) |>
                dplyr::summarize(ABUNDANCE=sum(num)/MILLION,           #--convert to MILLIONS
                                 BIOMASS=sum(`wgt (kg)`)/MILLION) |>   #--convert to 1,000's mt
                dplyr::ungroup() |> 
                tidyr::pivot_longer(cols=c("ABUNDANCE","BIOMASS"),names_to="type") |> 
                dplyr::inner_join(dfrFC_YG,by=c("year","gear","type")) |> 
                dplyr::mutate(effort=0,
                              season=asmd_ssn,
                              male=male*value,
                              female=female*value) |> 
                dplyr::select(!value) |> 
                tidyr::pivot_longer(cols=c("male","female"),names_to="sex",values_to="value") |> 
                dplyr::select(year,gear,sex,season,type,value,effort);
#--add sum over known gear types by year to get values for undetermined
dfrTmp = dfrABs_YGXT |> dplyr::filter(gear %in% c("fixed","trawl")) |> 
           dplyr::group_by(year,sex,season,type,effort) |> 
           dplyr::summarize(value=sum(value)) |> 
           dplyr::ungroup() |> 
           dplyr::mutate(gear="undetermined");
dfrABs_YGXT = dplyr::bind_rows(dfrABs_YGXT,dfrTmp)
#--add appropriate cv's
dfrABs_YGXT = applyAssumedCVs(dfrABs_YGXT,dfrCVs) |> 
                dplyr::select(year,gear,sex,season,type,effort,value,cv);
################################################################################
#--write output to assessment data files for catch by gear type
  years<-1973:(s$asmtYr-1);
  for (unitsType in c("ABUNDANCE","BIOMASS")){
    #--testing: unitsType = "ABUNDANCE";
    for (sex_ in unique(dfrABs_YGXT$sex)){ 
      #--for testing: sex_ = "male";
      for (g in c("fixed","trawl","undetermined")){
        #--testing: g="fixed";
        dfrABs <- dfrABs_YGXT |> 
                    dplyr::filter(year %in% years,gear==g,type==unitsType,sex==sex_);
        gt = ifelse(g=="undetermined","All",stringr::str_to_sentence(g));
        flt_nm = paste0("GF_",stringr::str_sub(gt,1,1),stringr::str_sub(sex_,1,1));
        # fn <- file.path(dirOut,paste("Data.Fishery",unitsType,s$asmtYr,flt_nm,"inp",sep="."));
        if (nrow(dfrABs)>0){
          writeInput_CatchDataFrame(conn=stdout(),
                                    dfr=dfrABs,
                                    fleet_name=flt_nm,
                                    fleet_names=NULL,
                                    sex=sex_,
                                    catchType="TOTAL",
                                    discard_mortality=dms[g],
                                    unitsType=unitsType,
                                    unitsIn=ifelse(toupper(unitsType[1])=="ABUNDANCE",
                                                 c("MILLIONS"),c("THOUSANDS_MT")),
                                    unitsOut=ifelse(toupper(unitsType[1])=="ABUNDANCE",
                                                 c("MILLIONS"), c("THOUSANDS_MT")));
        }
        rm(dfrABs,gt,flt_nm);
      }#--g
    }#--sex
  } #--unitsType

