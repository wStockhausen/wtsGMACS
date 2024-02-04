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
dms = c(`crab pot`=s$hm_pot,fixed=s$hm_fxd,trawl=s$hm_trl,undetermined=s$hm_trl);

#-------------------------------------------------------------------------------
#--crab fisheries
#----define assumed values
asmd_ssn = 2;  #--assumed season

#----get crab fisheries info
lstCF = wtsUtilities::getObj(s$fnDataCFsA);#--retained catch assigned to directed fishery

#----retained catch
#------need to filter area to "all EBS", aggregate across maturity and shell condition
#------need to  add in cv's
#year	season	fleet	sex	obs	cv	type	units	mult (??)	effort	discard_mortality
dfrRC = lstCF$dfrRC_ABs_FAYXMS |> 
          dplyr::filter(area=="all EBS") |>
          dplyr::mutate(catchType="RETAINED",season=asmd_ssn,gear="crab pot") |> 
          dplyr::select(catchType,fleet=fishery,gear,year,season,
                        x=sex,m=maturity,s=`shell condition`,abundance,biomass=`biomass (kg)`) |> 
          tidyr::pivot_longer(cols=c("abundance","biomass"),names_to="type",values_to="value") |> 
          dplyr::select(catchType,type,fleet,gear,year,season,x,m,s,value) |> 
          dplyr::group_by(catchType,type,fleet,gear,year,season,x) |> 
          dplyr::summarise(value=wtsUtilities::Sum(value)/MILLION) |>       #--convert to millions or 1000's t
          dplyr::ungroup() |> 
          dplyr::mutate(m="undetermined",s="undetermined");
#------assumed cvs
lstCVs = list(tibble::tibble(type="biomass",cv=0.20,minErr=1,units="THOUSANDS_MT",years=1965:1979),
              tibble::tibble(type="biomass",cv=0.10,minErr=1,units="THOUSANDS_MT",years=1980:2004),
              tibble::tibble(type="biomass",cv=0.05,minErr=1,units="THOUSANDS_MT",years=2005:2023),
              tibble::tibble(type="abundance",cv=0.20,minErr=1,units="MILLIONS",years=1965:1979),
              tibble::tibble(type="abundance",cv=0.10,minErr=1,units="MILLIONS",years=1980:2004),
              tibble::tibble(type="abundance",cv=0.05,minErr=1,units="MILLIONS",years=2005:2023));
dfrCVs = dplyr::bind_rows(lstCVs) |> dplyr::select(!years,year=years);
#------add in cv's
dfrRCp = applyAssumedCVs(dfrRC,dfrCVs) |> 
                dplyr::select(catchType,unitsType=type,fleet,gear,year,season,x,m,s,value,cv);
#------add in effort
#--set missing effort to -1
#--remove years with 0 effort (fishery closed, catch/effort comes from incidental)
dfrRC = dfrRCp |> dplyr::left_join(lstCF$dfrEff |> dplyr::filter(area=="all EBS",fishery=="TCF") |> 
                                                   dplyr::select(fleet=fishery,year,effort),
                                   by=c("fleet","year")) |> 
          dplyr::mutate(effort = ifelse(is.na(effort),-1,effort)) |>   
          dplyr::filter(effort!=0,value!=0);                                    
rm(lstCVs,dfrCVs,dfrRCp);

#----total catch
#------need to filter area to "all EBS", aggregate across area, maturity and shell condition
#------need to  add in cv's
#year	season	fleet	sex	obs	cv	type	units	mult (??)	effort	discard_mortality
dfrTC = lstCF$dfrTC_ABs_FAYXMS |> 
          dplyr::filter(area=="all EBS") |>
          dplyr::mutate(catchType="TOTAL",season=asmd_ssn,gear="crab pot") |> 
          dplyr::select(catchType,fleet=fishery,gear,year,season,
                        x=sex,m=maturity,s=`shell condition`,abundance,biomass=`biomass (kg)`) |> 
          tidyr::pivot_longer(cols=c("abundance","biomass"),names_to="type",values_to="value") |> 
          dplyr::select(catchType,type,fleet,gear,year,season,x,m,s,value) |> 
          dplyr::group_by(catchType,type,fleet,gear,year,season,x) |> 
          dplyr::summarise(value=wtsUtilities::Sum(value)) |> 
          dplyr::ungroup() |> 
          dplyr::mutate(m="undetermined",s="undetermined");
#------assumed cvs
lstCVs = list(tibble::tibble(type="biomass",  cv=0.20,minErr=1,units="THOUSANDS_MT",years=1990:2023),
              tibble::tibble(type="abundance",cv=0.20,minErr=1,units="MILLIONS",    years=1990:2023));
dfrCVs = dplyr::bind_rows(lstCVs);
#------add in cv's
dfrTCp = applyAssumedCVs(dfrTC,dfrCVs) |> 
                dplyr::select(catchType,unitsType=type,fleet,gear,year,season,x,m,s,value,cv);
#------add in effort
#--set missing effort to -1
#--remove years with 0 effort (fishery closed, catch/effort comes from incidental)
dfrTC = dfrTCp |> dplyr::left_join(lstCF$dfrEff |> dplyr::filter(area=="all EBS") |> 
                                                   dplyr::select(fleet=fishery,year,effort),
                                   by=c("fleet","year")) |> 
          dplyr::mutate(effort = ifelse(is.na(effort),-1,effort)) |>   
          dplyr::filter(effort!=0,value!=0);                                    
rm(lstCVs,dfrCVs,dfrTCp);

#--concatenate crab catch data
dfrCC = dplyr::bind_rows(dfrRC,dfrTC);
rm(asmd_ssn,lstCF,dfrRC,dfrTC);

#-------------------------------------------------------------------------------
#--get groundfish fishery info
lstGF = wtsUtilities::getObj(s$fnDataGFs);

#----define assumed values
asmd_ssn = 2;  #--assumed season

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
                            frac_male  =  male/(female+male),
                            type=tolower(type)) |> 
              dplyr::filter(is.finite(frac_female)) |> 
              dplyr::select(year,gear,type,female=frac_female,male=frac_male);
#--extract catch value by year, gear, sex, type, add season, effort
#----input abundance units: ones
#----input biomass units  : kg
dfrABs_YGXT = lstGF$dfrABs.YGAT |> 
                dplyr::group_by(year,gear) |>
                dplyr::summarize(abundance=sum(num)/MILLION,           #--convert to MILLIONS
                                 biomass=sum(`wgt (kg)`)/MILLION) |>   #--convert to 1,000's mt
                dplyr::ungroup() |> 
                tidyr::pivot_longer(cols=c("abundance","biomass"),names_to="type") |> 
                dplyr::inner_join(dfrFC_YG,by=c("year","gear","type")) |> 
                dplyr::mutate(effort=-1,
                              season=asmd_ssn,
                              male=male*value,
                              female=female*value) |> 
                dplyr::select(!value) |> 
                tidyr::pivot_longer(cols=c("male","female"),names_to="x",values_to="value") |> 
                dplyr::select(type,year,season,gear,x,value,effort);
#--add sum over known gear types by year to get values for undetermined
dfrTmp = dfrABs_YGXT |> dplyr::filter(gear %in% c("fixed","trawl")) |> 
           dplyr::group_by(year,season,type,effort,x) |> 
           dplyr::summarize(value=sum(value)) |> 
           dplyr::ungroup() |> 
           dplyr::mutate(gear="undetermined");
#--concatenate gear data
dfrGCp = dplyr::bind_rows(dfrABs_YGXT,dfrTmp) |> 
          dplyr::mutate(fleet=paste0("GF",tolower(stringr::str_sub(gear,1,1))),
                        fleet=ifelse(stringr::str_sub(fleet,3,3)=="u","GFa",fleet),
                        catchType="TOTAL",
                        m="undetermined",
                        s="undetermined");
#------assumed cvs
lstCVs = list(tibble::tibble(type="biomass",  cv=0.20,minErr=1,units="THOUSANDS_MT"),
              tibble::tibble(type="abundance",cv=0.20,minErr=1,units="MILLIONS",   ));
dfrCVs = dplyr::bind_rows(lstCVs);
#--add appropriate cv's
dfrGC = applyAssumedCVs(dfrGCp,dfrCVs) |> 
                dplyr::select(catchType,unitsType=type,year,season,fleet,gear,x,m,s,value,cv,effort);
rm(lstGF,asmd_ssn,lstCVs,dfrFC_YG,dfrABs_YGXT,dfrGCp);

#--concatenate crab and grooundfish catch data
dfrCD = dplyr::bind_rows(dfrCC,dfrGC);
rm(dfrCC,dfrGC)

#determine distinct categories
dfrDCs = dfrCD |> dplyr::distinct(unitsType,catchType,fleet,gear,x,m,s);
################################################################################
#--write output to assessment data files for catch by gear type
conn = file("GMACS_InputCatchData.txt",open="w")
cat("##------------------------------------------------------------\n",file=conn);
cat("##--CATCH DATA------------------------------------------------\n",file=conn);
cat("##------------------------------------------------------------\n",file=conn);
cat(1,"#--input format for catch data (0:old format; 1: new format)\n",file=conn);
cat(nrow(dfrDCs),"#--number of catch dataframes\n",file=conn);
for (rw in 1:nrow(dfrDCs)){
  #--testing rw=1;
  rwDC = dfrDCs[rw,];
  dfrCDp = dfrCD |> dplyr::filter(catchType==rwDC$catchType,
                                   unitsType==rwDC$unitsType,
                                   fleet==rwDC$fleet,
                                   gear==rwDC$gear,
                                   x==rwDC$x,m==rwDC$m,s==rwDC$s);
  if (nrow(dfrCDp)>0){
    writeInput_CatchDataFrame(conn=conn,
                              dfr=dfrCDp,
                              fleet_name=rwDC$fleet,
                              fleet_names=NULL,
                              sex=rwDC$x,
                              maturity=rwDC$m,
                              shell=rwDC$s,
                              catchType=rwDC$catchType,
                              discard_mortality=dms[rwDC$gear],
                              unitsType=rwDC$unitsType,
                              unitsIn=ifelse(toupper(rwDC$unitsType)=="ABUNDANCE",
                                           c("MILLIONS"),c("THOUSANDS_MT")),
                              unitsOut=ifelse(toupper(rwDC$unitsType)=="ABUNDANCE",
                                           c("MILLIONS"), c("THOUSANDS_MT")));
  }
  rm(rwDC,dfrCDp);
}
  # years<-1973:(s$asmtYr-1);
  # for (unitsType in c("BIOMASS","ABUNDANCE")){
  #   #--testing: unitsType = "BIOMASS";
  #   for (x_ in unique(dfrABs_YGXT$sex)){ 
  #     #--for testing: sex_ = "male";
  #     for (g_ in unique(dfrZCs$gear)){
  #       #--testing: g="fixed";
  #       dfrABs <- dfrABs_YGXT |> 
  #                   dplyr::filter(year %in% years,gear==g,type==unitsType,sex==sex_);
  #       gt = ifelse(g=="undetermined","All",stringr::str_to_sentence(g));
  #       flt_nm = paste0("GF_",stringr::str_sub(gt,1,1),stringr::str_sub(sex_,1,1));
  #       if (nrow(dfrABs)>0){
  #         writeInput_CatchDataFrame(conn=stdout(),
  #                                   dfr=dfrABs,
  #                                   fleet_name=flt_nm,
  #                                   fleet_names=NULL,
  #                                   sex=x_,
  #                                   catchType="TOTAL",
  #                                   discard_mortality=dms[g],
  #                                   unitsType=unitsType,
  #                                   unitsIn=ifelse(toupper(unitsType[1])=="ABUNDANCE",
  #                                                c("MILLIONS"),c("THOUSANDS_MT")),
  #                                   unitsOut=ifelse(toupper(unitsType[1])=="ABUNDANCE",
  #                                                c("MILLIONS"), c("THOUSANDS_MT")));
  #       }
  #       rm(dfrABs,gt,flt_nm);
  #     }#--g
  #   }#--sex
  # } #--unitsType
close(conn);

