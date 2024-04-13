#' 
#' @title Run a GMACS model 
#' @description Function to run a GMACS model. 
#' @param runpath - path to folder in which to run model
#' @param runCmds - list of inputs to [runCommands()]
#' @returns NULL (if `test`==TRUE), or a `gmacspar`-class dataframe.
#' 
#' @details If `test == FALSE` and the model successfully converges, the returned 
#' value is a dataframe of class `gmacspar` from [readParFile]. In addition, if 
#' jittering was done, the random number generator seed is prepended as the first 
#' row.
#' 
#' @importFrom dplyr bind_rows 
#' 
#' @export
#' 
runGMACS<-function(runpath='.',
                   runCmds=
                     list(path2exe=".",
                          path2dat='.',
                          pinFile=NULL,
                          os=NULL,
                          hess=FALSE,
                          minPhase=NULL,
                          maxPhase=NULL,
                          mcmc=FALSE,
                          mc.N=1000000,
                          mc.save=1000,
                          mc.scale=1000,
                          jitter=FALSE,
                          iseed=NULL,
                          fullClean=FALSE,
                          cleanup=TRUE,
                          verbose=FALSE),
                   test=FALSE){
                   

  #--start timing----
  stm<-Sys.time();

  plat<-Sys.info()[['sysname']];
  if (tolower(plat)=='darwin'){
      os="osx";
  } else if (tolower(plat)=='windows') {
      os="win";
  } else if (tolower(plat)=='linux') {
      os="osx";
  }
    
  #-switch to run folder (create if necessary)----
  currdir<-getwd();
  on.exit(setwd(currdir));
  if (!file.exists(runpath)) dir.create(runpath,recursive=TRUE)
  setwd(runpath);
  cat("Running GMACS model at \n\t'",runpath,"'.\n",sep="");

  #--create run commands----
  run.cmds<-do.call(getRunCommands,runCmds);
  
  #--run gmacs----
  if (tolower(os)=='win'){
      cat(run.cmds,file="tmp.bat")
      Sys.chmod("tmp.bat",mode='7777')
      if (!test) system("tmp.bat",wait=TRUE);
  } else {
      cat(run.cmds,file="./tmp.sh")
      Sys.chmod("./tmp.sh",mode='7777')
      if (!test) system("./tmp.sh",wait=TRUE);
  }

  #--print timing-related info----
  etm<-Sys.time();
  elt<-etm-stm;
  cat("start time: ")
  print(stm);
  cat("end time: ")
  print(etm);
  cat("elapsed time: ")
  print(elt);

  #--parse par file into dataframe----
  dfr<-NULL;
  if (!test) dfr<-readParFile("gmacs.par");

  #get jitter info
  if (!test){
      if (runCmds$jitter&(!is.null(dfr))) {
        seed = as.numeric(readLines("jitter.txt"));
        dfr = dplyr::bind_rows(tibble::tibble(name="jitter_seed",value=seed),
                               dfr);
      }
  }#!test
  return(dfr);
}
#runGMACS("testing/example_run")
