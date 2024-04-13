#'
#'@title Generate run commands for a GMACS model run
#'
#'@description Function to generate a script to make a GMACS2 model run.
#'
#'@param path2exe - path to gmacs executable
#'@param path2dat - path to folder with input dat, data, ctl, and prj files
#'@param pinFile - if not NULL, the path to the pin file used to initialize the parameters
#'@param os - 'win', 'mac', 'osx', or 'linux'
#'@param hess - flag (T/F) to calculate the hessian
#'@param minPhase - start phase (or NULL) for minimization calculations
#'@param maxPhase - last phase (or NULL) for minimization calculations
#'@param mcmc - flag (T/F) to do mcmc calculations
#'@param mc.N - number of mcmc iterations to do
#'@param mc.save - number of iterations to skip when saving mcmc calculations
#'@param mc.scale - number of iterations to adjust scale for mcmc calculations
#'@param jitter - flag (T/F) to use jitter initial values
#'@param iseed - value for random number seed (or 0 to use start time)
#'@param fullClean - flag to clean up almost all files (use when making multiple jitter runs)
#'@param cleanup - flag (T/F) to clean up unnecessary files
#'@param verbose - flag (T/F) to print diagnostic info
#' 
#'@returns string for shell script or batch file to run gmacs model.
#' 
#'@details. All files in `path2dat` are copied to the "current" folder. If the 
#'pinfile to used is in this folder, it will be copied into the "current" folder and 
#'only the file name (e.g., 'gmacs.pin') needs to be given. If `pinFile` is NULL, 
#'the model is iniitialized from the ctl file.
#'If \code{cleanup} is TRUE, then .bar, .b0*, .p0*, .r0*, variance,
#'and fimn.log files are deleted.\cr
#'
#'@export
#'
getRunCommands<-function(path2exe='.',
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
                         verbose=FALSE){
  pin = TRUE;
  if (is.null(pinFile)) pin = FALSE;
  if (is.null(iseed)) iseed = 0;
  
  if (is.null(os)) {
    plat<-Sys.info()[['sysname']];
    if (tolower(plat)=='darwin'){
        os="osx";
    } else if (tolower(plat)=='windows') {
        os="win";
    } else if (tolower(plat)=='linux') {
        os="osx";
    }
  }
  
    exe = "gmacs";
    noexepath<-FALSE;
    if ((path2exe=='.')||(path2exe=='./')||(path2exe=='.\\')||(path2exe=="")) noexepath=TRUE;
    nodatpath<-FALSE;
    if ((path2dat=='.')||(path2dat=='./')||(path2dat=='.\\')||(path2dat=="")) nodatpath=TRUE;
    echo.on <-"echo on";
    echo.off<-"echo off";
    cln<-"";
    if (cleanup) {
        cln<-"del &&exe.bar
            del &&exe.b0*
            del &&exe.p0*
            del &&exe.r0*
            del *.rept
            del variance
            del *.tmp
            del fmin.log";
        if (fullClean){
            fullcln<-"del &&model1
                      del *.rep
                      del Gmacsall.out
                      del *.rep1";
            cln<-paste0(cln,"\n",fullcln);
        }
    }
    rn.mcmc<-'';
    cpyexe<-'';
    cpydat<-'';
    opts <- " -rs -nox &&mnPhs &&mxPhs &&mcmc &&nohess &&jitter &&pin !!pinFile";
    if (tolower(os)=='win'){
        ##--windows commands----
        model1<-paste(exe,'exe',sep='.');
        if (!noexepath) cpyexe<-"copy &&path2exe &&model1";
        if (!nodatpath) cpydat<-"copy &&path2dat\\*.*";
        rn.mdl<-paste("&&exe",opts);
        if (mcmc) rn.mcmc<-"&&exe -mceval";
        ##cln is correct for 'win', so do nothing
        run.cmds<-paste(echo.on,cpyexe,cpydat,rn.mdl,rn.mcmc,cln,sep="\n");
        path2exe<-gsub("/","\\",file.path(path2exe,model1),fixed=TRUE);
        path2dat<-gsub("/","\\",file.path(path2dat),fixed=TRUE);
    } else if (tolower(os) %in% c('mac','osx','linux')){
        ##--osx commands----
        model1<-exe;
        if (!noexepath) cpyexe<-"cp &&path2exe ./&&exe";
        if (!nodatpath) cpydat<-"cp &&path2dat/* ./";
        rn.mdl<-paste("./&&exe",opts);
        if (mcmc) rn.mcmc<-"./&&exe  -mceval ";
        if (cleanup) cln<-gsub("del ","rm ",cln,fixed=TRUE);
        cdr<-paste('DIR="$( cd "$( dirname "$0" )" && pwd )"','cd ${DIR}',sep='\n');
        rme = paste(paste0("if test -e ",exe," ;"),
                    "then",paste0("  rm ",exe),"fi",sep="\n");
        run.cmds<-paste("#!/bin/sh",echo.on,cdr,cpyexe,cpydat,rn.mdl,rn.mcmc,cln,sep="\n");
        path2exe<-file.path(path2exe,model1);
    }
    ##--create final command string----
    if (!noexepath) run.cmds<-gsub("&&path2exe",  path2exe,  run.cmds,fixed=TRUE);
    if (!nodatpath) run.cmds<-gsub("&&path2dat",  path2dat,  run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&model1",model1,run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&exe",   exe,   run.cmds,fixed=TRUE);
    str<-''; if (pin) str<-"-pin";
    run.cmds<-gsub("&&pin",str,run.cmds,fixed=TRUE);
    str<-''; if (!is.null(pinFile)) str<-pinFile;
    run.cmds<-gsub("!!pinFile",str,run.cmds,fixed=TRUE);
    str<-''; if (!hess) str<-"-nohess";
    run.cmds<-gsub("&&nohess",str,run.cmds,fixed=TRUE);
    str<-''; if (jitter) str<-paste("-jitter",iseed);
    run.cmds<-gsub("&&jitter",str,run.cmds,fixed=TRUE);
    str<-''; if (mcmc) str<-paste("-mcmc",mc.N,"-mcsave",mc.save,"-mcscale",mc.scale);
    run.cmds<-gsub("&&mcmc",str,run.cmds,fixed=TRUE);
    str<-''; if (!is.null(minPhase)) str<-paste0("-phase ",minPhase);
    run.cmds<-gsub("&&mnPhs",str,run.cmds,fixed=TRUE);
    str<-''; if (!is.null(maxPhase)) str<-paste0("-maxph ",maxPhase);
    run.cmds<-gsub("&&mxPhs",str,run.cmds,fixed=TRUE);

    if (verbose) cat("Run commands:\n",run.cmds,"\n\n");

    return(run.cmds);
}

#' 
#' @title Get default run commands
#' @description Function to get default run commands.
#' @returns list with default input values for [getRunCommands] 
#' @details Use the returned list (after modification, as necessary) as 
#' input to [getRunCommands]:\cr
#' ```
#' deflst = getDefaultRunCmds(); 
#' #..update defaults...
#' runcmds = do.call(deflst,getRunCommands);
#' ````
#' 
#' @export
#' 
getDefaultRunCmds<-function(){
  rnCmds = list(path2exe=".",
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
                verbose=FALSE);
  return(rnCmds);
}
# str = getRunCommands(os='osx',
#                      path2exe='path-to-exe',
#                      path2dat='path-to-dat',
#                      pinFile="path-to-pin/gmacs.pin",
#                      hess=TRUE,
#                      minPhase=1,
#                      maxPhase=10,
#                      mcmc=FALSE,
#                      mc.N=1000000,
#                      mc.save=1000,
#                      mc.scale=1000,
#                      jitter=FALSE,
#                      iseed=5,
#                      fullClean=TRUE,
#                      cleanup=TRUE,
#                      verbose=FALSE)
# cat(str)

