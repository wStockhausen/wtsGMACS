#'
#'@title Function to run GMACS multiple times using jittered initial parameter values.
#'
#'@description This functions runs a GMACS model multiple times, jittering the
#'initial starting values to assess model convergence.
#'
#'@details
#'For each model run, this function creates a shell script ('./tmp.sh') in the
#'current working directory and uses it to run the ADMB version of the GMACS model.
#'Initial model parameters with phase > 0 are jittered based on the system clock time as a seed
#'to the random number generator. If a pin file is given (`pinFile`), the values for 
#'model parameters that are *not* estimated are taken from that file, otherwise they are 
#'taken from the initial values specified in the ctl file. The seed and final objective function value are
#'saved for each model run in a csv file (the value of out.csv).
#'
#'When all the models requested have been run,
#'the function determines the seed associated with the 1st model run that yielded
#'the smallest value for the objective function.
#'
#'@param inp_fns - name(s) of data, ctl, prj files (default = NULL: must supply one or all)
#'@param pinFile - pin file to set values for variables with phase < 0 (not estimated) (default: NULL)
#'@param exe - name of gmacs executable (without extension) (default='gmacs')
#'@param path2exe - path to model executable (default: '.')
#'@param path2dat - path to model dat, ctl, and prj files (default: '.')
#'@param path2pin - path to pin file (default: `path2dat`)
#'@param path2out - path to top folder for model output (default: '.')
#'@param numRuns    - number of jitter runs to make
#'@param minPhase - phase in which to start optimization
#'@param onlyEvalJitter - flag (T/F) to only evaluate a (previous) set of jitter runs, not make new runs
#'@param out.csv - filename for jittered results
#'@param calcOFL - does nothing at present (default=FALSE)
#'@param calcOFLJitter - does nothing at present (default=FALSE)
#'@param saveResults - (default=FALSE)
#'@param cleanup - T/F to clean up SOME model output files after each run
#'@param keepFiles - vector of file names to keep, not clean up, after model run
#'@param cleanupAll - T/F to clean up ALMOST ALL model output files after each run
#'@param test - T/F to run as a test
#'
#'@returns - list w/ 4 elements:
#' \itemize{
#'  \item{imx  - index of (1st) smallest value for the objective function}
#'  \item{seed - seed resulting in the smallest objective function}
#'  \item{par  - dataframe with par results from run w/ smallest objective function}
#'  \item{objFuns - vector of objective function values from all model runs}
#'  \item{parList - list of par dataframes for each model run}
#'}
#'
#'@export
#'
runJitter<-function(inp_fns=NULL,
                    pinFile=NULL,
                    exe="gmacs",
                    path2exe='.',
                    path2dat=".",
                    path2pin=path2dat,
                    path2out=".",
                    numRuns=3,
                    minPhase=1,
                    onlyEvalJitter=FALSE,
                    out.csv='jitterResults.csv',
                    calcOFL=FALSE,
                    calcOFLJitter=FALSE,
                    saveResults=FALSE,
                    cleanup=TRUE,
                    keepFiles=c("tmp.sh","gmacs.par"),
                    cleanupAll=FALSE,
                    test=TRUE){
    #start timing
    stm<-Sys.time();

    #set up output
    out.csv<-file.path(path2out,out.csv);

    #--set up run commands----
    cmdLst = wtsGMACS::getDefaultRunCmds();
    cmdLst$inp_fns  = inp_fns;
    cmdLst$pinFile  = pinFile;
    cmdLst$exe      = exe;
    cmdLst$path2exe = normalizePath(path2exe);#--written to batch file/shell script
    cmdLst$path2dat = normalizePath(path2dat);#--written to batch file/shell script
    cmdLst$path2pin = normalizePath(path2pin);#--written to batch file/shell script
    cmdLst$minPhase = minPhase;
    cmdLst$jitter   = TRUE;
    cmdLst$iseed    = 0;  #-use the system time as seed 
    cmdLst$cleanup  = cleanup;
    
    #run models
    parList<-list();
    if (!onlyEvalJitter){
        rc<-0;
        for (r in 1:numRuns){
            cat("\n\n---running GMACS program for",r,"out of",numRuns,"---\n\n");
            fldr<-paste('run',wtsUtilities::formatZeros(r,width=max(2,ceiling(log10(numRuns)))),sep='');
            p2f<-normalizePath(file.path(path2out,fldr));#--normalization shouldn't hurt
            par<-runGMACS(runpath=p2f,
                          runCmds=cmdLst,
                          test=test);
            if (!is.null(par)){
                rc<-rc+1;
                objFun  <-par$value[par$name=='objective function'];
                seed    <-par$value[par$name=='jitter_seed'];
                maxgrad <-par$value[par$name=='max gradient'];
                tbl<-data.frame(idx=r,objFun=objFun,maxGrad=maxgrad,seed=seed,folder=fldr);
                if (file.exists(out.csv)) {
                    utils::write.table(tbl,file=out.csv,sep=",",col.names=FALSE,row.names=FALSE,append=TRUE)
                } else {
                    #create out.csv file
                    utils::write.table(tbl,file=out.csv,sep=",",col.names=TRUE,row.names=FALSE,append=FALSE)
                }
            }#par not NULL
            parList[[fldr]]<-par;#--save output
            if (cleanupAll){
                cat("Cleaning up 'all' files\n\n")
                fns<-list.files(path=p2f,full.names=FALSE);#vector of file names in folder p2f
                rmf<-fns[!(fns %in% keepFiles)];           #vector of file names in folder p2f to remove
                file.remove(file.path(p2f,rmf));           #leave only files to keep
            }
        }
    }#--run models

    #determine row index associated w/ minimum obj fun value
    #read jitter results from file
    if ((!test)&&file.exists(out.csv)){
        tbl<-utils::read.csv(out.csv) |> dplyr::arrange(objFun,abs(maxGrad));
        readr::write_csv(tbl,file=out.csv);  #--write out ordered results
        best<-tbl$idx[1];
        seed<-tbl$seed[1];
        fldr<-tbl$folder[1];
        if (onlyEvalJitter){parList<-NULL;}

        #re-run case associated with minimum objective function value, save in "best"
        cat("\n\n---Re-running ADMB program for",tbl$idx[1],"out of",numRuns,"as best run---\n");
        best<-"best";
        p2f<-normalizePath(file.path(path2out,best));
        cat("---Output folder is '",p2f,"'\n\n",sep='');
        if (!dir.exists(p2f)) dir.create(p2f);
        if (file.exists(file.path(fldr,"gmacs.par"))){
          #--start run with converged par file as pin file
          cat("\tRunning with pin file\n");
          file.copy(file.path(fldr,"gmacs.par"),file.path(p2f,"gmacs.pin"),overwrite=TRUE);
          cmdLst$path2pin = ".";#--need to use just-copied pin, not original pin
          cmdLst$pinFile = "gmacs.pin"; #--file.path(fldr,"gmacs.par");
          cmdLst$jitter = FALSE;
        } else {
          #--start from scratch using seed
          cat("\tRunning with jitter seed\n");
          cmdLst$jitter = TRUE;
          cmdLst$iseed  = seed;
        }
        cmdLst$hess=TRUE;#--do hessian to estimate std errors
        par<-runGMACS(runpath=p2f,
                      runCmds=cmdLst,
                      test=FALSE);

        #print timing-related info
        etm<-Sys.time();
        elt<-etm-stm;
        cat("start time: ")
        print(stm);
        cat("end time: ")
        print(etm);
        cat("elapsed time: ")
        print(elt);

        #return output
        return(list(imn=best,seed=seed,par=par,objFuns=tbl,parList=parList));
    } else {
        #print timing-related info
        etm<-Sys.time();
        elt<-etm-stm;
        cat("start time: ")
        print(stm);
        cat("end time: ")
        print(etm);
        cat("elapsed time: ")
        print(elt);

        return(NULL);
    }
}

