#'
#'@title Function to run GMACS multiple times using jittered initial parameter values.
#'
#'@description This functions runs a GMACS model multiple times, jittering the
#'initial starting values to assess model convergence.
#'
#'@details
#'For each model run, this function creates a shell script ('./tmp.sh') in the
#'current working directory and uses it to run the ADMB version of the GMACS model.
#'Initial model parameters are jittered based on the system clock time as a seed
#'to the random number generator. The seed and final objective function value are
#'saved for each model run in a csv file (the value of out.csv).
#'
#'When all the models requested have been run,
#'the function determines the seed associated with the 1st model run that yielded
#'the smallest value for the objective function.
#'
#'@param path2out - path to top folder for model output
#'@param path2exe - path to model executable
#'@param path2dat - path to model dat, ctl, and prj files
#'@param numRuns    - number of jitter runs to make
#'@param minPhase - phase in which to start optimization
#'@param onlyEvalJitter - flag (T/F) to only evaluate a (previous) set of jitter runs, not make new runs
#'@param out.csv - filename for jittered results
#'@param cleanup - T/F to clean up SOME model output files after each run
#'@param keepFiles - vector of file names to keep, not clean up, after model run
#'@param cleanupAll - T/F to clean up ALMOST ALL model output files after each run
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
runJitter<-function(path2out=".",
                    path2exe='.',
                    path2dat=".",
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
    out.csv<-file.path(path2out,out.csv)

    #--set up run commands----
    cmdLst = getDefaultRunCmds();
    cmdLst$path2exe = path2exe;
    cmdLst$path2dat = path2dat;
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
            p2f<-file.path(path2out,fldr);
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
    }

    #determine row index associated w/ minimum obj fun value
    #read jitter results from file
    if ((!test)&file.exists(out.csv)){
        tbl<-utils::read.csv(out.csv);
        idx<-order(tbl$objFun,abs(tbl$maxGrad));
        best<-tbl$idx[idx[1]];
        seed<-tbl$seed[idx[1]];
        fldr<-tbl$folder[idx[1]];
        if (onlyEvalJitter){parList<-NULL;}

        #re-run case associated with minimum objective function value, save in "best"
        cat("\n\n---Re-running ADMB program for",idx[1],"out of",numRuns,"as best run---\n");
        best<-"best";
        p2f<-file.path(path2out,best);
        cat("---Output folder is '",p2f,"'\n\n",sep='');
        if (!dir.exists(p2f)) dir.create(p2f);
        if (file.exists(file.path(fldr,"gmacs.par"))){
          #--start run with converged par file as pin file
          cat("\tRunning with pin file\n");
          file.copy(file.path(fldr,"gmacs.par"),file.path(p2f,"gmacs.pin"),overwrite=TRUE);
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

