#' 
#' @title Determine best GMACS jitter run among a series of runs 
#' @description Function to determine best GMACS jitter run among a series of runs. 
#' @param fldrs - character vector of folder names to search over 
#' @param verbose - flag to print plot of objective function results 
#' @returns list with elements `dfrJtr`, `plt`,`minObjFun`, and `maxGrad`. 
#' @details The "jitterResults.csv" file in each folder is read in, concatenated 
#' into a single dataframe, and arranged by order of the objective function value 
#' for each run. Details, including the folder name, of the "best" model are 
#' given in a message. A count of the number of model runs with "similar" 
#' objective function values (closer than 0.001 to the minimum) is also included. 
#' In addition, a plot is provided of maximum gradient vs. objective function 
#' value for all model runs. 
#' 
#' @import dplyr
#' @import ggplot2 
#' @importFrom readr read_csv 
#' 
#' @export 
#' 
findBestJitterRun<-function(fldrs,verbose=FALSE){
  #--identify best run across folders----
  lst = list();
  for (fldr in fldrs){
    #--testing: fldr = fldrs[1]
    dfrCSV = readr::read_csv(file.path(fldr,"jitterResults.csv"));
    lst[[fldr]] = dplyr::bind_cols(dfrCSV,parent=fldr);
  }
  dfrJtr = dplyr::bind_rows(lst);
  
  #--identify best model run----
  dfrJtr = dfrJtr |> dplyr::arrange(objFun,maxGrad);
  str = paste(names(dfrJtr),dfrJtr[1,],sep=": ",collapse="\n")
  message(str)
  
  #--identify min objective function value and 
  #--count of runs with "similar" values
  minOF = dfrJtr$objFun[1];
  cnt = sum(dfrJtr$objFun-minOF<1.0e-3);
  
  p = ggplot(dfrJtr,aes(x=objFun,y=maxGrad)) + 
      geom_point(shape=1) + 
      labs(subtitle=paste0("minimum objective function: ",minOF,
                           "\nmaximum gradient        : ",dfrJtr$maxGrad[1],
                           "\nnumber of similar runs  : ",cnt),
           x = "Objective Function Value",
           y = "maximum gradient") +
      wtsPlots::getStdTheme();
  if (verbose) (print(p));
  return(list(dfr=dfrJtr,plt=p,minObjFun=minOF,maxGrad=dfrJtr$maxGrad[1]))
}
# #--folders with; jitter runs----
# fldrs = paste0("runJitter",c("A","B","C","D"));
# #--find best  
# resJitter = findBestJitterRun(fldrs,verbose=TRUE);

