#'
#'@title Create a `gmacspar` dataframe from an ADMB par file.
#'
#'@description Function to create a `gmacspar` dataframe from an ADMB par file.
#'
#'@param parfn = filename of par file
#'
#'@returns a `gmacspar` object (a [tibble] dataframe of class 'gmacspar') corresponding to the par file (or NULL if file does not exist)
#'
#'@details The returned tibble as columns
#'\itemize{ 
#'  \item{name - parsed parameter name (or 'number of parameters','objective function','max gradient')}
#'  \item{value - parameter estimate or convergence quantity value
#'}
#'}
#' The ADMB convergence information is given in the first three rows of the tibble, 
#' associated with the names 'number of parameters','objective function',and 'max gradient'. 
#' 
#' Note that this is not the same format as returned by [gmr::readGMACSpar()], which 
#' contains more structured information based on the model run's dat and ctl files.
#' 
#' @importFrom tibble tibble 
#' 
#'@export
#' 
readParFile<-function(parfn=NULL){
    if (!file.exists(parfn)) return(NULL);
    
    r1<-readLines(con=parfn);
    
    #parse first line
    str <- strsplit(r1[1],'[[:blank:]]');#split based on blanks
    num <- as.numeric(str[[1]]);         #convert to numeric, non-numerics will be NAs
    objfun <- num[!is.na(num)];          #extract numeric elements (num par, obj fun, max gradient)
    names(objfun)<-c('number of parameters','objective function','max gradient');
    dfr<-tibble::tibble(name=names(objfun),value=objfun)
    
    #parse remaining lines
    nr<-length(r1);
    for (r in 1:((nr-1)/2)){
        nam  <- gsub('[^[:alnum:]_]','',r1[2*r],perl=TRUE);#get parameter name
        str  <- gsub('[^[:digit:][:blank:].]','',r1[2*r+1],perl=TRUE);#remove all characters except numbers and blanks
        str  <- str<-strsplit(str,'[[:blank:]]');#split str based on blanks
        str  <- str[[1]][str[[1]]!=''];#remove empty array elements
        valu <- as.numeric(str);
        nams <- nam;
        if (length(valu)>1) nams <- paste(nam,'[',1:length(valu),']',sep='');
        dfr <- rbind(dfr,data.frame(name=nams,value=valu,stringsAsFactors=FALSE));
    }
    
    class(dfr)<-c("gmacspar",class(dfr));
    
    return(dfr)
}
#dfr = readParFile("testing/example_results/gmacs.par")



