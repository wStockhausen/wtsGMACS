#' 
#' @title Plot Gmacs size composition fits 
#' @description Function to plot fits to size compositions. 
#' @param dfr - dataframe with observed/predicted size comps from Gmacs rep1 file(s)
#' @param ncols - number of panel columns for figure
#' @param subtitle - (sub) title for figure (default="")
#' @param xlab - x-axis label (default="size (mm CW)")
#' @param ylab - y-axis label (default="proportion")
#' @param plot1stObs - flag to plot only first set of observations (default=TRUE)
#' @param useBars - use bars for observed size comps (default=FALSE)
#' @param usePins - use only pins for observed size comps (default=FALSE)
#' @param usePinsAndPts - use pins and points for observed size comps (default=TRUE)
#' @param useLines - use lines for predicted size comps (default=TRUE)
#' @param usePoints - use points for predicted size comps (default=FALSE)
#' @param pinSize - pin size used for observed comps (default=0.2)
#' @param lineSize - line size used for predicted comps (default=0.5)
#' @param pointSize - size of points used for predicted comps (default=0.4) 
#' @param alpha - transparency (default=1) 
#' @return [ggplot2::ggplot()] object 
#' 
#' @details Input dataframe should represent a (possibly series of concatenated) 
#' size composition fits dataframe(s) with columns 
#' \itemize{
#'  \item {case - name(s) identifying case(s) for labels}
#'  \item {type - possible values are 'predicted' or 'observed'}
#'  \item {y - year}
#'  \item {z - size}
#'  \item {val - value}
#' }. 
#' 
#' @import dplyr
#' @import ggplot2 
#' @importFrom wtsPlots getStdTheme
#' @export 
#' 
compareFitsZCsAlt<-function(dfr,
                                  ncols=NULL,
                                 subtitle="",
                                 xlab="size (mm CW)",
                                 ylab="proportion",
                                 useBars=FALSE,
                                 usePins=FALSE,
                                 usePinsAndPts=TRUE,
                                 useLines=TRUE,
                                 usePoints=FALSE,
                                 plot1stObs=TRUE,
                                 pinSize=0.2,
                                 lineSize=0.5,
                                 pointSize=0.4,
                                 alpha=1){
  if (!("case" %in% names(dfr))) dfr$case = " ";
  if (is.null(ncols)) ncols = round(sqrt(length(unique(dfr$y))));
  sub_=subtitle;
  dfro = dfr |> dplyr::filter(type=="observed");
  dfrp = dfr |> dplyr::filter(type=="predicted");
  p  = ggplot(data=dfrp)
  if (plot1stObs) {
      if (useBars)       p = p + geom_bar(aes(x=z,y=val),         fill="black",  data=dfro,stat="identity",position='identity',alpha=0.5)
      if (usePins||
          usePinsAndPts) p = p + geom_linerange(aes(x=z,ymax=val),colour="black",data=dfro,stat="identity",position='identity',ymin=0.0,linewidth=pinSize)
      if (usePinsAndPts) p = p + geom_point(aes(x=z,y=val),       colour="black",data=dfro,stat="identity",position='identity',size=0.5,alpha=1)
  } else {
      if (useBars)       p = p + geom_bar(aes(x=z,y=val,fill=case),           data=dfro,stat="identity",position='identity',alpha=0.5)
      if (usePins||
          usePinsAndPts) p = p + geom_linerange(aes(x=z,ymax=val,colour=case),data=dfro,stat="identity",position='identity',ymin=0.0,linewidth=pinSize)
      if (usePinsAndPts) p = p + geom_point(aes(x=z,y=val,colour=case),       data=dfro,stat="identity",position='identity',size=0.5,alpha=1)
  }
  if (useLines)  p = p + geom_line(aes(x=z,y=val,colour=case),            data=dfrp,linewidth=lineSize,alpha=alpha)
  if (usePoints) p = p + geom_point(aes(x=z,y=val,colour=case,shape=case),data=dfrp,size=pointSize)
#  p <- p + ylim(0,rng[2])
  p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
  p <- p + labs(x=xlab,y=ylab)
  p <- p + facet_wrap(~y,ncol=ncols,dir='v')
  p <- p + labs(subtitle=sub_)
  p <- p + guides(fill=guide_legend('observed'),colour=guide_legend('predicted'),shape=guide_legend('predicted'))
  p = p + wtsPlots::getStdTheme();
  return(p)
}


