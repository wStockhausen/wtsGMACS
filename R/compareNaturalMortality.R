#'
#' @title Create a [ggplot2] object to compare tcsam02 and gmacs natural mortality rates
#' @description Function to create a [ggplot2] object to compare tcsam02 and gmacs
#' selectivity/retention functions. Can also be used to plot either model type.
#' @param dfr - dataframe to plot
#' @param ylab - y-axis label
#' @param plotZ - flag to plot size-specific mortality rates (not implemented yet)
#' @return [ggplot2] plot object
#'
#' @details The input dataframe should at least have columns named "case" (model case), "y" (year),
#' "z" (size), and "val" (value). Bars/curves are colored by `case`.
#' 
#' The gmacs `case` should start with "gmacs".
#'
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import tidyr
#' @import tidyselect
#' @import wtsPlots
#' @import wtsUtilities
#'
#' @export
#'
compareNaturalMortality<-function(dfr,ylab="M",plotZ=FALSE){
  ##--drop unnecessary columns----
  cols = names(dfr)[names(dfr) %in% c("case","y","x","m","s","z","val")];
  ncls = length(cols);
  tmp0 = dfr |> dplyr::select(tidyselect::all_of(cols));
  ##--collect values and create labels
  tmp1 = wtsUtilities::collectValuesByGroup(tmp0,collect="y",names_from="z",values_from="val");
  tmp1 = tmp1 |> dplyr::rowwise() |>
                 dplyr::mutate(ylab=wtsUtilities::collapseIntegersToString(y),.before=y) |>
                 dplyr::ungroup();
  ##--cross cases----
  tmp2  = tmp1 |> dplyr::select(tidyselect::any_of(1:ncls));
  tmp2a = tmp2 |> dplyr::cross_join(tmp2);
  tmp3  = tmp2a |> dplyr::filter(!(case.x==case.y),(stringr::str_starts(case.x,"gmacs")));
  if (nrow(tmp3)>0){
    ##--select unique case combinations out of duplicates and self-crosses
    tmp4 = tmp3 |> dplyr::rowwise() |> 
             dplyr::mutate(check=(any(unlist(y.y) %in% unlist(y.x)))) |> 
             dplyr::ungroup() |> dplyr::filter(check) |> 
             dplyr::mutate(grp2=paste(group.x,group.y),
                           ylabs=paste0(ylab.x,"\n",ylab.y),
                           .before=1);
    xcols = names(tmp4)[stringr::str_ends(names(tmp4),".x")]
    ycols = names(tmp4)[stringr::str_ends(names(tmp4),".y")]
    tmp5g = tmp4 |> dplyr::select(tidyselect::any_of(c("grp2","ylabs",xcols)));
    names(tmp5g) = c("grp2","ylabs",stringr::str_remove(xcols,".x"));
    tmp5t = tmp4 |> dplyr::select(tidyselect::any_of(c("grp2","ylabs",ycols)));
    names(tmp5t) = c("grp2","ylabs",stringr::str_remove(ycols,".y"));
    tmp5 = dplyr::bind_rows(tmp5g,tmp5t);
  } else {
    paste("got here");
    tmp5 = tmp2 |> dplyr::mutate(grp2=group,
                                  ylabs=ylab,
                                  .before=1);
  }
  ##--convert to long format, with numeric z's, drop "y" column (don't need it)
  ncls5 = ncol(tmp5);
  tmp6 = tmp5 |> dplyr::inner_join(tmp1) |>
           tidyr::pivot_longer(ncls5+1:(ncol(tmp1)-ncls),names_to="z",values_to="val") |>
           dplyr::mutate(z=as.numeric(z)) |>
           dplyr::select(!y);
  
  ##--make plot----
  dfrp = tmp6 |> dplyr::distinct(grp2,ylabs,ylab,case,x,m,s,val);
  if (length(unique(dfrp$s)) > 1) {
    dfrp = dfrp |> dplyr::mutate(xms=paste0(m," ",x,"\n",s));
  } else {
    dfrp = dfrp |> dplyr::mutate(xms=paste0(m," ",x));
  }
  mx = max(max(dfrp$val),0.23);
  p1 = ggplot(dfrp |> dplyr::filter(m=="immature"),
              aes(x=xms,y=val,colour=case,fill=case)) + 
         geom_col(position="dodge") + 
         geom_hline(yintercept=0.23,linetype=3) + 
         geom_hline(yintercept=0.00,linetype=3) + 
         scale_y_continuous(limits=c(0,mx)) + 
         facet_wrap(~ylab,ncol=1) + 
         labs(x="Population class",y=ylab) + 
         wtsPlots::getStdTheme() + 
         theme(axis.title.x=element_blank(),
               legend.direction="horizontal",
               legend.position="inside",
               legend.position.inside=c(0.01,0.99),
               legend.justification.inside=c(0,1));
  p2 = p1 %+% (dfrp |> dplyr::filter(m=="mature")) + theme(legend.position="none");
  pg = cowplot::plot_grid(p1,p2,ncol=1,rel_heights=c(1,1.8))
  return(pg)
}
# dfrM = wtsGMACS::extractNaturalMortality(resGMACS,resTCSAM02);
# compareNaturalMortalityty(dfrM);



