#'
#' @title Create a [ggplot2] object to compare tcsam02 and gmacs selectivity/retention functions
#' @description Function to create a [ggplot2] object to compare tcsam02 and gmacs
#' selectivity/retention functions. Can also be used to plot either model type.
#' @param dfr - dataframe to plot
#' @param ylab - y-axis label
#' @param sub - subtitle to label plot
#' @return [ggplot2] plot object
#'
#' @details The input dataframe should at least have columns named "case" (model case), "y" (year),
#' "z" (size), and "val" (value). Curves are colored by `case`.
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
compareSelFcns<-function(dfr,ylab="Selectivity",sub=""){
  plotSels<-function(dfr,ylab="Selectivity",sub=""){
    ggplot(dfr,aes(x=z,y=val,colour=case)) +
      geom_line() +
      facet_wrap(~ylabs) +
      scale_y_continuous(limits=c(0,NA)) +
      labs(x="size (mm CW)",y=ylab,subtitle=sub) +
      wtsPlots::getStdTheme();
  }
  cols = names(dfr)[names(dfr) %in% c("case","fleet","y","x","m","s","z","val")];
  ncls = length(cols);
  tmp0 = dfr |> dplyr::select(tidyselect::all_of(cols));
  tmp1 = wtsUtilities::collectValuesByGroup(tmp0,collect="y",names_from="z",values_from="val");
  tmp1 = tmp1 |> dplyr::rowwise() |>
                 dplyr::mutate(ylab=wtsUtilities::collapseIntegersToString(y),.before=y) |>
                 dplyr::ungroup();
  tmp2  = tmp1 |> dplyr::select(tidyselect::any_of(1:ncls));
  tmp2a = tmp2 |> dplyr::cross_join(tmp2);
  tmp3  = tmp2a |> dplyr::filter(!(case.x==case.y),(stringr::str_starts(case.x,"gmacs")));
  if (nrow(tmp3)>0){
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
  ncls5 = ncol(tmp5);
  tmp6 = tmp5 |> dplyr::inner_join(tmp1) |>
           tidyr::pivot_longer(ncls5+1:(ncol(tmp1)-ncls),names_to="z",values_to="val") |>
           dplyr::mutate(z=as.numeric(z)) |>
           dplyr::select(!y);
  return(plotSels(tmp6,ylab=ylab,sub=sub))
}
# tmp =  dfrSel |> dplyr::filter(fleet %in% "TCF",type=="retained",x=="male")
# compareSelFcns(tmp,"Retention",sub="TCF males");
# tmp =  dfrSel |> dplyr::filter(fleet %in% "TCF",type=="capture",x=="male")
# compareSelFcns(tmp,"Selectivity",sub="TCF males");
# tmp =  dfrSel |> dplyr::filter(fleet %in% "TCF",type=="capture",x=="female")
# compareSelFcns(tmp,"Selectivity",sub="TCF females");
# tmp =  dfrSel |> dplyr::filter(fleet %in% "SCF",type=="capture",x=="male")
# compareSelFcns(tmp,"Selectivity",sub="SCF males");
# tmp =  dfrSel |> dplyr::filter(fleet %in% "SCF",type=="capture",x=="female")
# compareSelFcns(tmp,"Selectivity",sub="SCF females");
# tmp =  dfrSel |> dplyr::filter(fleet %in% "RKF",type=="capture",x=="male")
# compareSelFcns(tmp,"Selectivity",sub="RKF males");
# tmp =  dfrSel |> dplyr::filter(fleet %in% "RKF",type=="capture",x=="female")
# compareSelFcns(tmp,"Selectivity",sub="RKF females");
# tmp =  dfrSel |> dplyr::filter(fleet %in% "GFA",type=="capture",x=="male")
# compareSelFcns(tmp,"Selectivity",sub="GFA males");
# tmp =  dfrSel |> dplyr::filter(fleet %in% "GFA",type=="capture",x=="female")
# compareSelFcns(tmp,"Selectivity",sub="GFA females");



