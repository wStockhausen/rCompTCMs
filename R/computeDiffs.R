#'
#' @title Compute differences between model cases
#'
#' @description Function to compute differences between model cases
#'
#' @param x - dataframe in canonical format to cast for differences
#' @param base - name (or number) of case to use as base
#' @param cast - cast'ing factors (left side of [reshape2::dcast()] cast'ing equation, with "case" on the righthand side)
#' @param fun - aggregation function (default is [wtsUtilities::Sum()])
#' @param type - type of difference ("absolute" or "percent")
#'
#' @return a dataframe (see details)
#'
#' @details Uses [reshape2::dcast()] to aggregate values within factor levels defined by the cast'ing formula prior to differencing.
#' The default cast string is "process+fleet+category+type+pc+x+m+s+z+y",
#' yielding the cast formula "process+fleet+category+type+pc+x+m+s+z+y~cast" (i.e., no aggregation, but possibly transformation
#' if \code{fun} transforms values prior to aggregating them).
#'
#' The columns of the returned dataframe will include the factors included in the input cast'ing string \code{cast}, as well as
#' columns of the form "absdiff.base-case" or "pctdiff.base-case", where "base" and "case" are the names of the base model and subsequent
#' cases. Note that the latter column names can be returned to "case" by \code{colnames(xd) = gsub("^.*-","",colnames(xd))}, where
#' \code{x} is the result of \code{computeDiffs}.
#'
#' @importFrom reshape2 dcast
#' @importFrom wtsUtilities Sum
#'
#' @md
#'
#' @export
#'
computeDiffs<-function(x,
                       base=1,
                       cast="process+fleet+category+type+pc+x+m+s+z+y",
                       fun=wtsUtilities::Sum,
                       type=c("percent","absolute")){

    if (is.numeric(base)) base <- unique(x$case)[base];
    type<-type[1];
    cast<-paste0(cast,"~case");
    nc<-length(unique(x$case));
    dp<-reshape2::dcast(x,cast,fun.aggregate=fun,value.var='val')
    cls<-names(dp);
    col<-cls[(length(cls)-nc+1):length(cls)];
    col<-col[(col != base)]; #drop base
    for (c in 1:length(col)){
      if (type=="absolute"){
        dff<-paste0("absdiff.",base,"-",col[c])
        dp[[dff]]<-dp[[base]]-dp[[col[c]]];
      } else {
        pdf<-paste0("pctdiff.",base,"-",col[c])
        dp[[pdf]]<-ifelse(abs(dp[[base]]+dp[[col[c]]])>0,100*0.5*(dp[[base]]-dp[[col[c]]])/(dp[[base]]+dp[[col[c]]]),0);
      }
    }
    cols<-names(dp);
    dp<-dp[,!(cols %in% c(base,col))];
    return(dp);
}
