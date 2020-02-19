#'
#' @title Compute diiferences between model cases
#'
#' @description Function to compute differences between model cases
#'
#' @param x - dataframe to cast for differences
#' @param cast - cast'ing factors (left side of cast'ing equation)
#' @param base - name (or number) of case to use as base
#' @param fun - aggregation function
#' @param type - type of difference ("absolute" or "percent")
#'
#' @return dataframe
#'
#' @details None.
#'
computeDiffs<-function(x,cast="type+x+m+s+z+y",base=1,fun=sum,type=c("percent","absolute")){
    type<-type[1];
    cast<-paste0(cast,"~case");
    nc<-length(unique(x$case));
    dp<-reshape2::dcast(x,cast,fun.aggregate=fun,value.var='val')
    cls<-names(dp);
    col<-cls[(length(cls)-nc+1):length(cls)];
    if (is.numeric(base)) base <- col[base];
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
