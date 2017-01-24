#'
#'@title Function to compare differences of a quantity among several models
#'
#'@description This function compares differences of a quantity
#'   among several models.
#'   
#'@param dfr - dataframe in canonical format
#'@param diff.type - "percent" or "absolute"
#'@param diff.min - minimum difference to show
#'@param cast - cast'ing formula for aggregating by factors before differencing (fleet,x,m,s,z,y)
#'@param facet_grid - formula (or string version of formula) for faceting using facet_grid
#'@param facet_wrap - one-sided formula (e.g., "~y+x") or character vector (e.g., c('y','x')) for faceting using facet_wrap
#'@param scales - scales parameter for use with facet_grid/facet_wrap
#'@param dodge - width to dodge overlapping series
#'@param years - 'all' or vector of years to include
#'@param mxy - max number of years per page
#'@param nrow - number of rows per page, when facet_wrap'ing 
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return list of ggplot2 objects
#'
#'@details If 'z' is a cast'ing factor, then a set of annual size composition plots are produced. Otherwise,
#'a set of time series plots are produced.
#'
#'@import ggplot2
#'
#'@export
#'
compareDiffs<-function(dfr,
                       base=1,
                       diff.type=c("percent","absolute"),
                       diff.min=1.0e-3,
                       cast=NULL,
                       facet_grid=NULL,
                       facet_wrap=NULL,
                       scales="fixed",
                       dodge=0.2,
                       title="",
                       years='all',
                       showPlot=FALSE,
                       pdf=NULL,
                       verbose=FALSE){
    if (verbose) cat("--starting rCompTCMs::compareDiffs().\n");
    options(stringsAsFactors=FALSE);
    
    diff.type<-diff.type[1];
    
    if (is.null(cast)){
        cat("Error in rCompTCMs::compareDiffs()\n");
        cat("Must supply a 'cast' formula!\nReturning NULL.\n");
        return(NULL);
    }
    
    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    if (is.numeric(years)) dfr<-dfr[dfr$y %in% years,];

    cases<-unique(as.character(dfr$case));
    if (is.numeric(base)) base<-cases[base];
    dfrp<-computeDiffs(dfr,cast=cast,fun=sum,base=base,type=diff.type);
    cls<-names(dfrp);
    id.vars<-cls[cls %in% c("category",'fleet','type','x','m','s','z','zp','y')];
    pattern<-ifelse(diff.type=="percent",'pctdiff.','absdiff.');
    measure.vars<-cls[grep(cls,pattern=pattern,fixed=TRUE)];
    cls<-gsub(pattern,"",cls,fixed=TRUE);
    measure.vars<-gsub(pattern,"",measure.vars,fixed=TRUE);
    names(dfrp)<-cls;
    var<-'var';
    mdfr<-reshape2::melt(dfrp,id.vars,measure.vars,value.name="val",variable.name=var);


    #----------------------------------
    #plot differences
    #----------------------------------
    plots<-list();
    uF<-"";
    if (!is.null(mdfr$fleet)) uF<-unique(mdfr$fleet);
    if (verbose) cat("Fleets = ",uF,"\n");
    if (sum(grep('zp',cast,fixed=TRUE))>0){
        #plot growth matrices
        if (verbose) cat("Plotting growth matrices\n")
        mdfr$z <-as.numeric(mdfr$z);
        mdfr$zp<-as.numeric(mdfr$zp);
        mdfr$sign<-ifelse(mdfr$val>=0,">=0","<0");
        mdfr$val<-abs(mdfr$val);
        mdfrp<-mdfr;
        uX<-'all sex';
        if (!is.null(mdfrp$x)) uX<-sort(unique(mdfrp$x));
        for (x in uX){
            cat("plotting growth matrix for",x,"\n")
            idx<-TRUE;
            if (!is.null(mdfrp$x)) idx<-mdfrp$x==x;
            mdfrpp<-mdfrp[idx,];
            if (sum(mdfrpp$val)==0) mdfrpp$val[1]<-1.0e-10;
            p<-plotMDFR.Bubbles(mdfrpp,x='z',y='zp',colour="sign",
                                title=paste("growth matrices\nfor",x),
                                guideTitleColour=paste(diff.type,"\ndifference"))
            if (showPlot||!is.null(pdf)) print(p);
            cap<-paste0("\n  \nFigure &&figno. Growth matrix differences for ",x,".  \n  \n")
            plots[[cap]]<-p;
        }
    } else if (sum(grep('z',cast,fixed=TRUE))>0){
        #plot size comps by year
        if (verbose) cat("Plotting size comps\n")
        mdfr$z<-as.numeric(mdfr$z);
        mdfr$sign<-ifelse(mdfr$val>=0,">=0","<0");
        mdfr$val<-abs(mdfr$val);
        for (f in uF){
            if (verbose) cat("Plotting fleet",f,"\n")
            subPlots<-list();
            mdfrp<-mdfr;
            if (!is.null(mdfr$fleet)) mdfrp<-mdfr[mdfr$fleet==f,];
            uX<-'all sex';
            uM<-'all maturity';
            uS<-'all shell';
            if (!is.null(mdfrp[["x"]])) uX<-sort(unique(mdfrp[["x"]]));
            if (!is.null(mdfrp[["m"]])) uM<-sort(unique(mdfrp[["m"]]));
            if (!is.null(mdfrp[["s"]])) uS<-sort(unique(mdfrp[["s"]]));
            for (x in uX){
                idx<-TRUE;
                if (!is.null(mdfrp[["x"]])) idx<-mdfrp[["x"]]==x;
                for (m in uM){
                    idm<-TRUE;
                    if (!is.null(mdfrp[["m"]])) idm<-mdfrp[["m"]]==m;
                    for (s in uS){
                        ids<-TRUE;
                        if (!is.null(mdfrp[["s"]])) ids<-mdfrp[["s"]]==s;
                        mdfrpp<-mdfrp[idx&idm&ids,];
                        if (nrow(mdfrpp)>0){
                            if (verbose) cat("Plotting ",nrow(mdfrpp)," rows for",x,m,s,".\n")
                            p<-plotMDFR.Bubbles(mdfrpp,x='y',y='z',colour="sign",
                                                title=paste(uF,title,"\nfor",x,m,s),
                                                guideTitleColour=paste(diff.type,"\ndifference"))
                            if (showPlot||!is.null(pdf)) print(p);
                            cap<-paste0("\n  \nFigure &&figno. Differences for ",f," ",title," for ",x," ",m," ",s,".  \n  \n")
                            subPlots[[cap]]<-p;
                        } else {
                            if (verbose) cat("Skipping ",x,m,s,"\n");
                        }
                    }#uS
                }#uM
            }#uX
            plots[[f]]<-subPlots;
        }#uF
    } else {
        #plot time series
        if (verbose) cat("Plotting time series.\n")
        for (f in uF){
            if (verbose) cat("Plotting fleet",f,"\n")
            title1<-title;
            mdfrp<-mdfr;
            if (!is.null(mdfr$fleet)) {
              title1<-paste0(f,": ",title);
              mdfrp<-mdfr[mdfr$fleet==f,];
            }
            p<-plotMDFR.XY(mdfrp,x='y',value.var='val',agg.formula=NULL,
                           facet_grid=facet_grid,scales=scales,
                           facet_wrap=facet_wrap,nrow=nrow,
                           xlab='year',ylab=paste(diff.type,"difference"),units='',lnscale=FALSE,
                           title=title1,
                           colour=var,guideTitleColour='case',
                           shape=var,guideTitleShape='case',
                           showPlot=FALSE);
            if (showPlot||!is.null(pdf)) print(p);
            cap<-paste0("\n  \nFigure &&figno. Differences for ",title1,".  \n  \n")
            plots[[cap]]<-p;
        }#uF
    }

    if (verbose) cat("rCompTCMs::compareDiffs: Done!\n");
    return(plots)
}

#'
#' @title Compute diiferences between model cases
#' 
#' @description Function to compute differences between model cases
#' 
#' @param x - dataframe to cast for differences
#' @param cast - cast'ing factors (left side of cast'ing equation)
#' @param base - name (or number) of case to use as base
#' @param fun - aggregation function
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
