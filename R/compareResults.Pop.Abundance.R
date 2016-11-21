#'
#'@title Function to compare estimated population abundance by year among several models
#'
#'@description This function compares estimated population abundance by year
#'   among several models.
#'   
#'@param objs - list of resLst objects
#'@param cast - cast'ing formula for aggregating by factors (y,x,m,s,z)
#'@param facet_grid - formula for faceting using facet_grid
#'@param facet_wrap - formula for faceting using facet_wrap
#'@param dodge - width to dodge overlapping series
#'@param years - 'all' or vector of years to include
#'@param mxy - max number of years per page
#'@param nrow - number of rows per page, when facet_wrap'ing 
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details If 'z' is a cast'ing factor, then a set of annual size composition plots are produced. Otherwise,
#'a set of time series plots are produced. Results are extracted using \code{rTCSAM2013::getMDFR.Pop.Abundance},
#'\code{rsimTCSAM::getMDFR.Pop.Abundance}, and/or \code{rTCSAM02::getMDFR.Pop.Abundance}, as appropriate, and 
#'cast to aggregate. This differs from \code{compareResults.Pop.Abundance1}.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.Abundance<-function(objs,
                                       cast="y+x",
                                       facet_grid="x~.",
                                       facet_wrap=NULL,
                                       dodge=0.2,
                                       years='all',
                                       mxy=15,
                                       nrow=5,
                                       showPlot=FALSE,
                                       pdf=NULL,
                                       verbose=TRUE){
    if (verbose) cat("rCompTCMs::compareResults.Pop.Abundance: Start.\n");
    options(stringsAsFactors=FALSE);
    
    if (is.null(cast)){
        cat("Error in rCompTCMs::compareResults.Pop.Abundance()\n");
        cat("Must supply a 'cast' formula!\nReturning NULL.\n");
        return(NULL);
    }
    
    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.Abundance(obj,cast=cast,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.Abundance(obj,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.Abundance(obj,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    
    if (is.numeric(years)) {
        mdfr<-mdfr[mdfr$y %in% years,];
    }
    
    #----------------------------------
    #population abundance
    #----------------------------------
    plots<-list();
    if (sum(grep('z',cast,fixed=TRUE))>0){
        #plot size comps by year
        if (verbose) cat("Plotting size comps\n")
        mdfr$z<-as.numeric(mdfr$z);
            uY<-sort(unique(mdfr$y));
            for (pg in 1:ceiling(length(uY)/mxy)){
                mdfrpp<-mdfr[mdfr$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],];
                p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
                               facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=nrow,
                               xlab='size (mm CW)',ylab='Population Abundance',units='millions',lnscale=FALSE,
                               colour='case',guideTitleColor='',
                               shape='case',guideTitleShape='',
                               showPlot=FALSE);
                if (showPlot||!is.null(pdf)) print(p);
                plots[[paste(cast,pg,sep=".")]]<-p;
            }#pg
    } else {
        #plot time series
        if (verbose) cat("Plotting time series.\n")
        p<-plotMDFR.XY(mdfr,x='y',value.var='val',agg.formula=NULL,
                       facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=nrow,
                       xlab='year',ylab='Population Abundance',units='millions',lnscale=FALSE,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='',
                       showPlot=FALSE);
        if (showPlot||!is.null(pdf)) print(p);
        plots[[cast]]<-p;
    }

    if (verbose) cat("rCompTCMs::compareResults.Pop.Abundance: Done!\n");
    return(plots)
}
