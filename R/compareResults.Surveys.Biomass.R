#'
#'@title Function to compare estimated survey biomass by year among several models
#'
#'@description This function compares estimated survey biomass by year
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
#'a set of time series plots are produced.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Surveys.Biomass<-function(objs,
                                           category='index',
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
    if (verbose) cat("Starting rCompTCMs::compareResults.Surveys.Biomass().\n");
    options(stringsAsFactors=FALSE);
    
    if (is.null(cast)){
        cat("Error in rCompTCMs::compareResults.Surveys.Biomass()\n");
        cat("Must supply a 'cast' formula!\nReturning NULL.\n");
        return(NULL);
    }
    
    category<-"index";
    
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
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Surveys.Biomass(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Surveys.Biomass(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Surveys.Biomass(obj,category=category,cast=cast,verbose=verbose);
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
    #survey biomass
    #----------------------------------
    plots<-list();
    uF<-unique(mdfr$fleet);
    if (sum(grep('z',cast,fixed=TRUE))>0){
        #plot size comps by year
        if (verbose) cat("Plotting size comps\n")
        mdfr$z<-as.numeric(mdfr$z);
        for (f in uF){
            if (verbose) cat("Plotting fleet",f,"\n")
            mdfrp<-mdfr[mdfr$fleet==f,];
            uY<-sort(unique(mdfrp$y));
            for (pg in 1:ceiling(length(uY)/mxy)){
                mdfrpp<-mdfrp[mdfrp$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],];
                p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
                               facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=nrow,
                               xlab='size (mm CW)',ylab='Survey Biomass',units="1000's t",lnscale=FALSE,
                               title=f,
                               colour='case',guideTitleColor='',
                               shape='case',guideTitleShape='',
                               showPlot=FALSE);
                if (showPlot||!is.null(pdf)) print(p);
                plots[[paste(f,pg,sep=".")]]<-p;
            }#pg
        }#uF
    } else {
        #plot time series
        if (verbose) cat("Plotting time series.\n")
        for (f in uF){
            if (verbose) cat("Plotting fleet",f,"\n")
            mdfrp<-mdfr[mdfr$fleet==f,];
            p<-plotMDFR.XY(mdfrp,x='y',value.var='val',agg.formula=NULL,
                           facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=nrow,
                           xlab='year',ylab='Survey Biomass',units="1000's t",lnscale=FALSE,
                           title=f,
                           colour='case',guideTitleColor='',
                           shape='case',guideTitleShape='',
                           showPlot=FALSE);
            if (showPlot||!is.null(pdf)) print(p);
            plots[[f]]<-p;
        }#uF
    }

    if (verbose) cat("rCompTCMs::compareResults.Surveys.Biomass: Done!\n");
    return(plots)
}
