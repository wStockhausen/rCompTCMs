#'
#'@title Function to compare estimated population biomass by year among several models
#'
#'@description This function compares estimated population biomass by year
#'   among several models.
#'   
#'@param objs - list of resLst objects
#'@param cast - casting formula for excluding x,m,s,z factor levels from sums across the unspecified factors
#'@param facet_grid - formula for faceting using facet_grid
#'@param facet_wrap - formula for faceting using facet_wrap
#'@param dodge - width to dodge overlapping series
#'@param years - 'all' or vector of years to include
#'@param mxy - max number of years per page
#'@param nrow - number of rows per page, when facet_wrap'ing 
#'@param lnscale - use log scale on y axis
#'@param scales - scales parameter for facet_grid/facet_wrap
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details If 'z' is a cast'ing factor, then a set of annual size composition plots are produced. Otherwise,
#'a set of time series plots are produced. Results are extracted using \code{rTCSAM2013::getMDFR.Pop.Biomass},
#'\code{rsimTCSAM::getMDFR.Pop.Biomass}, and/or \code{rTCSAM02::getMDFR.Pop.Biomass}, as appropriate, and 
#'cast to aggregate. This differs from \code{compareResults.Pop.Biomass1}.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.Biomass<-function(objs,
                                     cast="x",
                                     facet_grid="x~.",
                                     facet_wrap=NULL,
                                     dodge=0.2,
                                     years='all',
                                     mxy=15,
                                     nrow=5,
                                     lnscale=FALSE,
                                     scales="fixed",
                                     showPlot=FALSE,
                                     pdf=NULL,
                                     verbose=FALSE){
    if (verbose) cat("--starting rCompTCMs::compareResults.Pop.Biomass()\n");
    options(stringsAsFactors=FALSE);
    
    if (is.null(cast)){
        cat("Error in rCompTCMs::compareResults.Pop.Biomass()\n");
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

    mdfr<-extractMDFR.Pop.Biomass(objs,cast=cast,years=years,verbose=verbose);
    
    #----------------------------------
    #population biomass
    #----------------------------------
    plots<-list();
    if (sum(grep('z',cast,fixed=TRUE))>0){
        #plot size comps by year
        if (verbose) cat("Plotting size comps\n")
        mdfr$z<-as.numeric(mdfr$z);
        mdfrp<-mdfr;
        uY<-sort(unique(mdfrp$y));
        uX<-sort(unique(mdfrp$x));
        uM<-sort(unique(mdfrp$m));
        uS<-sort(unique(mdfrp$s));
        for (x in uX){
            idx<-mdfrp$x==x;
            for (m in uM){
                idm<-mdfrp$m==m;
                for (s in uS){
                    ids<-mdfrp$s==s
                    for (pg in 1:ceiling(length(uY)/mxy)){
                        idy<-mdfrp$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)];
                        mdfrpp<-mdfrp[idx&idm&ids&idy,];
                        if (nrow(mdfrpp)>0){
                            if (verbose) cat("Plotting ",x,m,s,paste0(uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],collapse=','),"\n");
                            mdfrpp$y<-as.character(mdfrpp$y);
                            p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
                                           facet_wrap=~y,nrow=nrow,,scales=scales,
                                           xlab='size (mm CW)',ylab='Population Biomass',units="1000's t",lnscale=FALSE,
                                           title=paste0(x," ",m," ",s),
                                           colour='case',guideTitleColor='',
                                           shape='case',guideTitleShape='',
                                           showPlot=FALSE);
                            if (showPlot||!is.null(pdf)) print(p);
                                cap<-paste0("\n  \nFigure &&figno. Population biomass for ",x," ",m," ",s,", (",pg," of ",ceiling(length(uY)/mxy),").  \n  \n")
                                plots[[cap]]<-p;
                        } else {
                            if (verbose) cat("Skipping ",x,m,s,paste0(uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],collapse=','),"\n");
                        }
                    }#pg
                }#uS
            }#uM
        }#uX
    } else {
        #plot time series
        if (verbose) cat("Plotting time series.\n")
        p<-plotMDFR.XY(mdfr,x='y',value.var='val',agg.formula=NULL,
                       facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=nrow,scales=scales,
                       xlab='year',ylab='Population Biomass',units="1000's t",lnscale=FALSE,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='',
                       showPlot=FALSE);
        if (showPlot||!is.null(pdf)) print(p);
        plots[[cast]]<-p;
    }

    if (verbose) cat("rCompTCMs::compareResults.Pop.Biomass: Done!\n");
    return(plots)
}
