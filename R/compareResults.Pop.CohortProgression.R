#'
#'@title Function to compare cohort progression by year among several models
#'
#'@description This function compares cohort progression by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param cast - casting formula for excluding x,m,s,z factor levels from sums across the unspecified factors
#'@param facet_grid - formula for faceting using facet_grid
#'@param facet_wrap - formula for faceting using facet_wrap
#'@param dodge - width to dodge overlapping series
#'@param years - numeric vector of years to include (or NULL for all)
#'@param mxy - max number of years per page
#'@param nrow - number of rows per page, when facet_wrap'ing
#'@param lnscale - use log scale on y axis
#'@param scales - scales parameter for facet_grid/facet_wrap
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - filename for pdf output (or NULL for no pdf)
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details If 'z' is a cast'ing factor, then a set of annual size composition plots are produced. Otherwise,
#'a set of time series plots are produced. Results are extracted using \code{extractMDFR.Pop.CohortProgression},
#'for TCSAM02 model results (only). Plots are made using \code{plotPop.CohortProgression}.
#'
#'@export
#'
compareResults.Pop.CohortProgression<-function(objs,
                                       cast="x+m+s+z",
                                       facet_grid=NULL,
                                       facet_wrap=NULL,
                                       dodge=0.2,
                                       years=NULL,
                                       mxy=4,
                                       nrow=2,
                                       lnscale=FALSE,
                                       scales="fixed",
                                       showPlot=FALSE,
                                       pdf=NULL,
                                       verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::compareResults.Pop.CohortProgression().\n");
    options(stringsAsFactors=FALSE);

    if (is.null(cast)){
        cat("Error in rCompTCMs::compareResults.Pop.CohortProgression()\n");
        cat("Must supply a 'cast' formula!\nReturning NULL.\n");
        return(NULL);
    }

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    mdfr<-extractMDFR.Pop.CohortProgression(objs,cast=cast,years=years,verbose=verbose);

    plots<-plotPop.CohortProgression(mdfr,
                                     facet_grid=facet_grid,
                                     facet_wrap=facet_wrap,
                                     dodge=dodge,
                                     years=years,
                                     mxy=mxy,
                                     nrow=nrow,
                                     lnscale=lnscale,
                                     scales=scales,
                                     types=c("progression","byyear"),
                                     showPlot=showPlot,
                                     verbose=verbose);

    if (verbose) cat("finished rCompTCMs::compareResults.Pop.CohortProgression().\n");
    return(plots)
}
