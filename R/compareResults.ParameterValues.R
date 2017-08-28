#'
#'@title Function to compare cohort progression by year among several models
#'
#'@description This function compares cohort progression by year
#'   among several models.
#'   
#'@param objs - list of resLst objects
#'@param showPlot - flag (T/F) to show plot
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
compareResults.ParameterValues<-function(objs,
                                       showPlot=FALSE,
                                       verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::compareResults.ParameterValues().\n");
    options(stringsAsFactors=FALSE);
    
    cases<-names(objs);

    mdfr<-extractMDFR.Results.ParameterValues(objs,verbose=verbose);
    
    
    
    if (verbose) cat("finished rCompTCMs::compareResults.ParameterValues().\n");
    return(plots)
}
