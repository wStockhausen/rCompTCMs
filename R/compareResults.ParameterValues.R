#'
#'@title Function to compare parameter values among several models (UNDER DEVELOPMENT!)
#'
#'@description This function compares parameter values
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object(?)
#'
#'@details Under development!
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
