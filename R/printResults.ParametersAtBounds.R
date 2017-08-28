#'
#'@title Function to print a table of parameters at bounds for several models
#'
#'@description This function prints a table of parameters at bounds for several models.
#'
#'@param objs - list of resLst objects
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details Uses \code{extractMDFR.Results.ParametersAtBounds} to get a dataframe of parameters at one of their bounds,
#'based upon the value for delta. If format="latex", the package \code{tables} is used to create and print a table
#'in latex format.
#'
#'@import tables
#'
#'@export
#'
printResults.ParametersAtBounds<-function(objs,
                                          delta=0.0001,
                                          caption="",
                                          format=c("latex","docx"),
                                          landscape=FALSE,
                                          font="11pt",
                                          verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::printResults.ParametersAtBounds().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-extractMDFR.Results.ParametersAtBounds(objs,delta=delta,verbose=verbose);

    format<-format[1];
    if (format=="latex")
        if (font=="small") cat("\\begin{small}\n")
        # tbr<-tables::tabular(category*process*name*type*index*min*max~case*(test+value+label)*wtsUtilities::Sum,data=mdfr);
        # latex(tbr,options=list(tabular="longtable",
        #                        toprule=paste0("\\caption{",cap,"} \\\\
        #                                        \\hline")));
        xtbl<-xtable::xtable(mdfr,caption=caption);
        xtable::print.xtable(xtbl,type="latex",
                             floating=FALSE,tabular.environment="longtable",
                             caption.placement="top",
                             include.rownames=FALSE)
        if (font=="small") cat("\\end{small}\n")

    if (verbose) cat("finished rCompTCMs::compareResults.ParameterValues().\n");
}
