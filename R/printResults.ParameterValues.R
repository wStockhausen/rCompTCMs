#'
#'@title Function to print a table of parameters from several models
#'
#'@description This function prints a table of parameters from several models.
#'
#'@param objs - list of resLst objects
#'@param caption - string for table caption
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details Uses \code{extractMDFR.Results.ParameterValues} to get a dataframe of parameters.
#'If format="latex", the package \code{tables} is used to create and print a table
#'in latex format.
#'
#'@import tables
#'
#'@export
#'
printResults.ParameterValues<-function(objs,
                                      caption="",
                                      format=c("latex","docx"),
                                      landscape=FALSE,
                                      font="11pt",
                                      verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::printResults.ParametersAtBounds().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-extractMDFR.Results.ParameterValues(objs,verbose=verbose);

    format<-format[1];
    if (format=="latex")
        if (font=="small") cat("\\begin{small}\n")
        tbr<-tables::tabular(Factor(category)*Factor(process)*Factor(name)*
                                 Factor(type)*Factor(index)*Factor(min)*Factor(max)~
                                 Factor(case)*(value+stdv)*wtsUtilities::Sum,data=mdfr);
        latex(tbr,options=list(tabular="longtable",
                               toprule=paste0("\\caption{",caption,"} \\\\
                                               \\hline")));
        # xtbl<-xtable::xtable(mdfr,caption=caption);
        # xtable::print.xtable(xtbl,type="latex",
        #                      floating=FALSE,tabular.environment="longtable",
        #                      caption.placement="top",
        #                      include.rownames=FALSE)
        if (font=="small") cat("\\end{small}\n")

    if (verbose) cat("finished rCompTCMs::compareResults.ParameterValues().\n");
}
