#'
#'@title Function to print a table of parameters at bounds for several models
#'
#'@description This function prints a table of parameters at bounds for several models.
#'
#'@param objs - list of resLst objects
#'@param mdfr - dataframe from call to \code{extractMDFR.Results.ParametersAtBounds} (as alternative to objs)
#'@param delta - fraction of range which defines "at the bounds"
#'@param format - 'latex' or 'docx'
#'@param landscape - flag indicating whether page orientation is landscape or portrait
#'@param fontsize - font size (can be 'small')
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
printResults.ParametersAtBounds<-function(objs=NULL,
                                          mdfr=NULL,
                                          delta=0.0001,
                                          format=c("latex","docx"),
                                          landscape=FALSE,
                                          fontsize="11pt",
                                          verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::printResults.ParametersAtBounds().\n");
    options(stringsAsFactors=FALSE);

    mdfrp<-mdfr;
    if (is.null(mdfrp)){
        mdfrp<-extractMDFR.Results.ParametersAtBounds(objs,delta=delta,verbose=verbose);
    }
    mdfr<-NULL;

    format<-format[1];
    if (format=="latex")
        if (fontsize=="small") cat("\\begin{small}\n")
        if (fontsize=="tiny")  cat("\\begin{tiny}\n")
        uCs<-unique(mdfrp$category)
        for (uC in uCs){
            if (verbose) cat("Processing category = ",uC,"\n",sep='')
            mdfrpp<-mdfrp[mdfrp$category==uC,];
            uPs<-unique(mdfrpp$process);
            for (uP in uPs){
                if (verbose) cat("\tProcessing process = ",uP,"\n",sep='')
                mdfr<-mdfrpp[mdfrpp$process==uP,];
                if (nrow(mdfr>0)){
                    mdfr$name    <-as.factor(mdfr$name);
                    mdfr$type    <-as.factor(mdfr$type);
                    mdfr$index          <-as.factor(mdfr$index);
                    mdfr$parameter_scale<-as.factor(mdfr$parameter_scale);
                    mdfr$min_arith      <-as.factor(mdfr$min_arith);
                    mdfr$max_arith      <-as.factor(mdfr$max_arith);
                    mdfr$min_param      <-as.factor(mdfr$min_param);
                    mdfr$max_param      <-as.factor(mdfr$max_param);
                    mdfr$label          <-as.factor(mdfr$label);
                    mdfr$case           <-as.factor(mdfr$case);
                    caption<-paste0("Model parameters for ",uP," at bounds.");
                    tbr<-tables::tabular(Factor(name)*Factor(case)*Factor(label)*Factor(type)*Factor(index)*Factor(parameter_scale)*
                                             Factor(min_arith)*Factor(max_arith)*Factor(min_param)*Factor(max_param)*DropEmpty()~
                                             (final_arith_value+final_param_value+test)*wtsUtilities::Sum,data=mdfr);
                    Hmisc::latex(tbr,options=list(tabular="longtable",
                                           toprule=paste0("\\caption{",caption,"} \\\\
                                                           \\hline")));
                    cat("\n\n")
                }
            }
        }
        if (fontsize=="small") cat("\\end{small}\n")
        if (fontsize=="tiny") cat("\\end{tiny}\n")

    if (verbose) cat("finished rCompTCMs::compareResults.ParametersAtBounds().\n");
}
