#'
#'@title Function to print a table of parameters from several models
#'
#'@description This function prints a table of parameters from several models.
#'
#'@param objs - list of resLst objects
#'@param mdfr - dataframe from call to \code{extractMDFR.Results.ParameterValues} (as alternative to objs)
#'@param format - 'latex' or 'docx'
#'@param landscape - flag indicating whether page orientation is landscape or portrait
#'@param fontsize - font size (can be 'small')
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details Uses \code{extractMDFR.Results.ParameterValues} to get a dataframe of parameters.
#'If format="latex", the package \code{tables} is used to create and print a table
#'in latex format. Output format for values and standard deviations is 3 significant figures.
#'
#'@import tables
#'
#'@export
#'
printResults.ParameterValues<-function(objs=NULL,
                                       mdfr=NULL,
                                       format=c("latex","docx"),
                                       landscape=FALSE,
                                       fontsize="11pt",
                                       verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::printResults.ParameterValues().\n");
    options(stringsAsFactors=FALSE);

    mdfrp<-mdfr;
    if (is.null(mdfrp)){
        mdfrp<-extractMDFR.Results.ParameterValues(objs,verbose=verbose);
    }
    mdfr<-NULL;
    mdfrp$label[mdfrp$label==""]<-" ";

    format<-format[1];
    if (format=="latex")
        if (fontsize=="small") cat("\\begin{small}\n")
        if (fontsize=="tiny") cat("\\begin{tiny}\n")
        uCs<-unique(mdfrp$category);
        for (uC in uCs){
            if (verbose) cat("Processing category = ",uC,"\n",sep='')
            mdfrpp<-mdfrp[mdfrp$category==uC,];
            if (uC!="selectivity") {
                uPs<-unique(mdfrpp$process);
                for (uP in uPs){
                    if (verbose) cat("\tProcessing process = ",uP,"\n",sep='');
                    mdfrppp<-mdfrpp[mdfrpp$process==uP,];
                    uTs<-unique(mdfrppp$type);
                    for (uT in uTs){
                        if (verbose) cat("\t\tProcessing type = ",uT,"\n",sep='');
                        mdfr<-mdfrppp[mdfrppp$type==uT,];
                        mdfr$name    <-as.factor(mdfr$name);
                        mdfr$type    <-as.factor(mdfr$type);
                        mdfr$index   <-as.factor(mdfr$index);
                        mdfr$parameter_scale<-as.factor(mdfr$parameter_scale);
                        mdfr$min_arith      <-as.factor(mdfr$min_arith);
                        mdfr$max_arith      <-as.factor(mdfr$max_arith);
                        mdfr$min_param      <-as.factor(mdfr$min_param);
                        mdfr$max_param      <-as.factor(mdfr$max_param);
                        mdfr$label          <-as.factor(mdfr$label);
                        mdfr$arith_value<-signif(mdfr$final_arith_value,digits=3);
                        mdfr$param_value<-signif(mdfr$final_param_value,digits=3);
                        mdfr$stdv       <-signif(mdfr$stdv,digits=3);
                        caption<-paste0("Model parameter values and standard deviations for ",uP," parameters.");
                        tbr<-tables::tabular(Factor(name)*Factor(label)*Factor(index)*Factor(parameter_scale)*
                                                 Factor(min_arith)*Factor(max_arith)*Factor(min_param)*Factor(max_param)*DropEmpty()~
                                                 Factor(case)*(arith_value+param_value+stdv)*Format(scientific=FALSE,digits=3)*wtsUtilities::Sum,data=mdfr);
                        colLabels(tbr)<-colLabels(tbr)[c(2,3),];
                        Hmisc::latex(tbr,options=list(tabular="longtable",
                                               toprule=paste0("\\caption{",caption,"} \\\\
                                                               \\hline")));
                        cat("\n\\clearpage\n")
                    }#--uT
                }#--uP
            } else {
                for (uS in list(S1="pS1",S2="pS2",S3=c("pS3","pS4"))){
                    if (verbose) cat("\t\tProcessing selectivity parameters = ",paste0(uS,collapse=", "),"\n",sep='');
                    mdfr<-mdfrpp[stringr::str_sub(mdfrpp$name,1,3) %in% uS,];
                    mdfr$parameter_scale<-as.factor(mdfr$parameter_scale);
                    mdfr$min_arith      <-as.factor(mdfr$min_arith);
                    mdfr$max_arith      <-as.factor(mdfr$max_arith);
                    mdfr$min_param      <-as.factor(mdfr$min_param);
                    mdfr$max_param      <-as.factor(mdfr$max_param);
                    mdfr$label          <-as.factor(mdfr$label);
                    mdfr$arith_value<-signif(mdfr$final_arith_value,digits=3);
                    mdfr$param_value<-signif(mdfr$final_param_value,digits=3);
                    mdfr$stdv       <-signif(mdfr$stdv,digits=3);
                    caption<-paste0("Parameter values and standard deviations for ",paste0(uS,collapse=", "),"-type selectivity parameters.");
                    tbr<-tables::tabular(Factor(label)*Factor(parameter_scale)*
                                             Factor(min_arith)*Factor(max_arith)*Factor(min_param)*Factor(max_param)*DropEmpty(which="row")~
                                             Factor(case)*(arith_value+param_value+stdv)*Format(scientific=FALSE,digits=3)*wtsUtilities::Sum,data=mdfr);
                    # tbr<-tables::tabular(Factor(label)*Factor(name)*Factor(index)*Factor(min)*Factor(max)*DropEmpty()~
                    #                          Factor(case)*(value+stdv)*wtsUtilities::Sum,data=mdfr);
                    colLabels(tbr)<-colLabels(tbr)[c(2,3),];
                    Hmisc::latex(tbr,options=list(tabular="longtable",
                                           toprule=paste0("\\caption{",caption,"} \\\\
                                                           \\hline")));
                    cat("\n\\clearpage\n")
                }#--uS
                for (uS in list(S1="pDevsS1")){
                    if (verbose) cat("\t\tProcessing selectivity parameters = ",paste(uS,collapse=", "),"\n",sep='');
                    mdfr<-mdfrpp[stringr::str_sub(mdfrpp$name,1,7) %in% uS,];
                    mdfr$parameter_scale<-as.factor(mdfr$parameter_scale);
                    mdfr$min_arith      <-as.factor(mdfr$min_arith);
                    mdfr$max_arith      <-as.factor(mdfr$max_arith);
                    mdfr$min_param      <-as.factor(mdfr$min_param);
                    mdfr$max_param      <-as.factor(mdfr$max_param);
                    mdfr$label          <-as.factor(mdfr$label);
                    mdfr$arith_value<-signif(mdfr$final_arith_value,digits=3);
                    mdfr$param_value<-signif(mdfr$final_param_value,digits=3);
                    mdfr$stdv       <-signif(mdfr$stdv,digits=3);
                    caption<-paste0("Parameter values and standard deviations for ",uS,"-type selectivity parameters.");
                    tbr<-tables::tabular(Factor(label)*Factor(name)*Factor(index)*Factor(min)*Factor(max)*Factor(parameter_scale)*
                                             Factor(min_arith)*Factor(max_arith)*Factor(min_param)*Factor(max_param)*DropEmpty()~
                                             Factor(case)*(arith_value+param_value+stdv)*Format(scientific=FALSE,digits=3)*wtsUtilities::Sum,data=mdfr);
                    colLabels(tbr)<-colLabels(tbr)[c(2,3),];
                    Hmisc::latex(tbr,options=list(tabular="longtable",
                                           toprule=paste0("\\caption{",caption,"} \\\\
                                                           \\hline")));
                    cat("\n\\clearpage\n")
                }#--uS
            }
        }#--uC
        if (fontsize=="small") cat("\\end{small}\n")
        if (fontsize=="tiny") cat("\\end{tiny}\n")

    if (verbose) cat("finished rCompTCMs::compareResults.ParameterValues().\n");
}
