#'
#'@title Function to print a table of parameters from several models
#'
#'@description This function prints a table of parameters from several models.
#'
#'@param mdfr - dataframe from call to [extractMDFR.Results.ParameterValues()], filtered for
#'@param params - selected parameter names (default = "all")
#'@param fontsize - font size (can be 'small')
#'@param table_type - latex table type ("" or "longtable")
#'@param caption - table caption
#'@param label - table label (for cross-referencing)
#'@param conn - NULL, "", a filename, or a connection object
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return latex text string representing the table (for later insertion into a markdown document)
#'
#'@details \code{mdfr} should be a dataframe obtained from [extractMDFR.Results.ParameterValues()],
#'filtered to include only param_init_numbers and/or param_init_bounded_numbers.
#'The output format for values and standard deviations is 3 significant figures.
#'
#'NOTE: [wtsMarkdown::writeTabularToLatex()] is used to create the latex code representing
#'the tabular object. If \code{conn=""}, the latex code is written to standard output. If \code{conn} is a filename or a
#'connection object, the latex code is written to that. If it is NULL, the code is not written to an output device.
#'In all cases, the code is returned as a string, invisibly.
#'
#'@import dplyr
#'@import stringr
#'@import tables
#'@import wtsMarkdown
#'
#'@md
#'
#'@export
#'
createLatexForTable.ParamValues.Numbers<-function(mdfr,
                                                  params="all",
                                                  fontsize=c("normal","small"),
                                                  table_type=c("","longtable"),
                                                  caption="",
                                                  label="",
                                                  conn=NULL,
                                                  verbose=FALSE){
    if (verbose) message("Starting rCompTCMs::createLatex.ParamValues.NumberVectors");
    fontsize   = fontsize[1];
    table_type = table_type[1];
    uTs = stringr::str_trim(unique(mdfr$type));
    uNs = unique(mdfr$name);
    if (!all(uTs %in% c("param_init_number","param_init_bounded_number"))){
        str = paste0("All parameter types must be either param_init_number or param_init_bounded_number.\n",
                     "Input dataframe had types '",paste(uTs,collapse="',"),"'.\n",
                     "Parameter names were '",paste(uNs,collapse="',"),"'");
        stop(str);
    }
    if (params!="all") uNs = params;
    if (verbose) message("creating table for ",uNs);
    mdfr1 = mdfr %>% dplyr::filter(name %in% uNs);
    if (nrow(mdfr1)==0) {
        message("No rows matched parameters named '",paste(uNs,collapse="',"),"'");
        return("");
    } else {
        if (verbose) message("names = ",paste(mdfr$name,collapse=" "));
        pnms = stringr::str_extract(mdfr1$name,"(.*?)(?=\\[)");                    #--extract just parameter names
        if (verbose) message("pnms = ",paste(pnms,collapse=" "));
        pids = as.numeric(stringr::str_extract(mdfr1$name,"(?<=\\[)(.*?)(?=\\])"));#--extract just parameter indices
        if (verbose) message("pids = ",paste(pids,collapse=" "));
        nd   = ceiling(max(log10(pids+0.1),na.rm=TRUE));#--max number of digits
        if (verbose) message("nd = ",nd);
        pids = format(pids,width=nd)
        mdfr1%<>%
             dplyr::select(case,name,label,index,scale=parameter_scale,min=min_param,max=max_param,
                           `arith value`=final_arith_value,`param value`=final_param_value,stdv) %>%
             dplyr::mutate(name  = as.factor(paste0(pnms,"[",pids,"]")),
                           label = as.factor(label),
                           scale = as.factor(tolower(scale)),
                           lower = as.factor(signif(round(min,digits=6),digits=3)),
                           upper = as.factor(signif(round(max,digits=6),digits=3)),
                           value = signif(round(`param value`,digits=6),digits=3),
                           stdv  = signif(round(stdv,digits=6),digits=3));
        tbr<-tables::tabular(Justify("l","l")*Factor(name)*
                              (Justify("r","r")*(Factor(label)*
                                 Factor(scale)*Factor(lower)*Factor(upper)))*DropEmpty()~
                                   Factor(case)*(value+stdv)*Format(scientific=FALSE,digits=3)*wtsUtilities::Sum,
                             data=mdfr1);
        labelsForRowFactors =  c("name","label","scale","lower","upper");
        tbr = wtsMarkdown::fixColLabelsForRowFactors(tbr,labelsForRowFactors,dim=2,verbose=verbose);
        colLabels(tbr)<-colLabels(tbr)[c(2,3),];
        str = wtsMarkdown::writeTabularToLatex(tbr,caption,label=uP,type="longtable",conn=conn);
    }#--nrow(mdfr1)>0
    return(invisible(str));
}
