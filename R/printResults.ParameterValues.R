#'
#'@title Function to print a table of parameters from several models
#'
#'@description This function prints a table of parameters from several models.
#'
#'@param objs - list of resLst objects
#'@param mdfr - dataframe from call to [extractMDFR.Results.ParameterValues()] (as alternative to objs)
#'@param categories - default = "all"
#'@param processes - default = "all"
#'@param types - default = "all"
#'@param params - selected parameter names (default = "all")
#'@param format - 'latex' or 'docx'
#'@param landscape - flag indicating whether page orientation is landscape or portrait
#'@param fontsize - font size (can be 'small')
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details Uses [extractMDFR.Results.ParameterValues()] to get a dataframe of parameters.
#'If format="latex", the package \pkg{tables} is used to create and print a table
#'in latex format. Output format for values and standard deviations is 3 significant figures.
#'
#'Specific tables can be created/selected by specifying non-default values for categories,
#'processes, types, and params.
#'
#'@import dplyr
#'@import tables
#'@import wtsMarkdown
#'
#'@md
#'
#'@export
#'
printResults.ParameterValues<-function(objs=NULL,
                                       mdfr=NULL,
                                       categories="all",
                                       processes="all",
                                       types="all",
                                       params="all",
                                       format=c("latex","docx"),
                                       landscape=FALSE,
                                       fontsize="11pt",
                                       verbose=FALSE){
    if (verbose) message("starting rCompTCMs::printResults.ParameterValues().\n");
    options(stringsAsFactors=FALSE);

    mdfrp<-mdfr;
    if (is.null(mdfrp)){
        mdfrp<-extractMDFR.Results.ParameterValues(objs,verbose=verbose);
    }
    mdfr<-NULL;
    mdfrp$label[mdfrp$label==""]<-" ";#--expand empty labels to single spaces

    format<-format[1];
    if (format=="latex"){
        # if (fontsize=="small") cat("\\begin{small}\n")
        # if (fontsize=="tiny") cat("\\begin{tiny}\n")
        uCs<-unique(mdfrp$category);
        if (categories!="all") uCs = categories;
        for (uC in uCs){
            #--testing: uC = 'selectivity'; uC = 'surveys';
            if (verbose) message("Processing category = ",uC,"\n",sep='')
            mdfrpp<-mdfrp[mdfrp$category==uC,];
            if (nrow(mdfrpp)>0){
                if (uC!="selectivity") {
                    uPs<-unique(mdfrpp$process);
                    if (processes!="all") uPs = processes;
                    for (uP in uPs){
                        #--testing: uP = uPs[1];
                        if (verbose) message("\tProcessing process = ",uP,"\n",sep='');
                        caption<-paste0("Model parameter lower and upper limits, estimated values and standard deviations for ",uP,".");
                        mdfrppp<-mdfrpp[mdfrpp$process==uP,];
                        if (nrow(mdfrppp)>0){
                            uTs<-unique(mdfrppp$type);
                            if (types!="all") uTs = types;
                            for (uT in uTs){
                                #--testing: uT=uTs[1];
                                if (verbose) message("\t\tProcessing type = ",uT,"\n",sep='');
                                mdfr<-mdfrppp[mdfrppp$type==uT,];
                                str = createLatexForTable.ParamValues.Numbers(mdfr,
                                                                              params=params,
                                                                              fontsize=fontsize,
                                                                              table_type=table_type,
                                                                              caption=caption,
                                                                              label=label,
                                                                              conn=NULL,
                                                                              verbose=verbose);
                                # if (nrow(mdfr)>0){
                                #     uNs = unique(mdfr$name);
                                #     if (params!="all") uNs = params;
                                #     if (verbose) message("creating table for ",uNs);
                                #     mdfr1 = mdfr %>% dplyr::filter(name %in% uNs);
                                #     if (nrow(mdfr1)>0) {
                                #         if (verbose) message("Creating a table for ",nrow(mdfr1)," parameters.")
                                #         if (stringr::str_trim(uT) %in% c("param_init_number","param_init_bounded_number")){
                                #             if (verbose) message("Using table format 1")
                                #             if (verbose) message("names = ",paste(mdfr$name,collapse=" "));
                                #             pnms = stringr::str_extract(mdfr1$name,"(.*?)(?=\\[)");                    #--extract just parameter names
                                #             if (verbose) message("pnms = ",paste(pnms,collapse=" "));
                                #             pids = as.numeric(stringr::str_extract(mdfr1$name,"(?<=\\[)(.*?)(?=\\])"));#--extract just parameter indices
                                #             if (verbose) message("pids = ",paste(pids,collapse=" "));
                                #             nd   = ceiling(max(log10(pids+0.1),na.rm=TRUE));#--max number of digits
                                #             if (verbose) message("nd = ",nd);
                                #             pids = format(pids,width=nd)
                                #             mdfr1%<>%
                                #                  dplyr::select(case,name,label,index,scale=parameter_scale,min=min_param,max=max_param,
                                #                                `arith value`=final_arith_value,`param value`=final_param_value,stdv) %>%
                                #                  dplyr::mutate(name  = as.factor(paste0(pnms,"[",pids,"]")),
                                #                                label = as.factor(label),
                                #                                scale = as.factor(tolower(scale)),
                                #                                lower = as.factor(signif(round(min,digits=6),digits=3)),
                                #                                upper = as.factor(signif(round(max,digits=6),digits=3)),
                                #                                value = signif(round(`param value`,digits=6),digits=3),
                                #                                stdv  = signif(round(stdv,digits=6),digits=3));
                                #             tbr<-tables::tabular(Justify("l","l")*Factor(name)*
                                #                                   (Justify("r","r")*(Factor(label)*
                                #                                      Factor(scale)*Factor(lower)*Factor(upper)))*DropEmpty()~
                                #                                        Factor(case)*(value+stdv)*Format(scientific=FALSE,digits=3)*wtsUtilities::Sum,
                                #                                  data=mdfr1);
                                #             labelsForRowFactors =  c("name","label","scale","lower","upper");
                                #             tbr = wtsMarkdown::fixColLabelsForRowFactors(tbr,labelsForRowFactors,dim=2,verbose=verbose);
                                #             colLabels(tbr)<-colLabels(tbr)[c(2,3),];
                                #             wtsMarkdown::writeTabularToLatex(tbr,caption,label=uP,type="longtable",conn="");
                                #             cat("\n\\clearpage\n")
                                #         } else {
                                #             # if (verbose) message("Using table format 2")
                                #             # mdfr1%<>%
                                #             #      dplyr::select(case,name,label,index,scale=parameter_scale,min=min_param,max=max_param,
                                #             #                    `arith value`=final_arith_value,`param value`=final_param_value,stdv) %>%
                                #             #      dplyr::mutate(label = as.factor(label),
                                #             #                    scale = as.factor(tolower(scale)),
                                #             #                    lower = as.factor(signif(round(min,digits=6),digits=3)),
                                #             #                    upper = as.factor(signif(round(max,digits=6),digits=3)),
                                #             #                    value = signif(round(`param value`,digits=6),digits=3),
                                #             #                    stdv  = signif(round(stdv,digits=6),digits=3)) %>%
                                #             #     dplyr::group_by(case,name,label) %>%
                                #             #     dplyr::mutate(vwd=)
                                #             #
                                #             # tbr<-tables::tabular(Justify("l","l")*Factor(name)*
                                #             #                       (Justify("r","r")*(Factor(label)*
                                #             #                          Factor(scale)*Factor(lower)*Factor(upper)))*DropEmpty()~
                                #             #                            Factor(case)*(value+stdv)*Format(scientific=FALSE,digits=3)*wtsUtilities::Sum,
                                #             #                      data=mdfr1);
                                #             # labelsForRowFactors =  c("name","label","scale","lower","upper");
                                #             # tbr = fixColLabelsForRowFactors(tbr,labelsForRowFactors,dim=2,verbose=verbose);
                                #             # colLabels(tbr)<-colLabels(tbr)[c(2,3),];
                                #             # wtsMarkdown::writeTabularToLatex(tbr,caption,label=uP,type="longtable",conn="");
                                #             # cat("\n\\clearpage\n")
                                #         }
                                #     }#--nrow(mdfr1)>0
                                # }#--nrow(mdfr)>0
                            }#--uT
                        }#--nrow(mdfrpp)>0
                    }#--uP
                } else {  #--uC=="selectivity
                    # for (uS in list(S1="pS1",S2="pS2",S3=c("pS3","pS4"))){
                    #     if (verbose) message("\t\tProcessing selectivity parameters = ",paste0(uS,collapse=", "),"\n",sep='');
                    #     mdfr<-mdfrpp[stringr::str_sub(mdfrpp$name,1,3) %in% uS,];
                    #     mdfr$parameter_scale<-as.factor(mdfr$parameter_scale);
                    #     mdfr$min_arith      <-as.factor(mdfr$min_arith);
                    #     mdfr$max_arith      <-as.factor(mdfr$max_arith);
                    #     mdfr$min_param      <-as.factor(mdfr$min_param);
                    #     mdfr$max_param      <-as.factor(mdfr$max_param);
                    #     mdfr$label          <-as.factor(mdfr$label);
                    #     mdfr$arith_value<-signif(mdfr$final_arith_value,digits=3);
                    #     mdfr$param_value<-signif(mdfr$final_param_value,digits=3);
                    #     mdfr$stdv       <-signif(mdfr$stdv,digits=3);
                    #     caption<-paste0("Parameter values and standard deviations for ",paste0(uS,collapse=", "),"-type selectivity parameters.");
                    #     tbr<-tables::tabular(Factor(label)*Factor(parameter_scale)*
                    #                              Factor(min_arith)*Factor(max_arith)*Factor(min_param)*Factor(max_param)*DropEmpty(which="row")~
                    #                              Factor(case)*(arith_value+param_value+stdv)*Format(scientific=FALSE,digits=3)*wtsUtilities::Sum,data=mdfr);
                    #     # tbr<-tables::tabular(Factor(label)*Factor(name)*Factor(index)*Factor(min)*Factor(max)*DropEmpty()~
                    #     #                          Factor(case)*(value+stdv)*wtsUtilities::Sum,data=mdfr);
                    #     colLabels(tbr)<-colLabels(tbr)[c(2,3),];
                    #     # Hmisc::latex(tbr,options=list(tabular="longtable",
                    #     #                        toprule=paste0("\\caption{",caption,"} \\\\
                    #     #                                        \\hline")));
                    #     wtsMarkdown::writeTabularToLatex(tbr,caption,label=uS,type="longtable",conn="");
                    #     cat("\n\\clearpage\n")
                    # }#--uS
                    # for (uS in list(S1="pDevsS1")){
                    #     #--testing: uS = list(S1="pDevsS1")[[1]];
                    #     if (verbose) message("\t\tProcessing selectivity parameters = ",paste(uS,collapse=", "),"\n",sep='');
                    #     mdfr<-mdfrpp[stringr::str_sub(mdfrpp$name,1,7) %in% uS,];
                    #     mdfr$parameter_scale<-as.factor(mdfr$parameter_scale);
                    #     mdfr$index          <-as.factor(mdfr$index);
                    #     mdfr$min_index      <-as.factor(mdfr$min_index)
                    #     mdfr$max_index      <-as.factor(mdfr$max_index)
                    #     mdfr$min_arith      <-as.factor(mdfr$min_arith);
                    #     mdfr$max_arith      <-as.factor(mdfr$max_arith);
                    #     mdfr$min_param      <-as.factor(mdfr$min_param);
                    #     mdfr$max_param      <-as.factor(mdfr$max_param);
                    #     mdfr$label          <-as.factor(mdfr$label);
                    #     mdfr$arith_value<-signif(mdfr$final_arith_value,digits=3);
                    #     mdfr$param_value<-signif(mdfr$final_param_value,digits=3);
                    #     mdfr$stdv       <-signif(mdfr$stdv,digits=3);
                    #     caption<-paste0("Parameter values and standard deviations for ",uS,"-type selectivity parameters.");
                    #     tbr<-tables::tabular(Factor(label)*Factor(index)*Factor(min_index)*Factor(max_index)*Factor(parameter_scale)*
                    #                              Factor(min_arith)*Factor(max_arith)*Factor(min_param)*Factor(max_param)*DropEmpty()~
                    #                              Factor(case)*(arith_value+param_value+stdv)*Format(scientific=FALSE,digits=3)*wtsUtilities::Sum,data=mdfr);
                    #     colLabels(tbr)<-colLabels(tbr)[c(2,3),];
                    #     # cat("\n\nHmisc version:\n")
                    #     # Hmisc::latex(tbr,file="",options=list(tabular="longtable",
                    #     #                        toprule=paste0("\\caption{",caption,"} \\\\
                    #     #                                        \\hline")));
                    #     # cat("\n\nwtsMarkdown version:\n")
                    #     wtsMarkdown::writeTabularToLatex(tbr,caption,label=uS,type="longtable",conn="");
                    #     cat("\n\\clearpage\n")
                    # }#--uS
                }
            }#-nrow(mdfrpp)>0
        }#--uC
        # if (fontsize=="small") cat("\\end{small}\n")
        # if (fontsize=="tiny") cat("\\end{tiny}\n")
    } else {  #--format!="latex"

    }
    if (verbose) message("finished rCompTCMs::compareResults.ParameterValues().\n");
}
