#'
#'@title Function to compare fishery retention functions by year among several models
#'
#'@description This function compares fishery retention functions by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param cast - formula to exclude factors from "averaging" over
#'@param fleets - vector of feets to plot, or "all"
#'@param years - vector of years to show, or 'all' to show all years
#'@param dodge - width to dodge overlapping series
#'@param mxy - max number of years per page
#'@param singlePlot - flag to plot all years on single plot (be sure to adjust facet_grid)
#'@param facet_wrap - ggplot2 formula to produce figure with wrapped facets
#'@param facet_grid - ggplot2 formula to produce figure with gridded facets
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return lists of ggplot2 objects, nested by fishery or an empty list if year is NULL
#'
#'@details None.
#'
#'@import magrittr
#'@import dplyr
#'@import ggplot2
#'@import stringr
#'
#'@export
#'
compareResults.Fisheries.RetFcns<-function(objs,
                                           cast='y+x',
                                           fleets="all",
                                           years='all',
                                           dodge=0.2,
                                           mxy=15,
                                           singlePlot=FALSE,
                                           facet_wrap=NULL,
                                           facet_grid=ifelse(singlePlot,"x~case","y~x"),
                                           showPlot=FALSE,
                                           pdf=NULL,
                                           verbose=FALSE){
    if (verbose) message("Starting rCompTCMs::compareResults.Fisheries.RetFcns().\n");
    options(stringsAsFactors=FALSE);

    if (is.null(years)) return(list());

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    if (is.data.frame(objs)) {
        mdfr<-objs;
    } else {
        mdfr<-extractMDFR.Fisheries.RetFcns(objs,fleets=fleets,cast=cast,years=years,verbose=verbose);
        if (is.null(mdfr)||(nrow(mdfr)==0)) {
            warning("rCompTCMs::compareResults.Fisheries.RetFcns: returning empty list.")
            return(list()); #empty list
        }
    }

    #---------NEW CODE------------
    #--identify stanzas
    tmp = mdfr %>% tidyr::pivot_wider(names_from=z,values_from=val);
    cols = stringr::str_subset(names(tmp ),stringr::fixed("y"),negate=TRUE);
    mdfr = tmp %>% dplyr::group_by(dplyr::across(dplyr::all_of(cols))) %>%
                   dplyr::summarize(ymn=min(y),
                                    ymx=max(y)) %>%
                   dplyr::ungroup() %>%
                   tidyr::pivot_longer(cols=cols[10:length(cols)],names_to="z",values_to="val") %>%
                   dplyr::mutate(z=as.numeric(z),
                                 stanza=ifelse(ymn!=ymx,paste0(ymn,"-",ymx),ymn));
    #--use stanza as faceting variable

    #----------------------------------
    #retention functions
    #----------------------------------
    plots<-list();
    uF<-unique(mdfr$fleet);
    if (fleets[1]!="all") uF<-fleets;
    for (f in uF){
        if (verbose) message("Plotting fleet",f,"\n")
        mdfrp<-mdfr[mdfr$fleet==f,];
        # uY<-unique(mdfrp$y);
        subPlots<-list();
        # if(!singlePlot){
        #     for (pg in 1:ceiling(length(uY)/mxy)){
        #         mdfrpp<-mdfrp[mdfrp$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],];
        #         p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
        #                        facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
        #                        xlab='size (mm CW)',ylab='retention',units='',lnscale=FALSE,
        #                        title=f,
        #                        colour='case',guideTitleColor='',
        #                        shape='case',guideTitleShape='',
        #                        showPlot=FALSE);
        #         if (showPlot||!is.null(pdf)) print(p);
        #         cap<-paste0("\n  \nFigure &&figno. Retention functions for ",f,"(",pg," of ",ceiling(length(uY)/mxy),").  \n  \n")
        #         subPlots[[cap]]<-p;
        #     }#pg
        # } else {
        #     p<-plotMDFR.XY(mdfrp,x='z',value.var='val',agg.formula=NULL,
        #                    facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
        #                    xlab='size (mm CW)',ylab='retention',units='',lnscale=FALSE,
        #                    title=f,
        #                    colour='y',guideTitleColour='year',
        #                    shape='y',guideTitleShape='year',
        #                    showPlot=FALSE);
        #     if (showPlot||!is.null(pdf)) print(p);
        #     cap<-paste0("\n  \nFigure &&figno. Retention functions for ",f,".  \n  \n")
        #     subPlots[[cap]]<-p;
        # }
        rws = mdfrp %>% dplyr::distinct(x,m,s);
        for (i in 1:nrow(rws)){
            rw = rws[i,];
            str = stringr::str_trim(stringr::str_remove_all(paste(rw$s,rw$m,rw$x),stringr::fixed("all")));
            mdfrpp = mdfrp %>% dplyr::inner_join(rw,by=c("x","m","s"));
            nStanzas = length(unique(mdfrpp$stanza));
            facets = facet_wrap(~stanza,ncol=1);
            if (nStanzas>3){facets = facet_wrap(~stanza,ncol=floor(sqrt(nStanzas)))}
            p = ggplot(mdfrpp,aes(x=z,y=val,colour=case)) +
                  geom_line() +
                  facets + ylim(0,1) +
                  xlab('size (mm CW)')+ylab('retention')+ggtitle(f)+
                  theme(panel.background=element_rect(colour="black",fill="white"),
                        panel.border=element_rect(colour="black",fill=NA),
                        panel.spacing=unit(0.1,"cm"));
            cap<-paste0("\n  \nFigure &&figno. Retention functions for ",str," crab in ",f,".  \n  \n");
            subPlots[[cap]]<-p;
        }
        plots[[f]]<-subPlots;
    }#uF

    if (verbose) cat("rCompTCMs::compareResults.Fisheries.RetFcns(): Done!\n");
    return(plots)
}
