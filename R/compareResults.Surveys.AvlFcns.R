#'
#'@title Function to compare survey availability functions by year among several models
#'
#'@description This function compares survey availability functions by year
#'   among several models.
#'
#'@param objs - list of resLst objects or dataframe from call to \code{extractMDFR.Surveys.AvlFcns}
#'@param cast - formula to exclude factors from "averaging" over
#'@param fleets - vector of fleets to plot, or "all"
#'@param years - vector of years to show, or 'all' to show all years
#'@param dodge - width to dodge overlapping series
#'@param singlePlot - flag to plot all years on single plot (be sure to adjust facet_grid)
#'@param mxy - max number of years per page
#'@param facet_wrap - ggplot2 formula to produce figure with wrapped facets
#'@param facet_grid - ggplot2 formula to produce figure with gridded facets
#'@param plotPoints - flag to include points (default: FALSE)
#'@param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#'@param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
#'@param pdf - creates pdf, if not NULL
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object, or null
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Surveys.AvlFcns<-function(objs,
                                         cast='y+x',
                                         fleets="all",
                                         years='all',
                                         dodge=0.2,
                                         singlePlot=FALSE,
                                         mxy=15,
                                         facet_wrap=NULL,
                                         facet_grid=ifelse(singlePlot,"x~case","y~x"),
                                         plotPoints=FALSE,
                                         colour_scale=NULL,
                                         fill_scale=NULL,
                                         pdf=NULL,
                                         showPlot=FALSE,
                                         verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::compareResults.Surveys.AvlFcns().\n");
    options(stringsAsFactors=FALSE);

    std_theme = wtsPlots::getStdTheme();

    if (is.null(years)) return(list());

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    if (is.data.frame(objs)) {
        mdfr<-objs;
    } else {
        mdfr<-extractMDFR.Surveys.AvlFcns(objs,fleets=fleets,cast=cast,years=years,verbose=verbose);
        if (is.null(mdfr)) return(list()); #empty list
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
    #availability functions
    #----------------------------------
    plots<-list();
    uF<-unique(mdfr$fleet);
    if (fleets[1]!="all") uF<-fleets;
    for (f in uF){
        if (verbose) message("Plotting fleet",f,"\n")
        mdfrp<-mdfr[mdfr$fleet==f,];
        # if (!singlePlot){
        #     uY<-unique(mdfrp$y);
        #     for (pg in 1:ceiling(length(uY)/mxy)){
        #         mdfrpp<-mdfrp[mdfrp$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],];
        #         p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
        #                        facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
        #                        xlab='size (mm CW)',ylab='Availability',units='',lnscale=FALSE,
        #                        title=f,
        #                        colour='case',guideTitleColor='',
        #                        shape='case',guideTitleShape='',
        #                        showPlot=FALSE);
        #         if (showPlot||!is.null(pdf)) print(p);
        #         cap<-paste0("\n  \nFigure &&figno. Availability functions for ",f,"(",pg," of ",ceiling(length(uY)/mxy),").  \n  \n")
        #         plots[[cap]]<-p;
        #     }#pg
        # } else {
        #     p<-plotMDFR.XY(mdfrp,x='z',value.var='val',agg.formula=NULL,
        #                    facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
        #                    xlab='size (mm CW)',ylab='Availability',units='',lnscale=FALSE,
        #                    title=f,
        #                    colour='y',guideTitleColour='year',
        #                    shape='y',guideTitleShape='year',
        #                    showPlot=FALSE);
        #     if (showPlot||!is.null(pdf)) print(p);
        #     cap<-paste0("\n  \nFigure &&figno. Availability functions for ",f,".  \n  \n")
        #     plots[[cap]]<-p;
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
                  xlab('size (mm CW)')+ylab('availability')+ggtitle(f)+
                  std_theme;
            if (plotPoints) p <- p + geom_point(position=pd);
            if (!is.null(colour_scale)) p = p + colour_scale;
            if (!is.null(fill_scale))   p = p + fill_scale;
            cap<-paste0("\n  \nFigure &&figno. Availability functions for ",str," crab in the ",f," survey.  \n  \n");
            plots[[cap]]<-p;
        }
    }#--uF

    if (verbose) message("rCompTCMs::compareResults.Surveys.AvlFcns: Done!\n");
    return(plots)
}
