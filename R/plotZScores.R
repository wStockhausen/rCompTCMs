#'
#'@title Plot z-scores from a set of model runs
#'
#'@description Function to plot z-scores from a set of model runs.
#'
#'@param dfr - dataframe
#'@param x - column name for x axis (default = 'year')
#'@param y - column name for y axis (default='z-score')
#'@param color - column name for color levels (or NULL)
#'@param shape - column name for shape levels (or NULL)
#'@param size - size for shapes
#'@param position - indicates ggplot2 position_ to use ('dodge','jitter','identity',)
#'@param dodge - value to position dodge
#'@param facets - string giving faceting formula for facet_grid
#'@param facet.scales - ggplot2 scales option for facet_grid
#'@param xlab - label for x axis
#'@param ylab - label for y axis
#'@param title - title for plot
#'@param legend - legend title
#'@param xlims - limits for x axis
#'@param ylims - limits for y axis
#'@param alpha - transparency value to use
#'@param plotPoints - flag to include points on plots
#'@param colour_scale - ggplot2 scale_colour object (default is [ggplot2::scale_colour_hue()])
#'@param fill_scale - ggplot2 scale_fill object (default is [ggplot2::scale_fill_hue()])
#'@param showSmooths - flag (T/F) to show smooth fits to z-scores
#'@param showPlot - flag (T/F) to show plot immediately
#'
#'@return ggplot2 object
#'
#'@details Smooth fits are grouped by 'color'.
#'
#'@import ggplot2
#'
#'@md
#'
#'@export
#'
plotZScores<-function(dfr,
                      x='y',
                      y='zscore',
                      color=NULL,
                      shape=NULL,
                      size=2,
                      position='identity',
                      dodge=0.2,
                      facets=NULL,
                      facet.scales='fixed',
                      xlab='year',
                      ylab=NULL,
                      title=NULL,
                      legend=NULL,
                      xlims=NULL,
                      ylims=NULL,
                      alpha=1,
                      plotPoints=TRUE,
                      colour_scale=ggplot2::scale_colour_hue(),
                      fill_scale=ggplot2::scale_fill_hue(),
                      showSmooths=TRUE,
                      showPlot=FALSE){
    std_theme = ggplot2::theme(plot.background =ggplot2::element_blank(),
                               panel.background=ggplot2::element_blank(),
                               panel.border    =ggplot2::element_rect(colour="black",fill=NA),
                               panel.grid      =ggplot2::element_blank(),
                               panel.spacing   =unit(0,units="cm"));
    p <- ggplot(dfr,aes_string(x=x,y=y));
    p <- p + geom_hline(yintercept=0.0,color='black',size=1);
    if (showSmooths) p<-p+geom_smooth(mapping=aes_string(group=color,fill=color,colour=color),alpha=0.25);
    if (plotPoints){
        if (position=='dodge'){
            p <- p + geom_point(aes_string(shape=shape,color=color),size=size,alpha=alpha,
                                position=position_dodge(width=dodge));
        } else {
            p <- p + geom_point(aes_string(shape=shape,color=color),size=size,alpha=alpha,
                                position=position);
        }
    }
    p <- p + coord_cartesian(xlim=xlims,ylim=ylims)
    p <- p + labs(x=xlab,y=ylab);
    p <- p + ggtitle(title);
    p <- p + colour_scale;
    p <- p + fill_scale;
    if (!is.null(legend)) p <- p + guides(color=guide_legend(legend),shape=guide_legend(legend))
    if (!is.null(facets)) p <- p + facet_grid(facets,scales=facet.scales);
    p = p + std_theme;
    if (showPlot) print(p);

    return(p);
}
