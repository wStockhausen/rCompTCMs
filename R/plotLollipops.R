#'
#' @title plot lollipops for fits to size composition data
#'
#' @description function to plot lollipops for fits to size composition data.
#'
#' @param mdfr - input data
#'
#' @return ggplot2 plot object
#'
#' @details Under construction!
#'
#' @import magrittr
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plotLollipops.ZCs<-function(mdfr,
                            type="zscores",
                            extreme=3,
                            dw=5,
                            xlab="size bin (mm CW)",
                            ylab="z-score",
                            facet_wrap=~y,
                            ncol=2){
   type_ = type[1];
   tst = mdfr %>% dplyr::filter(type==type_) %>%
                  dplyr::select(case,y,x,z,val) %>%
                  dplyr::mutate(ctg=ifelse(abs(val)<extreme,"ok","extreme")) %>%
                  dplyr::mutate(ctgf=factor(ctg,levels=c("ok","extreme")),
                                ctgz=ifelse(ctg=="ok",1,2));

   mx = max(extreme,max(abs(tst$val),na.rm=TRUE));
   p1 = ggplot(tst,aes(x=z,xend=z,colour=case,fill=case,shape=ctgf)) +
         geom_linerange(aes(ymin=0,ymax=val),position=position_dodge2(dw)) +
         geom_point(aes(y=val,size=ctgz),    position=position_dodge2(dw)) +
         geom_hline(yintercept= extreme,linetype=2,colour="black")+
         geom_hline(yintercept=-extreme,linetype=2,colour="black")+
         scale_y_continuous(limits=mx*c(-1,1),breaks=c(-extreme,0,extreme))+
         scale_size(range=c(1,2),guide=NULL)+scale_shape_manual(values=c(21,22))+
         labs(x=xlab,y=ylab) +
         theme(panel.spacing.y=unit(0.01,"in"),
               panel.spacing.x=unit(0.02,"in"),
               panel.background=element_rect(colour="black",fill="white"),
               panel.grid.major.y=element_line(colour="grey",linetype=2));
        if (!is.null(facet_wrap)) p = p + facet_wrap(facets,ncol=ncol);


   # p2 = ggplot(tst,aes(x=as.factor(y),y=val,colour=case,fill=case)) +
   #       geom_boxplot(alpha=1.0,outlier.shape = NA,colour="black") +
   #       geom_point(shape=21,colour="black",position=position_jitterdodge()) +
   #       geom_hline(yintercept= 3,linetype=2,colour="black") +
   #       geom_hline(yintercept=-3,linetype=2,colour="black") +
   #       scale_y_continuous(limits=mx*c(-1,1),breaks=c(-3,0,3)) +
   #       labs(x="year",y="z-score") +
   #       theme(panel.spacing.y=unit(0.01,"in"),
   #             panel.spacing.x=unit(0.02,"in"),
   #             panel.background=element_rect(colour="black",fill="white"),
   #             panel.grid.major.y=element_line(colour="grey",linetype=2));
   # return(list(p1=p1,p2=p2));
        return(p1)
}

#'
#' @title plot lollipops for fits to size composition data
#'
#' @description function to plot lollipops for fits to size composition data.
#'
#' @param mdfr - input data
#'
#' @return ggplot2 plot object
#'
#' @details Under construction!
#'
#' @import magrittr
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plotLollipops.ACD<-function(mdfr,
                            type="z-score",
                            extreme=3,
                            dw=1,
                            xlab="year",
                            ylab=type){
   type_ = type[1];
   tst = mdfr %>% dplyr::filter(type==type_) %>%
                  dplyr::select(case,y,x,m,s,val) %>%
                  dplyr::mutate(ctg=ifelse(abs(val)<extreme,"ok","extreme")) %>%
                  dplyr::mutate(ctgf=factor(ctg,levels=c("ok","extreme")),
                                ctgz=ifelse(ctg=="ok",1,2),
                                facet=paste0(x,"\n",m,"\n",s));

   mx = max(extreme,max(abs(tst$val),na.rm=TRUE));
   p1 = ggplot(tst,aes(x=y,colour=case,fill=case,shape=ctgf)) +
         geom_linerange(aes(ymin=0,ymax=val),position=position_dodge2(dw)) +
         geom_point(aes(y=val,size=ctgz),    position=position_dodge2(dw)) +
         geom_hline(yintercept= extreme,linetype=2,colour="black")+
         geom_hline(yintercept=       0,linetype=2,colour="black")+
         geom_hline(yintercept=-extreme,linetype=2,colour="black")+
         scale_y_continuous(limits=mx*c(-1,1),breaks=c(-extreme,0,extreme))+
         scale_size(range=c(1,2),guide=NULL)+scale_shape_manual(values=c(21,22))+
         labs(x=xlab,y=ylab,shape="") +
         facet_grid(facet~.,scales="free_y") +
         theme(panel.spacing.y=unit(0.01,"in"),
               panel.spacing.x=unit(0.02,"in"),
               panel.background=element_rect(colour="black",fill="white"),
               panel.grid=element_blank());
    return(p1)
}




