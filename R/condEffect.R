#' Make interaction plot
#' @param data A data.frame
#' @param Y The name of dependent variable
#' @param X The name of independent variable
#' @param M The name of modifier variable
#' @param probs A vector of probability weights for obtaining the elements of the vector being sampled.
#'              Default value is c(0.16,0.5,0.84)
#' @param showEffect A logical
#' @importFrom jtools interact_plot
#' @importFrom ggplot2 annotate
#' @importFrom stats as.formula lm quantile
#' @export
#' @examples
#' condEffect(data=mtcars,Y="mpg",X="wt",M="am")
#' condEffect(data=mtcars,Y="mpg",X="wt",M="hp")
condEffect=function(data=mtcars,Y="mpg",X="wt",M="am",probs=c(0.16,0.5,0.84),showEffect=TRUE){
    # data=disaster;Y="justify";X="skeptic";M="frame"
    # probs=c(0.16,0.5,0.84)
    # data=mtcars;Y="mpg";X="wt";M="hp";probs=c(0.16,0.5,0.84);showEffect=TRUE
    temp=paste0(Y,"~",X,"*",M)
    fit=lm(as.formula(temp),data=data)
    summary(fit)


    if(length(unique(data[[M]]))==2){
        fun0=function(x) {fit$coef[1]+fit$coef[2]*x}
        fun1=function(x) {fit$coef[1]+fit$coef[3]+(fit$coef[2]+fit$coef[4])*x}
        modValues=quantile(data[[X]],probs=probs)
        modValues
        yhat1=fun0(modValues)
        yhat2=fun1(modValues)
        effect<-p<-c()
        for(i in 1:length(modValues)){
            temp1=paste0(Y,"~",M,"*I(",X,"-",modValues[i],")")
            fit1=lm(as.formula(temp1),data=data)
            effect=c(effect,fit1$coef[2])
            p<-c(p,summary(fit1)$coef[2,4])
        }

        df=data.frame(x=modValues,y0=yhat1,y1=yhat2,effect=effect,p=p)
        df$label1=paste0("theta [italic(X) %->% italic(Y)]")
        df$label2=paste0("list((italic(W)==",df$x,")==",
                         sprintf("%4.3f",df$effect),
                         ",italic(p)==",sprintf("%.03f",df$p),")")
        df$vjust=ifelse(df$effect>=0,0.2,-0.1)
        df
        temp=paste0("interact_plot(fit,pred=",X,",modx=",M,")")
        temp
        p<-eval(parse(text=temp))
        if(showEffect)
            p<-p+ annotate("segment",x=df$x,y=df$y0,xend=df$x,yend=df$y1,lty=2,color="red",
                           arrow=arrow(angle=10,length=unit(0.15,"inches"),type="closed"))+
            annotate("text",x=df$x,y=df$y1+df$vjust,label=df$label1,parse=TRUE,hjust=1.1,vjust=1)+
            annotate("text",x=df$x,y=df$y1+df$vjust,label="|",vjust=1)+
            annotate("text",x=df$x,y=df$y1+df$vjust,label=df$label2,parse=TRUE,hjust=-0.01,vjust=1)
    } else{
        # modValues=mean(data[[M]])-c(-1,0,1)*sd(data[[M]])
        # modValues
        # effect<-p<-c()
        # i=1
        # for(i in 1:length(modValues)){
        #     temp1=paste0(Y,"~",M,"*I(",X,"-",modValues[i],")")
        #     fit1=lm(as.formula(temp1),data=data)
        #     summary(fit1)
        #     effect=c(effect,fit1$coef[2])
        #     p<-c(p,summary(fit1)$coef[2,4])
        # }
        # df=data.frame(x=modValues,effect=effect,p=p)
        temp=paste0("interact_plot(fit,pred=",X,",modx=",M,")")
        temp
        p<-eval(parse(text=temp))

    }
    p

}

#' get relative position og a ggplot object
#' @param p A ggplot object
#' @param pos A numeric vector of xposition and y position
#' @importFrom ggplot2 layer_scales
#' @export
relpos=function(p,pos=c(0.5,0.9)){
    xmin=layer_scales(p)$x$range$range[1]
    xmax=layer_scales(p)$x$range$range[2]
    ymin=layer_scales(p)$y$range$range[1]
    ymax=layer_scales(p)$y$range$range[2]
    x= xmin+(xmax-xmin)*pos[1]
    y= ymin+(ymax-ymin)*pos[2]

    c(x,y)
}
