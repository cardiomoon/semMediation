library(jtools)
library(ggplot2)
glbwarm=read.csv("./inst/pmacro/data/glbwarm.csv",stringsAsFactors = FALSE)
glbwarm

fit=lm(govact~negemot*age+posemot+ideology+sex,data=glbwarm)
summary(fit,digits=3)

fit1=lm(govact~negemot*I(age-50)+posemot+ideology+sex,data=glbwarm)
summary(fit1,digits=3)
fit1$coef[1]
fit1$coef[2]

interact_plot(fit,pred=negemot,modx=age)
interact_plot(fit,pred=negemot,modx=age,modx.values=c(30,50,70))+
    stat_function(fun=function(x) {fit1$coef[1]+fit1$coef[2]*x},color="red")

disaster=read.csv("./inst/pmacro/data/disaster.csv",stringsAsFactors = FALSE)
disaster
fit=lm(justify~skeptic*frame,data=disaster)
condEffect2(fit,"skeptic","frame")
condEffect(data=disaster,Y="justify",X="skeptic",M="frame")
interact_plot(fit,"skeptic","frame")

fit=lm(mpg~wt*hp+disp+cyl+drat,data=mtcars)
summary(fit)
interact_plot(fit,pred=wt,modx=hp)

modValues=mean(mtcars$hp)+c(-1,0,1)*sd(mtcars$hp)
modValues
fit1=lm(mpg~wt*I(hp-modValues[2])+disp+cyl+drat,data=mtcars)
summary(fit1)

mtcars
interact_plot(fit,pred="wt",modx="hp")+
    stat_function(fun=function(x){fit1$coef[1]+mean(mtcars$disp)*fit1$coef[4]+mean(mtcars$cyl)*fit1$coef[5]+mean(mtcars$drat)*fit1$coef[6]+fit1$coef[2]*x},color="red")


fit=lm(mpg~wt*hp+disp+cyl+drat,data=mtcars)
pred="wt"
modx="hp"
require(stringr)
require(jtools)
require(ggplot2)
require(semMediation)

condEffect2(fit,pred="negemot",modx="age",modx.values=c(30,40,50,60,70))
condEffect2(fit,pred="negemot",modx="age")
condEffect2(fit,pred="negemot",modx="age",probs=c(0.16,0.4,0.6,0.84))

glbwarm=read.csv("./inst/pmacro/data/glbwarm.csv",stringsAsFactors = FALSE)
glbwarm

fit=lm(govact~negemot*age+posemot+ideology+sex,data=glbwarm)
fit
pred="negemot";modx="age"

modx.values=c(30,50,70)

fit=lm(mpg~wt*hp+disp+cyl+drat,data=mtcars)
pred="wt"
modx="hp"
interact_plot(fit,pred="wt",modx="hp")
interact_plot(fit,pred="hp",modx="wt")
interact_plot(fit,pred=hp,modx=wt,modx.values=c(2.2,3.2,4.2))

#'Draw interact plot with moderation effect
#'@param fit A regression model
#'@param pred The name of the predictor variable
#'@param modx The name of the moderator variable
#'@param modx.values For which values of the moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators.
#'@param probs A vector of probability weights for obtaining the elements of the vector being sampled. Default is NULL.
#'@param digits integer indicating the number of decimal places
#'@param show.Effect A logical
#'@param max.ylev An integer indicating the maximum number of levels of modifier variable
#'@param arrowlength arrow length
#'@param ... Further argument to be passed to jtools::interact_plot
#'@importFrom ggplot2 annotate
#'@importFrom jtools interact_plot
#'@importFrom stringr str_replace
#'@export
#'@examples
#'fit=lm(mpg~wt*am+disp+cyl+drat,data=mtcars)
#'condEffect2(fit,pred="wt",modx="am")
#'condEffect2(fit,pred="wt",modx="am",switchVars=TRUE)
#'fit=lm(mpg~wt*hp+disp+cyl+drat,data=mtcars)
#'condEffect2(fit,pred="wt",modx="hp")
#'condEffect2(fit,pred="wt",modx="hp",show.Effect=FALSE)
#'condEffect2(fit,pred="wt",modx="hp",switchVars=TRUE)
#'condEffect2(fit,pred="wt",modx="hp",modx.values=c(100,150,200))
#'condEffect2(fit,pred="wt",modx="hp",probs=c(0.16,0.4,0.6,0.84))
condEffect2=function(fit,pred,modx,modx.values=NULL,
                     probs=NULL,digits=1,show.Effect=TRUE,switchVars=FALSE,
                     max.ylev=6,arrowlength=0.05,...){

    df=fit$model

    if(switchVars){
        temp<-pred
        pred<-modx
        modx<-temp
    }
    if(!is.null(probs)){
        modValues = quantile(df[[modx]],probs)
    } else if(!is.null(modx.values)){
        modValues=modx.values
    } else if(length(unique(df[[modx]]))>max.ylev) {
        modValues=round(mean(df[[modx]])+c(-1,0,1)*sd(df[[modx]]),digits)
    } else{
        modValues=sort(unique(df[[modx]]))
    }
    count=length(modValues)

    temp=paste0("interact_plot(fit,pred=",pred,",modx=",modx,
                ",modx.values=",paste0("c(",paste0(modValues,collapse=","),")"),",...)")

    # print(temp)
    p<-eval(parse(text=temp))

    if(show.Effect){
        intercept=0
        if(ncol(df)>3){
            for(i in 4:ncol(df)) {
                intercept=intercept+mean(df[[i]])*fit$coef[i]
            }
        }
        myfit=lapply(1:count,function(i){
            temp=deparse(fit$call)
            temp
            if(switchVars){
                temp1=str_replace(temp,modx,"temp")
                temp1=str_replace(temp1,pred,modx)
                temp1=str_replace(temp1,"temp",pred)
            }
            temp1=str_replace(temp,modx,paste0("I(",modx,"- modValues[",i,"])"))
            # cat("temp1=",temp1,"\n")
            eval(parse(text=temp1))
        })
        fun=lapply(1:count,function(i){
            function(x) {myfit[[i]]$coef[1]+intercept+
                    myfit[[i]]$coef[2+ifelse(switchVars,1,0)]*x}

        })
        midpos=relpos(p,c(0.5,0.5))
        if(count>2){

            startpos=relpos(p,c(0,0))
            minx=startpos[1]
            miny=startpos[2]
            endpos=relpos(p,c(1,1))
            maxx=endpos[1]
            maxy=endpos[2]
            xlength=(maxx-minx)*arrowlength
            ylength=(maxy-miny)*arrowlength
            endx=minx+(maxx-minx)*(1:count)/(count+1)
            endy=unlist(lapply(1:count,function(i) {fun[[i]](endx[i])}))
            startx=endx+ifelse(endx>=midpos[1],-1,1)*xlength
            starty=endy+ifelse(endy>=midpos[2],1,-1)*ylength

            for(i in 1:count){
                label=paste0("italic(theta)[italic(X) %->% italic(Y) ] ==",
                             round(myfit[[i]]$coef[2],3),
                             " (italic(W) ==",round(modValues[i],1),")")
                hjust=ifelse(endx[i]>=midpos[1],1.02,-0.02)
                p<-p+
                    annotate("text",x=startx[i],y=starty[i],label=label,parse=TRUE,
                             hjust=hjust)+
                    annotate("segment",x=startx[i],y=starty[i],
                             xend=endx[i],yend=endy[i],
                             arrow=arrow(angle = 15,length=unit(0.15,"inches"),
                                         type="closed"))
            }
        } else{
            if(is.null(probs)) probs=c(0.16,0.5,0.84)
            xvalues=quantile(df[[pred]],probs)
            yhat0=fun[[1]](xvalues)
            yhat1=fun[[2]](xvalues)

            myfit=lapply(1:length(xvalues),function(i){
                temp=deparse(fit$call)
                temp
                if(switchVars){
                    temp1=str_replace(temp,modx,"temp")
                    temp1=str_replace(temp1,pred,modx)
                    temp1=str_replace(temp1,"temp",pred)
                }
                temp1=str_replace(temp,pred,paste0("I(",pred,"-", xvalues[i],")"))
                # cat("temp1=",temp1,"\n")
                eval(parse(text=temp1))
            })

            labels<-end<-vjust<-c()
            for(i in 1:length(xvalues)){

            temp=paste0("list(italic(theta)[italic(X) %->% italic(Y) ] ==",
                         round(myfit[[i]]$coef[3],3),
                         " (italic(W) ==",round(xvalues[i],1),")",
                        ",p==",round(summary(myfit[[i]])$coef[3,4],3),")")
            labels=c(labels,temp)
            # print(myfit[[i]]$coef)
            end=c(end,ifelse(myfit[[i]]$coef[2]>=0,1,-1))
            vjust=c(vjust,ifelse(myfit[[i]]$coef[3]>=0,-0.02,1.02))

            }
            labels
            slope=ifelse(fit$coef[2]>=0,1,-1)


            df=data.frame(x=xvalues,y=yhat0,yend=yhat1,label=labels,end=end,vjust=vjust)
            if(slope>0){
                df$hjust=ifelse(df$vjust>0,-0.02,1.02)
            } else{
                df$hjust=ifelse(df$vjust>0,1.02,-0.02)
            }
            df$hjust[df$x<relpos(p,c(0.4,0))[1]]=-0.02

            # print(slope)
            # print(df)

            p<-p+
                annotate("segment",x=df$x,y=df$y,
                             xend=df$x,yend=df$yend,
                        arrow=arrow(angle = 15,length=unit(0.15,"inches"),
                        type="closed"))+
                annotate("text",x=df$x,y=ifelse(df$end*slope>0,df$yend,df$y),label=df$label,hjust=df$hjust,vjust=df$vjust,parse=TRUE)
        }
    }
    p
}


interact_plot(fit,pred="negemot",modx="age")
johnson_neyman(fit,pred="negemot",modx="age")

summary(fit)
str(fit$coef)
str(summary(fit))
X="negemot"
M="age"
XM=paste(X,M,sep=":")
fit$coef[X]
fit$coef[XM]
fit$coef[M]
label=paste0("italic(theta) [italic(X) %->% italic(Y)] == ",
              sprintf("%.03f",fit$coef[X]),"+",sprintf("%.03f",fit$coef[XM]),"*italic(W)")
label
p<-johnson_neyman(fit,pred=negemot,modx=age)$plot

pos=relpos(p)
pos
p+
annotate("text",x=pos[1],y=pos[2],label=label,parse=TRUE)

#' get relative position og a ggplot object
#' @param p A ggplot object
#' @param pos A numeric vector of xposition and y position
relpos=function(p,pos=c(0.5,0.9)){
    xmin=layer_scales(p)$x$range$range[1]
    xmax=layer_scales(p)$x$range$range[2]
    ymin=layer_scales(p)$y$range$range[1]
    ymax=layer_scales(p)$y$range$range[2]
    x= xmin+(xmax-xmin)*pos[1]
    y= ymin+(ymax-ymin)*pos[2]

   c(x,y)
}

relpos(p)
