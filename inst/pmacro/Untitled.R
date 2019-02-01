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
