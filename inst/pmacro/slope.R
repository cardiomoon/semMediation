require(ggplot2)

ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
    stat_smooth(method="lm",se=FALSE)+
    stat_function(fun=fun[[1]],lty=2)+
    stat_function(fun=fun[[2]],lty=2)+
    stat_function(fun=fun[[3]],lty=2)

fit=lm(Sepal.Width~Sepal.Length*Species,data=iris)
summary(fit)

fun=list()
fun[[1]]=function(x){fit$coef[1]+fit$coef[2]*x}
fun[[2]]=function(x){fit$coef[1]+fit$coef[3]+(fit$coef[2]+fit$coef[5])*x}
fun[[3]]=function(x){fit$coef[1]+fit$coef[4]+(fit$coef[2]+fit$coef[6])*x}

intercept=c(fit$coef[1],fit$coef[1]+fit$coef[3],fit$coef[1]+fit$coef[4])
slope=c(fit$coef[2],fit$coef[2]+fit$coef[5],fit$coef[2]+fit$coef[6])
df=data.frame(intercept,slope)
df$label=paste0("italic(y)==",round(df$intercept,2),"+",round(df$slope,2),"*italic(x)")
df$radian=atan(df$slope)
df$x=c(5,6,7.2)
df$y=unlist(lapply(1:3,function(i){fun[[i]](df$x[i])}))
df$angle=df$radian*180/pi
df


ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
    stat_smooth(method="lm",se=FALSE)+
    geom_text(data=df,aes(x=x,y=y,label=label,angle=angle),color="black")+
    coord_fixed()

ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
    stat_smooth(method="lm",se=FALSE)+
    geom_text(data=df,aes(x=x,y=y,label=label,angle=angle2),color="black")+
    coord_fixed(ratio=2)


p<-ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
    stat_smooth(method="lm",se=FALSE)
p
ratio=getAspectRatio(p)
ratio

df$slope2=df$slope/ratio
df$radian2=atan(df$slope2)
df$angle2=df$radian2*180/pi
df$vjust=c(-0.5,1.5,-0.5)

p+coord_fixed(ratio=1/ratio)+
    geom_text(data=df,aes(x=x,y=y,label=label,angle=angle2),color="black",vjust=df$vjust,parse=TRUE,family="Times",size=5)



ratio=y/x

getAspectRatio=function(p){
    xmin=layer_scales(p)$x$range$range[1]
    xmax=layer_scales(p)$x$range$range[2]
    ymin=layer_scales(p)$y$range$range[1]
    ymax=layer_scales(p)$y$range$range[2]
    cat("xmin=",xmin,"\n")
    cat("xmax=",xmax,"\n")
    cat("ymin=",ymin,"\n")
    cat("ymax=",ymax,"\n")

    (ymax-ymin)/(xmax-xmin)
}
