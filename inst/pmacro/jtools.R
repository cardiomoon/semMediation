disaster=read.csv("./inst/pmacro/data/disaster.csv",stringsAsFactors = FALSE)
disaster
fit=lm(justify~frame*skeptic,data=disaster)
interact_plot(fit,pred = skeptic,modx=frame)
interact_plot(fit,pred = skeptic,modx=frame,plot.points=TRUE)
johnson_neyman(fit, pred = skeptic, modx = frame)
result=johnson_neyman(fit, pred = frame, modx = skeptic,plot=FALSE)
str(result$bounds)
johnson_neyman(fit, pred = frame, modx = skeptic)$plot+
    annotate("text",x=result$bounds,y=-Inf,label=round(result$bounds,3),
             vjust=-0.5,hjust=-0.1)


quantile(disaster$skeptic)
disaster$skepticp=disaster$skeptic-1.592
fit1=lm(justify~frame*skepticp,data=disaster)
summary(fit1)


disaster$skepticp=disaster$skeptic-2.8
fit1=lm(justify~frame*skepticp,data=disaster)
summary(fit1)

disaster$skepticp=disaster$skeptic-5.2
fit1=lm(justify~frame*skepticp,data=disaster)
summary(fit1)
summary(disaster$skeptic)

mean(disaster$skeptic)+c(1,-1)*sd(disaster$skeptic)

condEffect=function(data=disaster,Y="justify",X="skeptic",M="frame",probs=c(0.16,0.5,0.84)){
    # data=disaster;Y="justify";X="skeptic";M="frame"
    # probs=c(0.16,0.5,0.84)
    temp=paste0(Y,"~",X,"*",M)
    fit=lm(as.formula(temp),data=data)
    summary(fit)
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
    effect
    p
    df=data.frame(x=modValues,y0=yhat1,y1=yhat2,effect=effect,p=p)
    df
    df$label1=paste0("theta [italic(X) %->% italic(Y)]")
    df$label2=paste0("list((italic(W)==",df$x,")==",
                    sprintf("%4.3f",df$effect),
                    ",italic(p)==",sprintf("%.03f",df$p),")")
    df$vjust=ifelse(df$effect>=0,0.2,-0.1)
    df
}
require(ggplot2)
interact_plot(fit,pred = skeptic,modx=frame)+
    annotate("segment",x=df$x,y=df$y0,xend=df$x,yend=df$y1,lty=2,color="red",
             arrow=arrow(angle=10,length=unit(0.15,"inches"),type="closed"))+
    annotate("text",x=df$x,y=df$y1+df$vjust,label=df$label1,parse=TRUE,hjust=1.1,vjust=1)+
    annotate("text",x=df$x,y=df$y1+df$vjust,label="|",vjust=1)+
    annotate("text",x=df$x,y=df$y1+df$vjust,label=df$label2,parse=TRUE,hjust=-0.01,vjust=1)

fit1=lm(justify~frame*I(skeptic-modValues[2]),data=disaster)
fit1$coef[2]
str(summary(fit1))
summary(fit1)$coef[2,4]

ggplot(aes(x=skeptic,y=justify,color=factor(frame)),data=disaster)+
    geom_point()+
    geom_smooth(method="lm",se=FALSE)+
    theme_bw()

ggplot(aes(x=skeptic,y=justify,color=frame),data=disaster)+
    geom_point()+
    geom_smooth(method="lm",se=FALSE)+
    theme_bw()

ggEffect(mtcars,aes(x=wt,y=mpg,color=hp))
ggEffect(disaster,aes(x=skeptic,y=justify,color=frame))
ggEffect(justify~skeptic*frame,data=disaster)

ggEffect(justify~frame*skeptic,data=disaster,probs=c(0.16,0.5,0.84))
ggEffect(justify~skeptic*frame,data=disaster)
fit=lm(justify~skeptic*frame,data=disaster)
summary(fit)
confint(fit)

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))


require(jtools)
require(ggstance)
states <- as.data.frame(state.x77)
fiti <- lm(Income ~ Illiteracy * Murder, data = states)
summ(fiti)
summ(fiti,center=TRUE)

interact_plot(fiti, pred = "Illiteracy", modx = "Murder")
fitiris <- lm(Petal.Length ~ Petal.Width * Species, data = iris)
interact_plot(fitiris, pred = "Petal.Width", modx = "Species")
interact_plot(fitiris, pred = "Petal.Width", modx = "Species",
              modx.values = c("versicolor", "virginica"))
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", plot.points = TRUE)
interact_plot(fitiris, pred = "Petal.Width", modx = "Species",
              plot.points = TRUE)
interact_plot(fitiris, pred = "Petal.Width", modx = "Species",
              plot.points = TRUE,  point.shape = TRUE)
interact_plot(fitiris, pred = "Petal.Width", modx = "Species",
              plot.points = TRUE, jitter=0.1, point.shape = TRUE)

fiti <- lm(Income ~ Illiteracy * Murder, data = states,
           weights = Population)
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", plot.points = TRUE)

interact_plot(fiti, pred = "Illiteracy", modx = "Murder", interval = TRUE,
              int.width = 0.8)

set.seed(99)
x <- rnorm(n = 200, mean = 3, sd = 1)
err <- rnorm(n = 200, mean = 0, sd = 4)
w <- rbinom(n = 200, size = 1, prob = 0.5)

y_1 <- 5 - 4*x - 9*w + 3*w*x + err

model_1 <- lm(y_1 ~ x * w)
summ(model_1)
interact_plot(model_1, pred = "x", modx = "w", linearity.check = TRUE,
              plot.points = TRUE)

x_2 <- runif(n = 200, min = -3, max = 3)
y_2 <- 2.5 - x_2^2 - 5*w + 2*w*(x_2^2) + err
data_2 <- as.data.frame(cbind(x_2, y_2, w))

model_2 <- lm(y_2 ~ x_2 * w, data = data_2)
summ(model_2)

interact_plot(model_2, pred = "x_2", modx = "w", linearity.check = TRUE,
              plot.points = TRUE)

model_3 <- lm(y_2 ~ poly(x_2, 2) * w, data = data_2)
summ(model_3)
interact_plot(model_3, pred = "x_2", modx = "w", data = data_2)

interact_plot(model_3, pred = "x_2", modx = "w", data = data_2,
              linearity.check = TRUE, plot.points = TRUE)

interact_plot(fiti, pred = "Illiteracy", modx = "Murder",
              x.label = "Custom X Label", y.label = "Custom Y Label",
              main.title = "Sample Plot",  legend.main = "Custom Legend Title",
              color.class = "Oranges")

interact_plot(fitiris, pred = "Petal.Width", modx = "Species") + theme_apa()

sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = FALSE)

johnson_neyman(fiti, pred = Illiteracy, modx = Murder, alpha = 0.01)

sim_slopes(fiti, pred = Illiteracy, modx = Murder, modx.values = c(0, 5, 10),
           johnson_neyman = FALSE)

ss <- sim_slopes(fiti, pred = Illiteracy, modx = Murder, modx.values = c(0, 5, 10))
plot(ss)

ss <- sim_slopes(fiti, pred = Illiteracy, modx = Murder, modx.values = c(0, 5, 10))
library(huxtable)
as_huxtable(ss)

sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = TRUE)
johnson_neyman(fiti, pred = Illiteracy, modx = Murder)
