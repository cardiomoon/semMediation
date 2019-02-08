library(jtools)
library(ggplot2)
library(lavaan)
glbwarm=read.csv("./inst/pmacro/data/glbwarm.csv",stringsAsFactors = FALSE)
attach(glbwarm)




protest=read.csv("./inst/pmacro/data/protest.csv",stringsAsFactors = FALSE)
str(protest)
df=addCatVar(protest,"protest")
str(df)
covar=list(name="angry",site=list("liking"))
equation=catInteraction(Y="liking",W="sexism",count=3,covar=covar)
cat(equation)

library(lavaan)
library(diagram)
library(dplyr)
library(semMediation)
semfit=sem(model=equation,data=df)
res=estimatesTable(semfit,digits=3)
res
estimateTable=res
for(i in 2:length(levels(df$protest))){
   res$Predictors=str_replace(res$Predictors,paste0("d",i),paste0("protest=",levels(df$protest)[i]))
}
levels(df$protest)
names(df)

statisticalDiagram(no=1.1,estimateTable=res,whatLabel="est",labels=list("d2"="protest=2",d3="protest=3"))


pmacro=rbind(pmacro,pmacro[pmacro$no %in% c(15),])

tail(pmacro)
nrow(pmacro)
pmacro=rbind(pmacro,pmacro[,])
pmacro$no[46]=11
pmacro$no[47]=12
pmacro$no[48]=13
pmacro$no[49]=18
pmacro$no[50]=19
pmacro$no[51]=20

pmacro$modName[pmacro$no %in% c(14,15,18,19,20)]="W"

nodes[nodes$no %in% c(14,15,18,19,20),]
nodes$name[c(59,64)]="W"
nodes$name[c(60,65)]="MiW"
nodes$name[66]="XW"
arrows[arrows$no %in% c(14,15),]
arrows$start[c(68,73)]="W"
arrows$start[c(69,74)]="MiW"
arrows$start[c(75)]="XW"
nodes[nodes$no %in% c(7,8,11:13),]
pmacro[pmacro$no %in% c(7,8,11:13),]

nodes[nodes$no==3,]
nodes[nodes$no %in% 11:13,]
nodes=rbind(nodes,nodes[nodes$no %in% 19,])
nodes=rbind(nodes,nodes[nrow(nodes),])
arrows=rbind(arrows,arrows[nrow(arrows),])

nodes[nrow(nodes),]
arrows[arrows$no==3,]
arrows=rbind(arrows,arrows[arrows$no %in% 19,])
tail(arrows,10)
nrow(arrows)
arrows=arrows[-c(424:428),]
arrows<-editData::editData(arrows)

arrows[450:nrow(arrows),]
arrows$no[450:458]=18
arrows$no[459:473]=19
arrows$no[474:nrow(arrows)]=20

nodes<-editData::editData(nodes)
tail(nodes,10)

devtools::use_data(pmacro,overwrite=TRUE)
devtools::use_data(nodes,overwrite=TRUE)
devtools::use_data(arrows,overwrite=TRUE)

str(pmacro)
quantile(df$sexism,c(0.16,0.5,0.84),type=6)

fit=lm(liking~protest*I(sexism-4.250),data=df)
fit$coef

fit=lm(liking~protest*sexism,data=df)
require(ggiraphExtra)
ggPredict(fit,point=FALSE)+theme_bw()


summary(fit)

interact_plot(fit,pred=protest,modx=sexism)

interact_plot(fit,pred=sexism,modx=protest)


ss=sim_slopes(fit,pred=sexism,modx=protest,digits=3,confint=TRUE)
ss$slopes
library(tibble)
df=as_tibble(ss$slopes[,c(1,2,4,5)])
df[]=lapply(df,as.numeric)
names(df)=c("protest","est","low","high")
df=as.data.frame(df)
df[2:4]=round(df[2:4],3)
df
ggplot(data=df,aes(y=est,x=protest))+geom_point()+
    geom_pointrange(aes(ymin=low,ymax=high))+coord_flip()

plot(ss,digits=3)
johnson_neyman(fit,pred=sexism,modx=protest)
johnson_neyman(fit,pred=protest,modx=sexism)
probe_interaction(fit,pred=sexism,modx=protest)

df=protest
varname="protest"

require(moonBook)
acs
addCatVar(acs,"smoking")
df1=addCatVar(acs,"smoking")
df1

#' Add dummy vars to data.frame
#' @param df A data.frame
#' @param varname Variable name to be converted as factor and add dummies
#' @export
#' @examples
#' addCatVar(iris,"Species")
addCatVar=function(df,varname){
    if(!is.factor(df[[varname]])) {
        df[[varname]]<-factor(df[[varname]])
    }
    res=sort(as.numeric(unique(df[[varname]])))
    for(i in 2:length(res)){
        df[[paste0("d",i)]]=ifelse(as.numeric(df[[varname]])==i,1,0)
    }
    df
}


#'Make interaction equation with dummy categorical variable"
#'@param Y Name of dependent variable
#'@param X Optional.Name of independent variable
#'@param W Name of moderator variable
#'@param data A data.frame
#'@param count length of unique values of independent variable
#'@param prefix A prefix
#'@examples
#'cat(makeCatInteraction(Y="mpg",W="wt",count=3))
#'cat(makeCatInteraction(Y="mpg",X="cyl",W="wt",data=mtcars))
makeCatInteraction=function(Y="liking",X=NULL,W="sexism",data=NULL,
                            count=NULL,prefix="b"){
    if(is.null(count)) count=length(unique(data[[X]]))
    no=1
    res=c()
    for(i in 2:count){
      res=c(res,paste0(prefix,i-1,"*d",i))
      no=no+1
    }
    res=c(res,paste0(prefix,no,"*",W))

    for(i in 2:count){
        res=c(res,paste0(prefix,no+i-1,"*d",i,":",W))
    }
    temp=paste0(Y," ~ ",paste0(res,collapse="+"))
    temp=paste0(temp,"\n",W," ~ ",W,".mean*1")
    temp=paste0(temp,"\n",W," ~~ ",W,".var*",W)
    temp
}

protest$protest=protest$protest+1
protest$protest=factor(protest$protest)
sort(unique(protest$protest))
protest$d2=ifelse(protest$protest==2,1,0)
protest$d3=ifelse(protest$protest==3,1,0)

fit=lm(liking~protest*sexism,data=protest)
interact_plot(fit,pred=sexism,modx=protest)
sim_slopes(fit,pred=sexism,modx=protest)

fun0=
detach(protest)
equation="
liking ~ b1*d2+b2*d3+b3*sexism+b4*d2:sexism+ b5*d3:sexism
sexism ~ sexism.mean*1
sexism ~~ sexism.var*sexism
"
semfit=sem(equation,data=protest)
summary(semfit)
str(semfit)
res=parameterEstimates(semfit)
res
res=estimatesTable(semfit)

fun=list()
fun[[1]]=function(x) {7.706+res$est[3]*x}
fun[[2]]=function(x) {7.706+res$est[1]+(res$est[3]+res$est[4])*x}
fun[[3]]=function(x) {7.706+res$est[2]+(res$est[3]+res$est[5])*x}


draw1_1=function(res,wahtLabel="est"){
    res<-res[res$op=="~",]
    res
    count=(nrow(count)-1)/2


}

seq(from=1,by=0.2,length.out=5)
nodes=makeNodes(res)
nodes
arrows=makeArrows(res)
arrows
res



interact_plot(fit,pred=sexism,modx=protest)+
    stat_function(fun=fun[[1]])+
    stat_function(fun=fun[[2]])+
    stat_function(fun=fun[[3]])

sim_slopes(fit,pred=sexism,modx=protest,mod.values=c(4.250,5.120,5.896),digits=3)
predict(fit)
x1=rep(c(4.250,5.120,5.896),each=3)
x2=factor(rep(1:3,3))
new=data.frame(sexism=x1,protest=x2)
y=predict(fit,new)

new$y=y
library(ggplot2)
interact_plot(fit,pred=sexism,modx=protest)+
    annotate("point",x=new$sexism,y=new$y,size=3)

mean(protest$sexism)+c(-1,0,1)*sd(protest$sexism)

predict(fit,new)
Crabs <- read.table("http://www.da.ugent.be/datasets/crab.dat", header=T)
Crabs$y <- ifelse(Crabs$Sa > 0, 1, 0)
fit <- glm(y ~ W, data=Crabs, family=binomial)
summary(fit)

table.7.5 <- read.table("http://www.da.ugent.be/datasets/Agresti2002.Table.7.5.dat",
                 header=TRUE)

summary(table.7.5)
table.7.5$mental <- ordered(table.7.5$mental,
                            levels = c("well","mild","moderate","impaired"))

str(table.7.5)

set.seed(1234)
table.7.5[ sample(1:40, 10), ]

library(MASS)
fit.polr <- polr(mental~ses +life, data=table.7.5)
summary(fit.polr)

fit.polr <- polr(mental~ses +life, data=table.7.5, method="probit")
summary(fit.polr)

model <- ' mental ~ ses + life '
fit <- sem(model, data=table.7.5)
summary(fit)

model <- ' mental ~ ses + life + ses:life'
fit <- sem(model, data=table.7.5)
summary(fit)


fit.polr <- polr(mental~ses +life, data=table.7.5, method="probit")
summary(fit.polr)

table.7.5

glbwarm[["interaction0"]]=negemot*age*sex
saveRDS(glbwarm,"./inst/pmacro/interaction.RDS")
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


HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5",
                                 "x6","x7","x8","x9")]

HS9
HSbinary <- as.data.frame( lapply(HS9, cut, 2, labels=FALSE) )
HSbinary$school <- HolzingerSwineford1939$school
head(HSbinary)
str(HSbinary)


Y <- sample(1:4, size = 100, replace = TRUE)
head(Y, 20)

prop <- table(Y)/sum(table(Y))
prop
cprop <- c(0, cumsum(prop))
cprop
th <- qnorm(cprop)
th
library(MASS)
X1 <- rnorm(100); X2 <- rnorm(100); X3 <- rnorm(100)
fit <- polr(ordered(Y) ~ X1 + X2 + X3, method = "probit")
fit$zeta
x <- c(3,4,5)
class(x)
x <- factor(x)
class(x)
x <- ordered(x)
x
class(x)


library(lavaan)
varTable(HolzingerSwineford1939)

table.7.5 <- read.table("http://www.da.ugent.be/datasets/Agresti2002.Table.7.5.dat",
                        header=TRUE)

summary(table.7.5)
table.7.5$mental <- ordered(table.7.5$mental,
                            levels = c("well","mild","moderate","impaired"))

str(table.7.5)

set.seed(1234)
table.7.5[ sample(1:40, 10), ]

library(MASS)
str(table.7.5)
fit.polr <- polr(mental~ses +life, data=table.7.5)
summary(fit.polr)

fit.polr <- polr(mental~ses +life, data=table.7.5, method="probit")
summary(fit.polr)

model <- ' mental ~ c1*ses + c2*life + c3*ses:life
ses~ses;life~life
'
fit <- sem(model, data=table.7.5,ordered="mental")
summary(fit)

fit <- sem(model, data=table.7.5,group="mental")
summary(fit)

HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5","x6","x7","x8","x9")]
HSbinary <- as.data.frame( lapply(HS9, cut, 2, labels=FALSE) )
HSbinary$school <- HolzingerSwineford1939$school
head(HSbinary)
str(HSbinary)
model<-'visual =~ x1+x2+x3
            textual =~ x4 + x5 + x6
            speed =~ x7+x8+x9'

fit <- cfa(model, data=HSbinary, group="school", ordered=names(HSbinary),
group.equal=c("thresholds", "loadings"))
summary(fit, fit.measures=TRUE)

fit <- cfa(model, data=HSbinary, group="school", ordered=names(HSbinary),
           group.equal=c("thresholds", "loadings"),parameterization="theta")
summary(fit, fit.measures=TRUE)


teams=read.csv("./data/teams.csv",stringsAsFactors = FALSE)
names(teams)
fit1=lm(negtone~dysfunc,data=teams)
summary(fit1)
fit=lm(perform~dysfunc+negtone*negexp,data=teams)
summary(fit)
library(jtools)
interact_plot(fit,pred=negtone,modx=negexp)
