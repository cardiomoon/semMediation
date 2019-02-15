require(lavaan)
require(semMediation)
model="knowledge =~ general+symptoms+treatmt
empathy =~ cognitiv+emotion+disposit+attitude
intervention =~ classrm+instruct
intervention ~ b*empathy + c*knowledge
empathy ~ a*knowledge
indirect effect:=a*b
total effect:=c+(a*b)"
fit=sem(model=model,data=ADHD)
summary(fit)

mediationPlot(fit,whatLabels="est")
resTable=estimatesTable(fit)
resTable
estimateTable=resTable
parameterEstimates(fit)

statisticalDiagram(no=4)
labels=list(X="knowledge",Mi="empathy",Y="intervention")
statisticalDiagram(no=4,labels=labels,estimateTable = resTable,whatLabel="est")



names(labels)[str_detect(labels,"empathy")]


disaster=read.csv("./inst/pmacro/data/disaster.csv",stringsAsFactors = FALSE)
disaster
fit=lm(justify ~  frame+frame*skeptic,data=disaster)
summary(fit)
interact_plot(fit,pred=frame,modx=skeptic,plot.points=FALSE,interval=FALSE,int.type='confidence',int.width=0.95,linearity.check=FALSE)
temp="interact_plot(fit,pred=skeptic,modx=frame,plot.points=FALSE,interval=FALSE,int.type='confidence',int.width=0.95,linearity.check=FALSE)"
p=eval(parse(text=temp))
p


model1="justify ~ a1*frame+a2*skeptic+a3*frame:skeptic
donate ~ b1*justify+c1*frame+c2*skeptic+c3*frame:skeptic
skeptic ~ skeptic.mean*1
skeptic ~~ skeptic.var*skeptic
indirect :=(a1+a3*skeptic.mean)*(b1)
direct :=c1+c3*skeptic.mean
total := direct + indirect
indirect.below :=(a1+a3*(skeptic.mean-sqrt(skeptic.var)))*(b1)
indirect.above :=(a1+a3*(skeptic.mean+sqrt(skeptic.var)))*(b1)
direct.below:=c1+c3*(skeptic.mean-sqrt(skeptic.var))
direct.above:=c1+c3*(skeptic.mean+sqrt(skeptic.var))
total.below := direct.below + indirect.below
total.above := direct.above + indirect.above
prop.mediated.below := indirect.below / total.below
prop.mediated.above := indirect.above / total.above
"
model2="justify ~ a1*frame+a2*skeptic+a3*frame:skeptic
donate ~ b1*justify+c1*frame+c2*skeptic+c3*frame:skeptic
skeptic ~ skeptic.mean*1
skeptic ~~ skeptic.var*skeptic
indirect :=(a1+a3*2.8)*(b1)
direct :=c1+c3*2.8
total := direct + indirect
indirect.below :=(a1+a3*1.592)*(b1)
indirect.above :=(a1+a3*5.2)*(b1)
direct.below:=c1+c3*1.592
direct.above:=c1+c3*5.2
total.below := direct.below + indirect.below
total.above := direct.above + indirect.above
prop.mediated.below := indirect.below / total.below
prop.mediated.above := indirect.above / total.above
"
library(lavaan)
fit=sem(model=model1,data=disaster)
parameterEstimates(fit)
mean(disaster$skeptic)

fit2=sem(model=model2,data=disaster)
parameterEstimates(fit2)

require(ggplot2)
fun1=function(x) (0.160+0.015*x)
fun2=function(x) (0.519-0.186*x)
fun3=function(x) (0.679-0.171*x)
ggplot(data=disaster,aes(x=skeptic,y=donate))+
    stat_function(fun=fun1)+
    stat_function(fun=fun2,lty=3)+
    stat_function(fun=fun3,color="red")

mod="skeptic"
values=NULL
modmedSummary=function(fit,mod="skeptic",values=NULL){
    res=parameterEstimates(fit)
    res=res[res$label!="",]
    res
    if(is.null(values)){
      values=res$est[res$label==paste0(mod,".mean")]+c(0,-1,1)*res$est[res$label==paste0(mod,".var")]
      values
    }
    select=c("indirect","indirect.below","indirect.above")
    indirect=res$est[which(res$lhs %in% select)]
    lower=res$ci.lower[which(res$lhs %in% select)]
    upper=res$ci.upper[which(res$lhs %in% select)]
    indirectp=res$pvalue[which(res$lhs %in% select)]
    select=c("direct","direct.below","direct.above")
    direct=res$est[which(res$lhs %in% select)]
    se=res$se[which(res$lhs %in% select)]
    directp=res$p[which(res$lhs %in% select)]
    df=data.frame(values,indirect,lower,upper,indirectp,direct,se,directp)
    df=df[c(2,1,3),]
    df[]=round(df,3)
    class(df)=c("modmedSummary","data.frame")
    df
}
x=modmedSummary(fit,mod="skeptic")
class(x)
values=quantile(disaster$skeptic,probs=c(0.5,0.16,0.84),type=6)
modmedSummary(fit2,mod="skeptic",values=values)
x

#' Print a string in center
#' @param string A string
#' @param width A numeric
#' @export
rightPrint=function(string,width){
    str_pad(string,width,side="left")
}

print.modmedSummary=function(x){
    count=nrow(x)
    x[]=lapply(x,myformat)
    x[[5]]=pformat(x[[5]])
    x[[8]]=pformat(x[[8]])
    total=71

    cat("\nInference for the Conditional Direct and Indirect Effects","\n")
    cat(paste(rep("=",total),collapse = ""),"\n")
    cat(centerPrint("",8),centerPrint("Indirect Effect",35),centerPrint("Direct Effect",26),"\n")
    cat(centerPrint("",8),paste(rep("-",35),collapse = ""),paste(rep("-",26),collapse = ""),"\n")
    cat(centerPrint("W",8),centerPrint("estimate",8),centerPrint("95% CI",18),centerPrint("p",8))
    cat(centerPrint("estimate",10),centerPrint("SE",8),centerPrint("p",8),"\n")
    cat(paste(rep("-",total),collapse = ""),"\n")
    for(i in 1:count){
        cat(rightPrint(x[i,1],8))
        cat(rightPrint(x[i,2],8))
        cat(paste0(rightPrint(x[i,3],8)," to ",rightPrint(x[i,4],6)))
        cat(rightPrint(x[i,5],8))
        cat(rightPrint(x[i,6],13))
        cat(rightPrint(x[i,7],8))
        cat(rightPrint(x[i,8],8),"\n")
    }
    cat(paste(rep("=",total),collapse = ""),"\n")
}
x
