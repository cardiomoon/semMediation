require(rlang)
require(tidyverse)


modelsSummary2=function(...){

    res=ensyms(...)
    count=length(res)

    fit<-df<-coef<-list()
    modelNames=c()
    for(i in 1 :count){
        fit[[i]]=eval(res[[i]])
        df[[i]]=data.frame(summary(fit[[i]])$coef)
        colnames(df[[i]])=paste0(c("coef","se","t","p"),i)
        df[[i]][["name1"]]=rownames(df[[i]])
        colnames(df[[i]])[5]="name1"
        coef[[i]]=getInfo(fit[[i]])
        modelNames=c(modelNames,names(fit1$model)[1])
    }

    if(count==1){
        mydf=df[[1]]
    } else{
    mydf<-reduce(df,full_join,by="name1")
    }
    mydf
    mydf<-mydf %>% select("name1",everything())
    rownames(mydf)=mydf[["name1"]]
    mydf<-mydf[-1]

    mydf[]=lapply(mydf,myformat)
    mydf<-mydf[c(2:nrow(mydf),1),]
    rownames(mydf)[nrow(mydf)]="Constant"
    mydf
    for(i in 1:count){
        mydf[[4*i]]=pformat(mydf[[4*i]])
    }
    finalNames=rownames(mydf)
    df2=data.frame(coef,stringsAsFactors = FALSE)
    colnames(df2)=paste0("coef",1:ncol(df2))
    finalNames=c(finalNames,c("Observations","R2","Adjusted R2","Residual SE","F statistic"))

    res=full_join(mydf,df2,by=paste0("coef",1:count))
    res[is.na(res)]=""
    res
    rownames(res)=finalNames
    res
    class(res)=c("modelSummary","data.frame")
    attr(res,"modelNames")=modelNames
    res
}


#'S3 method print for object modelSummary
#'@param x Object of class modelSummary
#'@param ... additional arguments to pass to print.modelSummary
#'@importFrom stringr str_pad
#'@export
print.modelSummary=function(x,...){
    count=ncol(x)/4
    colwidth=32
    left=20
    total=left+colwidth*count+1
    right=colwidth*count+1

    cat(paste(rep("=",total),collapse = ""),"\n")
    cat(paste0(centerPrint("",left),centerPrint("Consequent",right)),"\n")
    cat(paste0(centerPrint("",left),paste(rep("-",right),collapse = "")),"\n")
    names=attr(x,"modelNames")
    cat(paste0(centerPrint("",left)))
    for(i in 1:count){cat(centerPrint(names[i],colwidth))}
    cat("\n")
    cat(paste0(centerPrint("",left)))
    for(i in 1:count) cat(paste(rep("-",colwidth),collapse = "")," ")
    cat("\n")
    cat(paste0(centerPrint("Antecedent",left)))
    for(i in 1:count) cat(paste0(centerPrint("Coef",8),centerPrint("SE",8),centerPrint("t",8),centerPrint("p",8)," "))
    cat("\n")
    cat(paste(rep("-",total),collapse = ""),"\n")
    for(i in 1:(nrow(x)-5)){
        cat(centerPrint(rownames(x)[i],left))
        for(j in 1:count){
            for(k in 1:4){
                cat(str_pad(x[i,(j-1)*4+k],6,"left")," ")
                cat(ifelse((j==count)&(k==4),"\n",""))
            }
        }
    }
    cat(paste(rep("-",total),collapse = ""),"\n")
    for(i in (nrow(x)-4):nrow(x)){
        cat(centerPrint(rownames(x)[i],left))
        for(j in 1:count){
            cat(centerPrint(x[i,(j-1)*4+1],colwidth))
            cat(ifelse(j==count,"\n",""))
        }

    }
    cat(paste(rep("=",total),collapse = ""),"\n")
}

centerPrint=function(string,width){
    str_pad(string,width,side="both")
}

fit1=lm(mpg~wt,data=mtcars)
fit2=lm(mpg~wt*hp,data=mtcars)
fit3=lm(mpg~wt*hp*am,data=mtcars)
x=modelsSummary2(fit1)
x
x=modelsSummary2(fit1,fit2)
print(x)
x=modelsSummary2(fit1,fit2,fit3)
print(x)
