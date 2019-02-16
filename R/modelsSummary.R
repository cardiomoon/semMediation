#' Make Model Coef Summary
#' @param fit A list of objects of class lm
#' @param labels optional list
#' @importFrom dplyr full_join
#' @importFrom purrr reduce
#' @export
#' @return A data.frame
#' @examples
#' fit1=lm(mpg~wt,data=mtcars)
#' fit2=lm(mpg~wt*hp,data=mtcars)
#' fit3=lm(mpg~wt*hp*am,data=mtcars)
#' labels=list(Y="mpg",X="wt",W="hp",Z="am")
#' modelsSummary(list(fit1),labels=labels)
#' modelsSummary(list(fit1,fit2),labels=labels)
#' modelsSummary(list(fit1,fit2))
#' modelsSummary(list(fit1,fit2,fit3),labels=labels)
modelsSummary=function(fit,labels=NULL){


    count=length(fit)

    df<-coef<-list()
    modelNames=c()
    for(i in 1 :count){

        df[[i]]=data.frame(summary(fit[[i]])$coef)
        colnames(df[[i]])=paste0(c("coef","se","t","p"),i)
        df[[i]][["name1"]]=rownames(df[[i]])
        colnames(df[[i]])[5]="name1"
        coef[[i]]=getInfo(fit[[i]])
        modelNames=c(modelNames,names(fit[[i]]$model)[1])
    }
    if(!is.null(labels)) modelNames=changeLabelName(modelNames,labels,add=TRUE)

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
    if(!is.null(labels)) finalNames=changeLabelName(finalNames,labels,add=TRUE)
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
    for(i in 1:count) cat(paste0(rep("-",colwidth),collapse = "")," ")
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


#' Print a string in center
#' @param string A string
#' @param width A numeric
#' @export
centerPrint=function(string,width){
    str_pad(string,width,side="both")
}



#' Make Model Coef Table
#' @param x object of class modelSummary
#' @importFrom officer fp_border
#' @importFrom flextable flextable merge_h_range align hline_top hline add_header
#' @importFrom flextable bold fontsize width italic set_header_labels add_header_row
#' @importFrom stats pf
#' @importFrom dplyr select
#' @importFrom tidyselect everything
#' @importFrom stats setNames
#' @export
#' @return A flextable
#' @examples
#' fit1=lm(mpg~wt,data=mtcars)
#' fit2=lm(mpg~wt*hp,data=mtcars)
#' fit3=lm(mpg~wt*hp*am,data=mtcars)
#' x=modelsSummary(list(fit1))
#' modelsSummaryTable(x)
#' x=modelsSummary(list(fit1,fit2))
#' modelsSummaryTable(x)
#' x=modelsSummary(list(fit1,fit2,fit3))
#' modelsSummaryTable(x)
modelsSummaryTable=function(x){
    modelNames=attr(x,"modelNames")
    modelNames

    result=x
    count=ncol(x)/4
    count
    result[["name1"]]=rownames(result)
    if(count>1){
        for(i in 2:count) result[[paste0("s",(i-1))]]=""
    }
    result<-result %>% select("name1",everything())
    rowcount=nrow(result)

    col_keys=c("name1",names(result)[2:5])
    if(count>1){
        for(i in 1:(count-1))
        col_keys=c(col_keys,paste0("s",i),names(result)[(i*4+2):(i*4+5)])
    }
    ft<-flextable(result,col_keys=col_keys)
    ft
    hlabel=c("Antecedent","Coef","SE","t","p")
    if(count>1){
        for(i in 2:count){
             hlabel=c(hlabel,"","Coef","SE","t","p")
        }
    }
    hlabel<-setNames(hlabel,col_keys)
    hlabel=as.list(hlabel)
    hlabel
    ft<-ft %>% set_header_labels(values=hlabel)
    for(i in 1:count){
        ft<-ft %>% merge_h_range(i=(rowcount-4):rowcount,j1=5*(i-1)+2,j2=5*(i-1)+5)
    }
    ft<- ft %>% align(align="center",part="all") %>%
         hline_top(part="header",border=fp_border(color="white",width=0))
    ft
    for(i in 1:count){
       ft <- ft %>% hline_top(j=((i-1)*5+2):((i-1)*5+5),part="header",border=fp_border(color="black",width=1))
    }
    big_border=fp_border(color="black",width=2)

    hlabel=c("",modelNames[1],rep("",3))
    if(count>1){
    for(i in 2:count){
        hlabel=c(hlabel,"",modelNames[i],rep("",3))
    }
    }
    hlabel<-setNames(hlabel,col_keys)
    hlabel=as.list(hlabel)
    hlabel
    length(hlabel)
    length(col_keys)
    ft <- add_header_row(ft,values=hlabel,top=TRUE,colwidths=rep(1,count*5))
    ft <- ft %>%
        hline_top(j=2:(count*5),part="header",border=fp_border(color="black",width=1))
    for(i in 1:count){
        ft<-ft %>% hline(i=1,j=((i-1)*5+2):((i-1)*5+5),part="header",border=fp_border(color="black",width=1))
    }
    for(i in 1:count){
        ft <- ft %>% merge_h_range (i=1,j1=(i-1)*5+2,j2=(i-1)*5+5,part="header")
    }
    ft
    hlabel=list(name1="",coef1="Consequent")
    ft<-ft %>% add_header_row(top=TRUE,values=hlabel,colwidths=c(1,count*5-1)) %>%
        hline_top(part="header",border=big_border) %>%
        hline(i=1,j=2:(count*5),part="header",border=fp_border(color="black",width=1))%>%
        merge_h_range (i=1,j1=2,j2=count*5,part="header") %>%
        align(align="center",part="header") %>%
        align(align="right",part="body") %>%
        bold(part="header") %>%
        fontsize(part="all",size=12) %>%
        hline(i=rowcount-5,border=fp_border(color="gray"),part="body")
    if(count>1){
        for(i in 1:(count-1)){
            ft<-ft %>% width(j=i*5+1,width=0.01)
        }
        for(i in 1:(count)){
            ft<-ft %>% italic(i=3,j=c(((i-1)*5+3):((i-1)*5+5)),italic=TRUE,part="header")
        }
    }
    ft

}

#'Format a numeric vector
#'@param x A numeric vector
#'@param digits integer indicating the number of decimal places
#'@export
myformat=function(x,digits=3){
    fmt=paste0("%.0",digits,"f")
    x=sprintf(fmt,x)
    x[x=="NA"]<-""
    x
}

#' Make p value format
#'@param x A numeric vector
#'@export
pformat=function(x){
    temp=substr(x,2,nchar(x))
    temp[temp==".000"]="<.001"
    temp
}

#'Get information of a model
#' @param fit object of class lm
#' @param digits integer indicating the number of decimal places
#' @export
#' @examples
#' fit=lm(mpg~wt,data=mtcars)
#' getInfo(fit)
getInfo=function(fit,digits=3){
    fmt=paste0("%.0",digits,"f")
    r1=nrow(fit$model)
    x<-summary(fit)
    r2=sprintf(fmt,x$r.squared)
    r3=sprintf(fmt,x$adj.r.squared)
    r4=paste0(sprintf(fmt,x$sigma)," ( df = ",round(x$df[2]),")")
    f=paste0("F(",round(x$fstatistic[2]),",",round(x$fstatistic[3]),") = ",
             sprintf(fmt,x$fstatistic[1]))
    p=sprintf("%0.4f",pf(x$fstatistic[1L],
                     x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE))
    p
    p=substr(p,2,digits+2)
    if(p==".000") p="< .001"
    else p=paste0("= ",p)
    f=paste0(f,", p ",p)
    result=c(r1,r2,r3,r4,f)
    result
}

