#' make same rank
#' @param equation semequation to add
#' @param temp character vector
#' @param addOrder logical. Whether or not add order
#' @importFrom stringr str_flatten
#' @importFrom magrittr %>%
#' @export
makeSameRank=function(equation,temp,addOrder=TRUE){
    if(length(temp)>0){
        equation<-equation %>% paste0("{\n")
        if(addOrder) equation<-equation %>% paste0(str_flatten(temp,"->"),"[style=invis]\n")
        equation<-equation %>%
            paste0("\nrank=same\n",str_flatten(temp,","),"\n}\n")
    }
    equation
}

#' Select characters contains even numbers
#' @param x a numeric or character vector
#' @importFrom stringr str_extract_all
#' @export
selectEven=function(x){
    if("character" %in% class(x)){
        temp=as.numeric(unlist(lapply(stringr::str_extract_all(x,"[0-9.]"),str_flatten)))
    } else if("factor" %in% class(x)){
        temp=as.numeric(unlist(lapply(stringr::str_extract_all(as.character(x),"[0-9.]"),str_flatten)))

    } else{
        temp=x
    }
    x[temp%%2==0]
}

#' Select characters contains odd numbers
#' @param x a numeric or character vector
#' @export
selectOdd=function(x){
    if("character" %in% class(x)){
        temp=as.numeric(unlist(lapply(stringr::str_extract_all(x,"[0-9.]"),str_flatten)))
    } else if("factor" %in% class(x)){
        temp=as.numeric(unlist(lapply(stringr::str_extract_all(as.character(x),"[0-9.]"),str_flatten)))

    } else{
        temp=x
    }
    temp
    x[temp%%2==1]
}

#' Extract Numbers in character vectors
#' @param x a numeric or character vector
#' @export
extractNumber=function(x){
    class(x)
    if("character" %in% class(x)){
        temp=as.numeric(unlist(lapply(stringr::str_extract_all(x,"[0-9.]"),str_flatten)))
    } else if("factor" %in% class(x)){
        temp=as.numeric(unlist(lapply(stringr::str_extract_all(as.character(x),"[0-9.]"),str_flatten)))

    } else{
        temp=x
    }
    temp
}

#' add x1 position
#' @param df a data.frame
#' @export
addPos2=function(df){
    df$x1=floor(df$x)
    df
    HGroups=unique(df$group1[stringr::str_detect(df$group1,"H")])
    MGroups=unique(df$group1[stringr::str_detect(df$group1,"M")])
    HMGroups=c(HGroups,MGroups)
    upper<-lower<-0
    for(i in seq_along(HGroups)){
        # cat("i=",i,",upper=",upper,",lower=",lower,"\n")
        temp=df$text[df$group1 == HGroups[i]]
        add=1:length(temp)
        mpos=ceiling(length(temp)/2)+1
        mpos
        if(length(temp)%%2==0){
               add[(length(temp)/2+1):length(temp)]=add[(length(temp)/2+1):length(temp)]+1
        }
        df$x1[df$group1== HGroups[i]]=add+ifelse(upper<=lower,upper,lower)
        df$x1[df$group1== paste0("M",i)]=mpos+ifelse(upper<=lower,upper,lower)
        df$y[df$group1== paste0("M",i)]=ifelse(upper<=lower,1,2)
        if(upper<=lower) {
               upper=upper+add[length(add)]
               df$y[df$group1== HGroups[i]]=1
        } else{
               lower=lower+add[length(add)]
               df$y[df$group1== HGroups[i]]=2
        }
    }
    df
    diff=upper-lower
    diff
    if(diff>=2){
        if(upper>lower){
            df$x1[df$group1 %in% HMGroups & df$y==2]=df$x1[df$group1 %in% HMGroups & df$y==2]+diff%/%2
        }
    }
    diff1=max(upper,lower)-max(df$x)
    if(diff1>0) df$x1[df$x>=(max(df$x)-1)]=df$x1[df$x>=(max(df$x)-1)]+diff1+1
    if((length(unique(df$x1))==2) &(max(df$x1)==1)){
        df$x1[df$x1==1]=4
    }
    df$y[df$group1=="X"]=1:length(df$y[df$group1=="X"])
    df$y[df$group1=="Y"]=1:length(df$y[df$group1=="Y"])
    df
}


#' Make sem diagram
#'@param fit A data.frame. Result of parameterEstimates function of package lavaan
#'@param ... Further arguments tobe passed to makeDiagram()
#'@importFrom DiagrammeR grViz
#'@export
semDiagram=function(fit,...){
    grViz(makeDiagram(fit,...))
}


#' Adds variable labels to the Diagrammer plot function call.
#'
#' @param label_list A named list of variable labels.
buildLabels <- function(label_list){

    names(label_list) <- stringr::str_replace_all(names(label_list), pattern = "\\.", replacement = "")
    labs <- paste(names(label_list), " [label = ", "'", label_list, "'", "]", sep = "")
    paste(labs, collapse = "\n")
}


#'Get list to string
#'@param options named list
#'@export
getOptions=function(options){
    paste(paste(names(options), options, sep = " = "), collapse = ", ")
}

#'remove period
#'@param x A character vector
#'@export
removePeriod=function(x){
    res=stringr::str_replace_all(x, pattern = "\\.", replacement = "")
    res=stringr::str_replace_all(res, pattern = ":", replacement = "")
    res
}


#' make list of remove period
#'@param x A character vector
#' @export
removePeriodLabels=function(x){
    location=stringr::str_detect(x, pattern = "[\\.:]")
    res=stringr::str_replace_all(x, pattern = "[\\.:]", replacement = "")
    label=x[location]
    find=res[location]
    list(find=find,label=label)
}

#' Make diagram euation
#'@param fit A data.frame. Result of parameterEstimates function of package lavaan
#'@param labels An optional named list of variable labels fit object of class lavaan
#'@param graphOptions A named list of graph options for Diagrammer syntax
#'@param nodeOptions A named list of node options for Diagrammer syntax
#'@param edgeOptions A named list of edge options for Diagrammer syntax.
#'@param whatLabels What should the edge labels indicate in the path diagram? Choices are c("est","std","name").
#'@param mediationOnly Whether or not draw mediation effect only. Default value is FALSE.
#'@param residuals Logical, should residuals (and variances) be included in the path diagram?
#'@param regression Whether or not draw regression. Default value is TRUE.
#'@param indirect Whether or not draw indirect effects. Default value is FALSE.
#'@param secondIndirect Whether or not draw 2nd indirect effects. Default value is FALSE.
#'@param total Whether or not draw total effect. Default value is FALSE.
#'@importFrom stringr str_flatten str_detect
#'@export
makeDiagram=function(fit,
                     labels=NULL,
                     graphOptions = list(rankdir="LR", ranksep="0.75", nodesep="0.25",
                                          overlap = "true", fontsize = "10"),
                     nodeOptions = list(),
                     edgeOptions = list(color = "black"),
                     whatLabels="std",mediationOnly=FALSE,
                     regression=TRUE,indirect=FALSE,secondIndirect=FALSE,total=FALSE,
                     residuals=FALSE){
    # whatLabels="std";mediationOnly=FALSE;residuals=FALSE;nodesep=NULL;ranksep=NULL
    # regression=TRUE;indirect=FALSE;secondIndirect=FALSE;total=FALSE
    # residuals=FALSE
    # graphOptions = list(rankdir="LR", ranksep="0.75", nodesep="0.25",
    #                     overlap = "true", fontsize = "10")
    # nodeOptions = list()
    # edgeOptions = list(color = "black")

    df=fit2df(fit)
    df<-df[df$text!="",]
    df

    df=addpos(df)
    labels2=removePeriodLabels(df$text)
    df$text=removePeriod(df$text)
    df
    df1=addPos2(df)

    df1


    HGroups=unique(df$group1[stringr::str_detect(df$group1,"H")])
    if(mediationOnly) {

        delVars=df1$text[df1$group1 %in% c("0","5",HGroups)]
        delVars
        df1=df1[!(df1$text %in% delVars),]
    }

    H1vars=df1$text[df1$group1=="H1"]
    H1vars
    width<-max(df1$x1)+1
    width
    equation=paste("digraph {\ngraph [",getOptions(graphOptions),"]\n")

    temp=paste0("t",0:(width-1))
    temp
    equation<-equation %>%
        paste0("
               subgraph cluster0{
               style=invis;
               node [style=invis]\n") %>%
        paste0(str_flatten(temp,";"),"\n") %>%
        paste0("edge[style=invis];\n",
               str_flatten(temp,"->"),"\n}\n")


    temp=df1$text[(df1$group1 %in% HGroups) & df1$y==1]
    temp1=df1$text[(df1$group1 %in% HGroups) & df1$y==1 & df1$latent==FALSE]
    temp2=df1$text[(df1$group1 %in% HGroups) & df1$y==1 & df1$latent==TRUE]
    if(length(temp)>0){
        equation<-equation %>%
            paste0("subgraph {\n")

        if(length(temp1)>0){
            equation<-equation %>%
                paste0("node [shape=box ",getOptions(nodeOptions)," ]\n",str_flatten(temp1,";"),"\n")
        }
        if(length(temp2)>0){
            equation<-equation %>%
                paste0("node [shape=oval ",getOptions(nodeOptions),"]\n",str_flatten(temp2,";"),"\n")
        }
        equation<-equation %>%
            paste0("edge[style=invis];\n",
                   str_flatten(temp,"->"),"\n}\n")
    }
    temp=df1$text[(df1$group1 %in% HGroups) & df1$y==2]
    temp1=df1$text[(df1$group1 %in% HGroups) & df1$y==2 & df1$latent==FALSE]
    temp2=df1$text[(df1$group1 %in% HGroups) & df1$y==2 & df1$latent==TRUE]
    if(length(temp)>0){
        equation<-equation %>%
            paste0("subgraph {\n")

        if(length(temp1)>0){
            equation<-equation %>%
                paste0("node [shape=box ",getOptions(nodeOptions),"]\n",str_flatten(temp1,";"),"\n")
        }
        if(length(temp2)>0){
            equation<-equation %>%
                paste0("node [shape=oval ",getOptions(nodeOptions),"]\n",str_flatten(temp2,";"),"\n")
        }
        equation<-equation %>%
            paste0("edge[style=invis];\n",
                   str_flatten(temp,"->"),"\n}\n")
    }
    temp=df1$text[df1$latent==FALSE & !(df1$group1 %in% HGroups)]
    temp
    if(length(temp)>0){
        equation<-equation %>%
            paste0("node [shape=box ",getOptions(nodeOptions),"]\n") %>%
            paste0(str_flatten(temp,";"),"\n")
    }
    temp=df1$text[df1$latent==TRUE & !(df1$group1 %in% HGroups)]
    temp
    if(length(temp)>0){
        equation<-equation %>%
            paste0("node [shape=oval ",getOptions(nodeOptions),"]\n") %>%
            paste0(str_flatten(temp,";"),"\n")
    }

    (oddHGroups=unique(df1$group1[df1$group1 %in% HGroups & df1$y==1]))
    (evenHGroups=unique(df1$group1[df1$group1 %in% HGroups & df1$y==2]))
    for(i in 0:(width-1)){
        df2=df1[df1$x1==i,]
        df2

        addOrder=ifelse("M2" %in% df2$group1,FALSE,TRUE)
        temp=c(df2$text[df2$group1 %in% oddHGroups],
               df2$text[!(df2$group1 %in% HGroups)],
               df2$text[df2$group1 %in% evenHGroups])
        temp=c(paste0("t",i),temp)
        equation<-equation %>% makeSameRank(temp,addOrder=addOrder)
    }
    equation<-equation %>% paste0("\n edge [",getOptions(edgeOptions),"]\n")
    res=parameterEstimates(fit,standardized=TRUE)
    res1=res[res$op!=":=",]
    res1=res1[res1$op!="~1",]
    labels3=removePeriodLabels(res1$lhs)
    res1$lhs=removePeriod(res1$lhs)
    labels4=removePeriodLabels(res1$rhs)
    res1$rhs=removePeriod(res1$rhs)
    if(residuals==FALSE) res1=res1[res1$lhs!=res1$rhs,]
    if(regression==FALSE) res1=res1[res1$op!="~",]
    res1
    if(mediationOnly) {
        res1=res1[!(res1$lhs %in% delVars),]
        res1=res1[!(res1$rhs %in% delVars),]
    }

    if(nrow(res1)>0){
        for(i in 1:nrow(res1)){

             if(res1$op[i]=="=~") {
                 if(res1$rhs[i] %in% H1vars) {
                     temp=paste0(res1$rhs[i],"->",res1$lhs[i],"[dir=back ")
                 } else{
                    temp=paste0(res1$lhs[i],"->",res1$rhs[i],"[")
                 }

             } else {

                 if(res1$op[i]=="~~") {
                     if(df1$group1[df1$text==res1$rhs[i]]==df1$group1[df1$text==res1$lhs[i]]){

                         if(df1$x1[df1$text==res1$rhs[i]]==min(df1$x1)){
                            temp=paste0(res1$rhs[i],":w ->",res1$lhs[i],":w [")
                         } else if(df1$x1[df1$text==res1$rhs[i]]==max(df1$x1)){
                                 temp=paste0(res1$rhs[i],":e ->",res1$lhs[i],":e [")
                         } else{
                             temp=paste0(res1$rhs[i],"->",res1$lhs[i],"[")
                         }

                     } else{
                         temp=paste0(res1$rhs[i],"->",res1$lhs[i],"[")
                     }
                     temp=paste0(temp,"dir=both constraint=false ")
                 } else{
                     temp=paste0(res1$rhs[i],"->",res1$lhs[i],"[")
                 }
             }
             equation <- equation %>% paste0(temp)
             if(is.na(res1$pvalue[i])) equation <- equation %>% paste0("style=dashed ")
             else if(res1$pvalue[i] >=0.05) equation <- equation %>% paste0("style=dashed ")
             temp=""
             if(whatLabels=="std") temp=sprintf("%.02f",res1$std.all[i])
             else if(whatLabels=="est") temp=sprintf("%.02f",res1$est[i])
             else if(whatLabels=="name") temp=res1$label[i]
             if(!is.null(temp)) {
                 if(temp!="") equation <- equation %>% paste0("label=",temp)
             }
             equation <- equation %>% paste0("]\n")
        }
    }

    labels_string=""
    if(!is.null(labels)){
        labels
        labels_string = buildLabels(labels)
        labels_string
        equation <- paste(equation, labels_string)
    }

    find=c(labels2$find,labels3$find,labels4$find)
    label=c(labels2$label,labels3$label,labels4$label)

    find
    label
    for(i in seq_along(find)){
        if(!str_detect(labels_string,find[i])){
           labs <- paste(find[i], " [label = ", "'", label[i], "'", "]\n", sep = "")
           equation <- paste(equation, labs)
        }
    }
    equation=paste0(equation,"\n}")
    equation
}

