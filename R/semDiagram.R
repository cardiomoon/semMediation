#' make same rank
#' @param equation semequation to add
#' @param temp character vector
#' @importFrom stringr str_flatten
#' @importFrom magrittr %>%
#' @export
makeSameRank=function(equation,temp){
    if(length(temp)>0){
        equation<-equation %>%
            paste0("{\nrank=same\n",
                   str_flatten(temp,","),"\n}\n")
    }
    equation
}

#' add x1 position
#' @param df a data.frame
#' @export
addPos2=function(df){
    df$x1=df$x
    width<-width1<-width2<-length(unique(df$x))
    width
    evenno=0
    temp=df$text[df$group1=="H1"]
    temp
    if(length(temp)>2){
        width1=width+length(temp)-3
        width1
        length(temp)%%2
        if(length(temp)%%2==0){
            width1=width1+1
            evenno=1
        }
    }
    width1
    temp=df$text[df$group1=="H2"]
    if(length(temp)>2){
        width2=width+length(temp)-3
        if(length(temp)%%2==0){
            width2=width2+1
            evenno=1
        }
    }
    width2
    width=max(width1,width2)
    width
    max(df$x1)
    diff=width-max(df$x1)-1

    df[df$x1>2,]
    evenno
    if(diff>=0) df$x1[df$x1>2]=df$x1[df$x1>2]+diff
    df
    diff+evenno

    width

    if(diff>=0) df$x1[df$group1 %in% c("M1","M2")]=width%/%2
    temp=df$text[df$group1=="H1"]
    if(length(temp)>2){
        if(length(temp)%%2==0){
            add=0:(length(temp)-1)
            add[(length(temp)/2+1):length(temp)]=add[(length(temp)/2+1):length(temp)]+1
            add
            df$x1[df$group1=="H1"]=1+add
        } else {
            df$x1[df$group1=="H1"]=1:(1+length(temp)-1)
        }
    } else if(length(temp)==2){
        df$x1[df$group1=="H1"]=c(1,width-2)
    }
    df
    temp=df$text[df$group1=="H2"]
    if(length(temp)>2){

        if(length(temp)%%2==0){

            add=0:(length(temp)-1)
            add[(length(temp)/2+1):length(temp)]=add[(length(temp)/2+1):length(temp)]+1
            df$x1[df$group1=="H2"]=1+add
        } else{
            df$x1[df$group1=="H2"]=1:(1+length(temp)-1)
        }
    } else if(length(temp)==2){
        df$x1[df$group1=="H2"]=c(1,width-2)
    }
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



#' Make diagram euation
#'@param fit A data.frame. Result of parameterEstimates function of package lavaan
#'@param whatLabels What should the edge labels indicate in the path diagram? Choices are c("est","std","name").
#'@param mediationOnly Whether or not draw mediation effect only. Default value is FALSE.
#'@param residuals Logical, should residuals (and variances) be included in the path diagram?
#'@importFrom stringr str_flatten
#'@export
makeDiagram=function(fit,whatLabels="std",mediationOnly=FALSE,residuals=FALSE){
    df=fit2df(fit)
    df=addpos(df)
    df
    df1=addPos2(df)
    df1
    if(mediationOnly) {
        delVars=df1$text[df1$group1 %in% c("0","H1","H2","5")]
        delVars
        df1=df1[!(df1$text %in% delVars),]
    }

    H1vars=df1$text[df1$group1=="H1"]
    H1vars
    width<-max(df1$x1)+1
    width
    equation="digraph {\ngraph [rankdir = LR]\n"
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


    temp=df1$text[df1$group1=='H1']
    temp1=df1$text[df1$group1=='H1' & df1$latent==FALSE]
    temp2=df1$text[df1$group1=='H1' & df1$latent==TRUE]
    if(length(temp)>0){
        equation<-equation %>%
            paste0("subgraph {\n")

        if(length(temp1)>0){
            equation<-equation %>%
                paste0("node [shape=box]\n",str_flatten(temp1,";"),"\n")
        }
        if(length(temp2)>0){
            equation<-equation %>%
                paste0("node [shape=oval]\n",str_flatten(temp2,";"),"\n")
        }
        equation<-equation %>%
            paste0("edge[style=invis];\n",
                   str_flatten(temp,"->"),"\n}\n")
    }
    temp=df1$text[df1$group1=='H2']
    temp1=df1$text[df1$group1=='H2' & df1$latent==FALSE]
    temp2=df1$text[df1$group1=='H2' & df1$latent==TRUE]
    if(length(temp)>0){
        equation<-equation %>%
            paste0("subgraph {\n")

        if(length(temp1)>0){
            equation<-equation %>%
                paste0("node [shape=box]\n",str_flatten(temp1,";"),"\n")
        }
        if(length(temp2)>0){
            equation<-equation %>%
                paste0("node [shape=oval]\n",str_flatten(temp2,";"),"\n")
        }
        equation<-equation %>%
            paste0("edge[style=invis];\n",
                   str_flatten(temp,"->"),"\n}\n")
    }
    temp=df1$text[df1$latent==FALSE & !(df1$group1 %in% c("H1","H2"))]
    temp
    if(length(temp)>0){
        equation<-equation %>%
            paste0("node [shape=box]\n") %>%
            paste0(str_flatten(temp,";"),"\n")
    }
    temp=df1$text[df1$latent==TRUE & !(df1$group1 %in% c("H1","H2"))]
    temp
    if(length(temp)>0){
        equation<-equation %>%
            paste0("node [shape=oval]\n") %>%
            paste0(str_flatten(temp,";"),"\n")
    }


    for(i in 0:(width-1)){
        df2=df1[df1$x1==i,]
        temp=c(df2$text[df2$group1=="H1"],df2$text[!(df2$group1 %in% c("H1","H2"))],df2$text[df2$group1=="H2"])
        temp=c(paste0("t",i),temp)
        equation<-equation %>% makeSameRank(temp)
    }

    # temp=unique(df1$group2)
    # temp=setdiff(temp,"")
    # temp
    # df1
    # if(length(temp>0)){
    #     for(i in 1:length(temp)){
    #         tempvar=df1$text[df1$group2==temp[i]]
    #         tempvar
    #         df1$group[df1$text==temp[i]]
    #         if(df1$group[df1$text==temp[i]]=="Y"){
    #             equation <- equation %>%
    #                 paste0(temp[i],"->{",str_flatten(tempvar,","),"};\n")
    #         } else if(df1$group[df1$text==temp[i]] %in% c("M1")){
    #             equation <- equation %>%
    #                 paste0("{",str_flatten(tempvar,","),"}->",temp[i],"[dir=back constraint=false];\n")
    #         } else {
    #             equation <- equation %>%
    #                 paste0("{",str_flatten(tempvar,","),"}->",temp[i],"[dir=back];\n")
    #         }
    #     }
    # }
    # M=df1$text[substr(df1$group1,1,1)=="M"]
    # Y=df1$text[df1$group1=="Y"]
    # MY=c(M,Y)
    # X=df1$text[df1$group1=="X"]
    # if(length(MY)>0){
    #     equation <- equation %>%
    #         paste0("{",str_flatten(X,","),"}->{",str_flatten(MY,","),"};\n")
    # }
    # if(length(M)*length(Y)>0){
    #     equation <- equation %>%
    #         paste0("{",str_flatten(M,","),"}->{",str_flatten(Y,","),"};\n")
    # }
    # if(length(M)>1){
    #     equation <- equation %>%
    #         paste0(str_flatten(M,"->"),";")
    # }
    # equation=paste0(equation,"\n}")
    # equation
    res=parameterEstimates(fit,standardized=TRUE)
    res
    res1=res[res$op!=":=",]
    if(residuals==FALSE) res1=res1[res1$lhs!=res1$rhs,]
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
                 temp=paste0(res1$rhs[i],"->",res1$lhs[i],"[")
             }
             equation <- equation %>% paste0(temp)
             if(is.na(res1$pvalue[i])) equation <- equation %>% paste0("style=dashed ")
             else if(res1$pvalue[i] >=0.05) equation <- equation %>% paste0("style=dashed ")
             if(res1$rhs[i]==res1$lhs[i]) equation <- equation %>% paste0("dir=both ")
             if(whatLabels=="std") temp=sprintf("%.02f",res1$std.all[i])
             else if(whatLabels=="est") temp=sprintf("%.02f",res1$est[i])
             else if(whatLabels=="name") temp=res1$label[i]
             if(temp!="") equation <- equation %>% paste0("label=",temp)
             equation <- equation %>% paste0("]\n")
        }
    }
    equation=paste0(equation,"\n}")
    equation
}

