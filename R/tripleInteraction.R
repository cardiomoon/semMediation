#' Make triple interaction equation
#' @param vars variable names to be interact
#' @param prefix A character
#' @param suffix A number
#' @param mode A number
#' @param addPrefix A logical
#' @importFrom utils combn
#' @export
#' @examples
#' vars=c("negemot","sex","age")
#' tripleInteraction(vars)
#' tripleInteraction(vars,mode=1)
tripleInteraction=function(vars,prefix="c",suffix=0,mode=0,addPrefix=TRUE){

    if(mode){
        temp=paste(vars,collapse="*")

    } else{
    result=vars
    result=c(result,apply(combn(vars,2),2,function(x){paste(x,collapse=":")}))
    result=c(result,paste0("interaction",suffix))
    if(addPrefix) {
        temp=paste0(prefix,1:length(result),"*",result)
    } else{
        temp=result
    }
    }
    temp
}

#' Make equation with triple interaction
#' @param X Name of independent variable
#' @param M Name of mediator
#' @param Y Name of dependent variable
#' @param vars A list of variables names and sites
#' @param suffix A number
#' @param moderator A list of moderators
#' @param covar A list of covariates
#' @param mode A number
#' @param range A logical
#' @param data A data.frame
#' @param rangemode range mode
#' @param probs numeric vector of probabilities with values in [0,1]
#' @export
#' @examples
#' X="negemot";M="ideology";Y="govact";suffix=0
#' cat(tripleEquation(X=X,M=M,Y=Y))
#' vars=list(name=list(c("sex","age")),site=list(c("a","c")))
#' vars=list(name=list(c("W","Z"),c("V","Q")),site=list(c("a","b","c"),c("a","b","c")))
#' X="negemot";Y="govact";suffix=0
#' moderator=list(name=c("W"),site=list(c("c")))
#' cat(tripleEquation(X=X,Y=Y,moderator=moderator))
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
#' cat(tripleEquation(X=X,M=M,Y=Y,moderator=moderator,covar=covar))
#' cat(tripleEquation(X=X,M=M,Y=Y,moderator=moderator,covar=covar,mode=1))
#' cat(tripleEquation(X=X,M=M,Y=Y,vars=vars))
#' cat(tripleEquation(X=X,M=M,Y=Y,vars=vars,moderator=moderator,covar=covar))
#' cat(tripleEquation(X=X,M=M,Y=Y,vars=vars,mode=1))
#' cat(tripleEquation(X=X,M=M,Y=Y,vars=vars,covar=covar,mode=1))
#' X="negemot";Y="govact";suffix=0
#' vars=list(name=list(c("sex","age")),site=list(c("c")))
#' cat(tripleEquation(X=X,Y=Y,vars=vars))
tripleEquation=function(X=NULL,M=NULL,Y=NULL,vars=NULL,suffix=0,moderator=list(),
                        covar=NULL,range=TRUE,mode=0,data=NULL,rangemode=1,probs=c(0.16,0.5,0.84)){

      # moderator=list();covar=NULL;mode=0;M=NULL
     # mode=0;M=NULL;vars=NULL
     #

    # cat("str(vars)\n")
     # str(vars)
    # cat("str(moderator)\n")
    # str(moderator)


    temp1<-temp2<-temp3<-temp4<-NULL

   (XM=moderator$name[str_detect2(moderator$site,"a")])
   (MY=moderator$name[str_detect2(moderator$site,"b")])
   (XY=moderator$name[str_detect2(moderator$site,"c")])

    res=seekNameVars(vars,"a")
    if(length(res)>0) {
        for(i in 1:length(res)){
           newvars=c(X,vars$name[[res[i]]])
           temp1=c(temp1,tripleInteraction(newvars,prefix="a",suffix=suffix,mode=mode,addPrefix=FALSE))
           suffix=suffix+1
        }
   }
   temp1
   if(!is.null(M)){
   XM=c(X,XM)
   XMstr=interactStr(XM,addPrefix=FALSE)
   temp1=union(temp1,XMstr)
   if(mode==0) temp1=paste0("a",1:length(temp1),"*",temp1)

   temp4=paste0(M,"~",paste(temp1,collapse="+"))
   if(!is.null(covar)){
       covar$site=lapply(covar$site,function(x) str_replace(x,"M",M))
       if(mode){
         temp4=addCovarEquation(temp4,covar,prefix=NULL)
       } else{
         temp4=addCovarEquation(temp4,covar)
       }
       }
   }
   res=seekNameVars(vars,"b")
   length(res)
   if(length(res)>0) {
       for(i in 1:length(res)){
          newvars=c(M,vars$name[[res[i]]])
          temp2=c(temp2,tripleInteraction(newvars,prefix="b",suffix=suffix,mode=mode,addPrefix=FALSE))
          suffix=suffix+1
       }
   }
   temp2
   MY=c(M,MY)
   MYstr=interactStr(MY,addPrefix=FALSE)
   temp2=union(temp2,MYstr)
   temp2
   if(mode==0) {
       if(length(temp2)>0) temp2=paste0("b",1:length(temp2),"*",temp2)
   }

   res=seekNameVars(vars,"c")

   if(length(res)>0) {
       for(i in 1:length(res)){
        newvars=c(X,vars$name[[res[i]]])
        if("a" %in% vars$site[[res[i]]]){
            newsuffix=0
            if(res[i]>1){
                for(j in 1:res[i]){
                    if("a" %in% vars$site[[j]]) newsuffix=newsuffix+1
                }
            }
        } else{
            newsuffix=suffix
        }
        temp3=c(temp3,tripleInteraction(newvars,suffix=newsuffix,mode=mode,addPrefix=FALSE))
        suffix=suffix+1
       }
   }

   XY=c(X,XY)
   XY
   XYstr=interactStr(XY,addPrefix=FALSE)
   temp3=union(temp3,XYstr)
   if(mode==0) temp3=paste0("c",1:length(temp3),"*",temp3)
   temp3
   temp=c(temp2,temp3)
   temp=paste0(Y,"~",paste(temp,collapse="+"))
   if(!is.null(covar)){
      covar$site=lapply(covar$site,function(x) str_replace(x,"Y",Y))
      if(mode){
        temp=addCovarEquation(temp,covar,prefix=NULL)
      } else{
        temp=addCovarEquation(temp,covar)
      }

   }
   temp=c(temp4,temp)
   equation=paste(temp,collapse="\n")
   equation=paste0(equation,"\n")
   if(mode==0){
       moderatorNames=union(unlist(vars$name),moderator$name)
       for(i in seq_along(moderatorNames)){
           name=moderatorNames[i]
           temp=paste0(name," ~ ",name,".mean*1\n")
           temp=paste0(temp,name," ~~ ",name,".var*",name,"\n")
           equation=paste0(equation,temp)
       }
       temp=makeIndirectEquation(X,M,temp1,temp2,temp3,moderatorNames,range=range,data=data,rangemode=rangemode)
       equation=paste0(equation,temp)

   }
   equation
}


#' select names of variables from list var
#' @param vars A list
#' @param site Site for look kor
#' @export
#' @examples
#' vars=list(name=list(c("W","Z"),c("V","Q")),site=list(c("a","c"),c("b","c")))
#' vars=list(name=list(c("W","Z")),site=list(c("a","c")))
#' seekNameVars(vars,"a")
#' seekNameVars(vars,"b")
#' seekNameVars(vars,"c")
seekNameVars=function(vars,site="a"){
    result<-c()
    if(!is.null(vars)){
    select=which(unlist(lapply(lapply(vars$site,str_detect,site),any)))
    result<-select
    }
    result
}



#'Treat moderator name with mean value
#'@param ind An equation
#'@param moderatorNames character vectors
#' @param data A data.frame
#' @param rangemode range mode
#' @param probs numeric vector of probabilities with values in [0,1]
#'@export
#'@examples
#'ind="(a1+a4*sex+a5*age)*(b1)"
#'moderatorNames=c("age","sex")
#'treatModerator(ind,moderatorNames)
treatModerator=function(ind,moderatorNames,data=NULL,rangemode=1,probs=c(0.16,0.5,0.84)){
    ind.below<-ind.above<-ind
    for(i in seq_along(moderatorNames)){
      if(rangemode==2){
        values=quantile(data[[moderatorNames[i]]],probs=probs,type=6)
        temp=as.character(values[2])
        ind=str_replace_all(ind,moderatorNames[i],temp)
        temp=as.character(values[1])
        ind.below=str_replace_all(ind.below,moderatorNames[i],temp)
        temp=as.character(values[3])
        ind.above=str_replace_all(ind.above,moderatorNames[i],temp)
      } else{
        temp=paste0(moderatorNames[i],".mean")
        ind=str_replace_all(ind,moderatorNames[i],temp)
        temp=paste0("(",moderatorNames[i],".mean-sqrt(",moderatorNames[i],".var))")
        ind.below=str_replace_all(ind.below,moderatorNames[i],temp)
        temp=paste0("(",moderatorNames[i],".mean+sqrt(",moderatorNames[i],".var))")
        ind.above=str_replace_all(ind.above,moderatorNames[i],temp)
      }
    }
    list(ind,ind.below,ind.above)
}

#'Make indirect equation
#'@param X A character string
#'@param M A character string
#'@param temp1 A character vector
#'@param temp2 A character vector
#'@param temp3 A character vector
#'@param moderatorNames A character vector
#'@param range A logical
#' @param data A data.frame
#' @param rangemode range mode
#' @param probs numeric vector of probabilities with values in [0,1]
#'@export
#'@examples
#'X="negemot";M="ideology"
#'temp1=c("a1*negemot","a2*sex","a4*negemot*sex","a5*negemot*age","a6*sex*age")
#'temp2="b1*ideology"
#'temp3="c1*negemot"
#'moderatorNames=c("age","sex")
#'cat(makeIndirectEquation(X,M,temp1,temp2,temp3,moderatorNames))
#'cat(makeIndirectEquation(X,M,temp1,temp2,temp3,moderatorNames,range=TRUE))
#'#glbwarm=read.csv("./inst/pmacro/data/glbwarm.csv",stringsAsFactors=FALSE)
#'#cat(makeIndirectEquation(X,M,temp1,temp2,temp3,moderatorNames,range=TRUE,data=glbwarm,rangemode=2))
makeIndirectEquation=function(X,M,temp1,temp2,temp3,moderatorNames,
                              range=FALSE,data=NULL,rangemode=1,probs=c(0.16,0.5,0.84)){
    equation=""
    if(!is.null(M)){

        temp1=stringr::str_replace_all(temp1,":","*")
        ind1=strGrouping(temp1,X)$yes
        temp2=stringr::str_replace_all(temp2,":","*")
        ind2=strGrouping(temp2,M)$yes
        ind=paste0("(",str_flatten(ind1,"+"), ")*(",str_flatten(ind2,"+"),")")

        res=treatModerator(ind,moderatorNames,data=data,rangemode=rangemode,probs=probs)
        ind<-res[[1]]
        ind.below=res[[2]]
        ind.above=res[[3]]
        equation=paste0(equation,"indirect :=",ind,"\n")

        temp3=stringr::str_replace_all(temp3,":","*")
        direct=strGrouping(temp3,X)$yes
        dir=paste0(str_flatten(direct,"+"))
        dir
        res=treatModerator(dir,moderatorNames,data=data,rangemode=rangemode,probs=probs)
        dir<-res[[1]]
        dir.below=res[[2]]
        dir.above=res[[3]]
        equation=paste0(equation,"direct :=",dir,"\n")
        equation=paste0(equation,"total := direct + indirect\n")
        if(range){
            equation=paste0(equation,"indirect.below :=",ind.below,"\n")
            equation=paste0(equation,"indirect.above :=",ind.above,"\n")
            equation=paste0(equation,"direct.below:=",dir.below,"\n")
            equation=paste0(equation,"direct.above:=",dir.above,"\n")
            equation=paste0(equation,"total.below := direct.below + indirect.below\n",
                            "total.above := direct.above + indirect.above\n")
            equation=paste0(equation,"prop.mediated.below := indirect.below / total.below\n",
                            "prop.mediated.above := indirect.above / total.above\n")


            if(length(moderatorNames)==1) {
                temp=ind1[str_detect(ind1,fixed("*"))]
                temp=unlist(str_split(temp,fixed("*")))
                if(length(temp)>2){
                temp[seq(1,length(temp),2)]
                ind2
                equation=paste0(equation,"index.mod.med := ",
                                temp[seq(1,length(temp),2)],"*",str_flatten(ind2,"+"),"\n")
                }

            }
        }
    }
    equation
}

