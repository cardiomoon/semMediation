# require(lavaan)
# require(semMediation)
# X="time.c";M="pubs";Y="jobs"
# moderator=list(name=c("alex.c"),site=list(c("a","c")))
# model=modmedEquation(X=X,M=M,Y=Y,moderator=moderator)
# cat(model)
# str(Success)
# fit=sem(model,data=Success)
# parameterEstimates(fit)
#
# semDiagram(fit)
# conceptDiagram(fit)
#
# require(stringr)

#'make interaction equation
#'@param x character vector
#'@param prefix prefix
interactStr=function(x,prefix="a"){
    res=c()
    count=1
    for(i in seq_along(x)){
        temp=paste0(prefix,count,"*",x[i])
        res=c(res,temp)
        count=count+1
        if(i>1){
            temp=paste0(prefix,count,"*",x[i],":",x[1])
            res=c(res,temp)
            count=count+1
        }
    }
    res
}

#' Extract groupby string
#' @param string character vector
#' @param groupby name of groupby
#' @importFrom stringr fixed
extractX=function(string,groupby="X"){
   for(i in seq_along(string)){
      if(string[i]==groupby) string[i]=paste0("1*",groupby)
   }
  str_replace(string,fixed(paste0("*",groupby)),"")
}

#' Make Grouping equation
#' @param x  character vector
#' @param groupby name of groupby
strGrouping=function(x,groupby="X"){

    yes=x[str_detect(x,groupby)]
    yes=extractX(yes,groupby=groupby)
    no=x[!str_detect(x,groupby)]
    no
    list(yes=yes,no=no)
}

#'Make moderated mediation equation
#' @param X A character vectors indicating independent variables
#' @param M A character vectors indicating mediators
#' @param Y A character vectors indicating dependent variables
#' @param moderator moderator
#' @param labels labels
#' @importFrom stringr str_replace_all
#' @export
#' @examples
#' X="X";M="M";Y="Y"
#' moderator=list(name=c("z1","z2"),site=list(c("a","b","c"),c("a","c")))
#' cat(modmedEquation(X=X,M=M,Y=Y,moderator=moderator))
modmedEquation=function(X="",M="",Y="",moderator=list(),labels=NULL){
      (XM=moderator$name[str_detect(moderator$site,"a")])
      (MY=moderator$name[str_detect(moderator$site,"b")])
      (XY=moderator$name[str_detect(moderator$site,"c")])

      # Regression of Moderator
      XM=c(X,XM)
      XMstr=interactStr(XM,prefix="a")
      equation=paste(M,"~",stringr::str_flatten(XMstr,"+"),"\n")
      MY=c(M,MY)
      XY=c(X,XY)
      MYstr=interactStr(MY,prefix="b")
      MYstr
      XYstr=interactStr(XY,prefix="c")
      XYstr
      temp=paste(Y,"~",stringr::str_flatten(XYstr,"+"),"+",
                 stringr::str_flatten(MYstr,"+"),"\n")
      temp
      equation=paste0(equation,temp)

      cat(equation)
      for(i in seq_along(moderator$name)){
        name=moderator$name[i]
        temp=paste0(name," ~ ",name,".mean*1\n")
        temp=paste0(temp,name," ~~ ",name,".var*",name,"\n")
        equation=paste0(equation,temp)
      }

      XMstr=stringr::str_replace_all(XMstr,":","*")
      ind1=strGrouping(XMstr,X)$yes
      ind1
      MYstr=stringr::str_replace_all(MYstr,":","*")
      ind2=strGrouping(MYstr,M)$yes
      ind2
      ind=paste0("(",str_flatten(ind1,"+"), ")*(",str_flatten(ind2,"+"),")")
      ind
      ind.below<-ind.above<-ind
      for(i in seq_along(moderator$name)){
          temp=paste0("(",moderator$name[i],".mean-sqrt(",moderator$name[i],".var))")
          ind.below=str_replace_all(ind.below,moderator$name[i],temp)
          temp=paste0("(",moderator$name[i],".mean+sqrt(",moderator$name[i],".var))")
          ind.above=str_replace_all(ind.above,moderator$name[i],temp)
      }
      ind.below
      ind.above
      equation=paste0(equation,"indirect.SDbelow:=",ind.below,"\n")
      equation=paste0(equation,"indirect.SDabove:=",ind.above,"\n")
      XYstr=stringr::str_replace_all(XYstr,":","*")
      XYstr
      direct=strGrouping(XYstr,X)$yes
      dir=paste0(str_flatten(direct,"+"))
      dir.below<-dir.above<-dir
      for(i in seq_along(moderator$name)){
          temp=paste0("(",moderator$name[i],".mean-sqrt(",moderator$name[i],".var))")
          dir.below=str_replace_all(dir.below,moderator$name[i],temp)
          temp=paste0("(",moderator$name[i],".mean+sqrt(",moderator$name[i],".var))")
          dir.above=str_replace_all(dir.above,moderator$name[i],temp)
      }
      equation=paste0(equation,"direct.SDbelow:=",dir.below,"\n")
      equation=paste0(equation,"direct.SDabove:=",dir.above,"\n")
      equation=paste0(equation,"total.SDbelow := direct.SDbelow + indirect.SDbelow\n",
                      "total.SDabove := direct.SDabove + indirect.SDabove\n")
      equation=paste0(equation,"prop.mediated.SDbelow := indirect.SDbelow / total.SDbelow\n",
                      "prop.mediated.SDabove := indirect.SDabove / total.SDabove\n")

      if(length(moderator$name)==1) {
          temp=ind1[str_detect(ind1,fixed("*"))]
          temp=unlist(str_split(temp,fixed("*")))
          temp[seq(1,length(temp),2)]
          ind2
          equation=paste0(equation,"index.mod.med := ",
                          temp[seq(1,length(temp),2)],"*",str_flatten(ind2,"+"),"\n")

      }
      equation
}

#' Remove parentheses
#' @param string A character vector
removeParentheses=function(string){
  res=c()
  for(i in seq_along(string)){
    x=string[i]
    x=str_replace(x,fixed(")"),"")
    x=unlist(str_split(x,fixed("*(")))
    x=unlist(str_split(x,fixed("+")))
    x

    for(i in seq_along(x)){
      if(i==1){
        if(length(x)==1) {
          temp=x[1]
        } else{
          temp=NULL
        }
      } else {
        temp=paste0(x[1],"*",x[i])
      }
      res=c(res,temp)
    }
  }
  res
}


