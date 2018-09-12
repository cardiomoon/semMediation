X="time.c";M="pubs";Y="jobs"
moderator=list(name=c("alex.c"),site=list(c("a","c")))
model=modmedEquation(X=X,M=M,Y=Y,moderator=moderator)
cat(model)
str(Success)
model='pubs ~ a1*time.c+a2*alex.c+a3*alex.c:time.c
jobs ~ c1*time.c+c2*alex.c+c3*alex.c:time.c + b1*pubs
alex.c ~ alex.c.mean*1
alex.c ~~ alex.c.var*alex.c'
fit=sem(model,data=Success)
parameterEstimates(fit)

semDiagram(fit)
conceptDiagram(fit)
moderator
require(stringr)

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
      XYstr=interactStr(XY,prefix="c")
      temp=paste(Y,"~",stringr::str_flatten(XYstr,"+"),"+",
                 stringr::str_flatten(MYstr,"+"),"\n")
      equation=paste0(equation,temp)

      for(i in seq_along(moderator$name)){
        name=moderator$name[i]
        temp=paste0(name," ~ ",name,".mean*1\n")
        temp=paste0(temp,name," ~~ ",name,".var*",name,"\n")
        equation=paste0(equation,temp)
      }

      XYstr1=stringr::str_replace_all(XYstr,":","*")
      MEqui=stringr::str_flatten(XMstr,"+")
      MYstr1=str_replace(MYstr,M,paste0("(",MEqui,")"))
      MYstr1=stringr::str_replace_all(MYstr1,":","*")
      MYstr2=removeParentheses(MYstr1)
      totalStr=c(XYstr1,MYstr2)
      result=strGrouping(totalStr,X)
      direct=result$yes
      indirect=result$no

      equation=paste0(equation,"direct:=",str_flatten(direct,"+"),"\n")
      equation=paste0(equation,"indirect:=",str_flatten(indirect,"+"),"\n")
      equation=paste0(equation,"total:=direct+indirect\n")


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


