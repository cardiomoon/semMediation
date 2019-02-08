#' Make triple interaction equation
#' @param vars variable names to be interact
#' @param prefix A character
#' @param suffix A number
#' @param mode A number
#' @importFrom utils combn
#' @export
#' @examples
#' vars=c("negemot","sex","age")
#' tripleInteraction(vars)
#' tripleInteraction(vars,mode=1)
tripleInteraction=function(vars,prefix="c",suffix=0,mode=0){

    if(mode){
        temp=paste(vars,collapse="*")

    } else{
    result=vars
    result=c(result,apply(combn(vars,2),2,function(x){paste(x,collapse=":")}))
    result=c(result,paste0("interaction",suffix))
    temp=paste0(prefix,1:length(result),"*",result)
    }
    temp
}

#' Make equation with triple interaction
#' @param M Name of mediator
#' @param Y Name of dependent variable
#' @param vars variable names to be interact
#' @param dep name of dependent variable
#' @param suffix A number
#' @param covar A list of covariates
#' @param mode A number
#' @export
#' @examples
#' X="negemot";M="ideology";Y="govact";vars=c("sex","age");site=c("a","b","c");suffix=0
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
#' cat(tripleEquation(X=X,M=M,Y=Y,vars=vars,site=site))
#' cat(tripleEquation(X=X,M=M,Y=Y,vars=vars,site=site,covar=covar))
#' cat(tripleEquation(X=X,M=M,Y=Y,vars=vars,site=site,mode=1))
#' cat(tripleEquation(X=X,M=M,Y=Y,vars=vars,site=site,covar=covar,mode=1))
tripleEquation=function(X=NULL,M=NULL,Y=NULL,vars,site,suffix=0,covar=NULL,mode=0){
   temp1<-temp2<-temp3<-NULL
   if("a" %in% site) {
       newvars=c(X,vars)
       temp1=tripleInteraction(newvars,prefix="a",suffix=suffix,mode=mode)
       temp1=paste0(M,"~",paste(temp1,collapse="+"))
       if(!is.null(covar)){
       covar$site=lapply(covar$site,function(x) str_replace(x,"M",M))
       if(mode){
         temp1=addCovarEquation(temp1,covar,prefix=NULL)
       } else{
         temp1=addCovarEquation(temp1,covar)
       }
       }
       suffix=suffix+1
   }
   if("b" %in% site) {
     newvars=c(M,vars)
     temp2=tripleInteraction(newvars,prefix="b",suffix=suffix,mode=mode)
     suffix=suffix+1
   }
   if("c" %in% site) {
     newvars=c(X,vars)
     temp3=tripleInteraction(newvars,suffix=suffix,mode=mode)
   }
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
   temp=c(temp1,temp)
   equation=paste(temp,collapse="\n")
   equation
}
