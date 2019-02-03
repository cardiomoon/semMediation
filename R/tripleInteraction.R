#' Make triple interaction equation
#' @param vars variable names to be interact
#' @param dep name of dependent variable
#' @param prefix A character
#' @param suffix A number
#' @param mode A number
#' @importFrom utils combn
#' @export
#' @examples
#' vars=c("negemot","sex","age");dep="govact"
#' tripleInteraction(vars,dep)
#' tripleInteraction(vars,dep,mode=1)
tripleInteraction=function(vars,dep,prefix="c",suffix=0,mode=0){

    if(mode){
        temp=paste(vars,collapse="*")

    } else{
    result=vars
    result=c(result,apply(combn(vars,2),2,function(x){paste(x,collapse=":")}))
    result=c(result,paste0("interaction",suffix))
    temp=paste0(prefix,1:length(result),"*",result)
    }
    paste0(dep,"~",paste(temp,collapse="+"))
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
#' vars=c("negemot","sex","age");dep="govact";suffix=0
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
#' Y="govact"
#' tripleEquation(Y=Y,vars=vars,dep=dep,covar=covar)
#' tripleEquation(Y=Y,vars=vars,dep=dep,covar=covar,mode=1)
tripleEquation=function(M=NULL,Y=NULL,vars,dep,suffix=0,covar,mode=0){
    equation=tripleInteraction(vars,dep,suffix=suffix,mode=mode)
    if(!is.null(Y)) covar$site=lapply(covar$site,function(x) str_replace(x,"Y",Y))
    if(!is.null(M)) covar$site=lapply(covar$site,function(x) str_replace(x,"M",M))
    if(mode){
        temp=addCovarEquation(equation,covar,prefix=NULL)
    } else{
      temp=addCovarEquation(equation,covar,prefix="h")
    }
    temp
}
