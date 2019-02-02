#'Make regression equation
#' @param X A character vectors indicating independent variables
#' @param M A character vectors indicating mediators
#' @param Y A character vectors indicating dependent variables
#' @param moderator moderator
#' @param covar covariates
#' @export
#' @examples
#' X="X";M=NULL;Y="Y"; moderator=list(name="W",site=list("c"))
#' regEquation(X,M,Y,moderator)
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
#' regEquation(X,M,Y,moderator,covar=covar)
regEquation=function(X="X",M=NULL,Y="Y",moderator=list(),covar=list()){
    (XM=moderator$name[str_detect2(moderator$site,"a")])
    (MY=moderator$name[str_detect2(moderator$site,"b")])
    (XY=moderator$name[str_detect2(moderator$site,"c")])

    if(length(XY)>0) {
        temp=c()
        for(i in 1:length(XY)){
            temp=c(temp,paste0(X,"*",XY[i]))
        }
        equation=paste0(temp,collapse="+")
        equation=paste(Y,"~",equation)
    } else{
        equation=paste0(Y,"~",X)
    }
    equation
    equation=addCovarEquation(equation,covar=covar,prefix=NULL)
    equation
}
