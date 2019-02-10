
#' Data Set for process macro model
#'
#'@format A data.frame with 43 rows and 7 variables
#'\describe{
#'   \item{no}{process macro model number}
#'   \item{X}{name of independent variable}
#'   \item{M}{names of mediator variables}
#'   \item{Y}{name of dependent variable}
#'   \item{modName}{names of moderator variables}
#'   \item{modSite}{sites of moderators}
#'   \item{pos}{position of moderators}
#'}
"pmacro"

#' Data Set for education and income
#'
#' A dataset contains measures about the teacher's knowlege, empathy and intervention about attention-deficit hyperactivity disorder(ADHD).
#'
#'@format A data.frame with 850 rows and 4 variables:
#'\describe{
#'   \item{age}{student age}
#'   \item{number}{number of students per class}
#'   \item{duration}{eduation duration}
#'   \item{income}{income}
#'}
"education"

#' Node Data Set for drawing stastical diagram of process macro model
#'
#'@format A data,frame with 327 rows and 4 variables
#'\describe{
#'   \item{no}{process macro model number}
#'   \item{name}{name of node}
#'   \item{xpos}{x position}
#'   \item{ypos}{y position}
#'}
"nodes"


#' Arrow Data Set for drawing stastical diagram of process macro model
#'
#'@format A data,frame with 392 rows and 6 variables
#'\describe{
#'   \item{no}{process macro model number}
#'   \item{name}{name of arrow}
#'   \item{start}{start node}
#'   \item{end}{end node}
#'   \item{labelpos}{position of label}
#'   \item{arrowpos}{position of arrow head}
#'}
"parrows1"
