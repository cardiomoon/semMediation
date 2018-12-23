#' Data Set for Teacher's Intervention for ADHD
#'
#' A dataset contains measures about the teacher's knowlege, empathy and intervention about attention-deficit hyperactivity disorder(ADHD).
#'
#'@format A data.frame with 334 rows and 12 variables:
#'\describe{
#'   \item{id}{study identification number}
#'   \item{gender}{male=1, female=2}
#'   \item{age}{age groups 1-4}
#'   \item{general}{general knowledge estimate derived from Knowledge of
#'   Attention Deficit Disorder Scale(KADDS) developed by Sciutto, Terjesen and Frank(2000). Maximum 15 points.}
#'   \item{symptoms}{knowledge about ADHD symptoms estimate derived from Knowledge
#'   of Attention Deficit Disorder Scale(KADDS) developed by Sciutto, Terjesen and Frank(2000). Maximum 9 points.}
#'   \item{treatmt}{knowledge about ADHD treatment estimate derived from Knowledge of Attention Deficit Disorder Scale(KADDS)
#'   developed by Sciutto, Terjesen and Frank(2000). Maximum 12 points.}
#'   \item{cognitiv}{estimates of cognitive empathy. Maximum 35 points.}
#'   \item{emotion}{estimates of emotional empathy. Maximum 35 points.}
#'   \item{disposit}{estimates of dispositional empathy. Maximum 60 points.}
#'   \item{attitude}{estimates of attitude empathy. Maximum 35 points.}
#'   \item{classrm}{estimates of classroom intervention. Maximum 36 points.}
#'   \item{instruct}{estimates of instructional intervention. Maximum 36 points.}
#'}
#'@source {Effects of teacher's knowledge and empathy on educational intervention for ADHD: Focused on the mediating effet of empathy. J Korean Acad Psychiatr Ment Health Nurs 2013:22;45-55.}
"ADHD"


#' Data Set for process macro
#' A dataset for support for process macro
#'
#' @format A data.frame with 6 variables
#' \describe{
#' \item{no}{A number of process macro model}
#' \item{X}{Name of the independent variable}
#' \item{M}{Name of the mediator}
#' \item{Y}{Name of the independent variable}
#' \item{modName}{Name(s) of moderator names collapsed with colon}
#' \item{modSite}{Site(s) of moderation collapsed with colon}
#' \item{pos}{Position of moderator(s) in a diagram collapsed with comma}
#' }
"pmacro"


