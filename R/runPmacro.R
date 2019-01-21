#' Run process macro shiny app
#' @importFrom shiny runApp
#' @export
runPmacro=function(){
    shiny::runApp(system.file('pmacro',package='semMediation'))
}
