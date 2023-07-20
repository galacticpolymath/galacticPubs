#' lesson_init
#'
#' Set up a new Galactic Polymath lesson or mini-unit project folder. Create folder structure in 'GP-Studio'/Edu/Lessons Google Shared Drive and add meta/files
#'
#' Will fail if ShortName duplicates an existing name found in GP-Studio or GP-Live
#'
#' @param ShortName What will be the ShortName prefix for the front-matter.yml?
#' @export
#'

lesson_init <- \(ShortName){
inputs <- lesson_init_helper()
print(inputs)
}

#' init_lesson
#'
#' @describeIn lesson_init alias for init_lesson
#' @export

init_lesson <- lesson_init
