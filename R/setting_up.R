#' Get Filestream path
#'
#' @description Identifies the operative system in which we are running, and returns the correct path to where Google Drive is mounted
#'
#' @return The OS-specific mounting of Google Drive
#' @export
#'

get_google_drive_path <- function(){

  system <- Sys.info()["sysname"]

  if(system == "Windows") {
    path <- "G:"
  } else {
    path <- "~/Google Drive"
  }

  return(path)
}

#' Create emLab directories
#'
#' @description Creates a set of standard directories in the emLab "current projects" folder: 01_raw_data, 02_processed_data, 03_output_data
#'
#' @param project_codename The code name used for the project. It should match the name used in the "current project" directory at emLab
#' @param other_dirs If needed, a character vector containing the names for extra directories.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' create_dirs() # This creates default directories
#'
#' create_dirs(other_dirs = "reports") # Creates all default directories and an "reports" directory
#' }
#'
#'

create_emlab_dirs <- function(project_codename, other_dirs = NULL){

  # Directory where all projects live
  projects_path <- file.path(get_google_drive_path(), "Shared drives/emlab/projects/current-projects")

  # Directory where this project will live
  project_path <- file.path(projects_path, project_codename)

  dir.create(path = project_path)

  # Standard directories
  standard_dirs <- c("data", "data/01_raw_data", "data/02_processed_data", "data/03_output_data", "figures", "presentations")
  sapply(file.path(project_path, standard_dirs), dir.create)

  if(!is.null(other_dirs)){
    sapply(file.path(project_path, other_dirs), dir.create)
  }

}

#' Create setup script
#'
#' @param project_codename The code name used for the project. It should match the name used in the "current project" directory at emLab
#'
#' @export
#'

create_setup_script <- function(project_codename) {
  writeLines(
    text = c(
      '##########################\n## Paths to directories ##\n##########################\n',
      '# Check for OS',
      'sys_path <- easyR::get_google_drive_path()\n',
      '# Path to emLab data folder',
      'emlab_data_path <- file.path(sys_path,"Shared drives/emlab/data")\n',
      '# Path to this project folder',
      paste0('project_path <- file.path(sys_path,"Shared drives/emlab/projects/current-projects/', project_codename, '")')
    ),
    con = "scripts/00_setup.R"
  )

  message("Adding setup script to Rprofile...\nIf you move or rename your setup script, remember to modify your .Rprofile file with `usethis::edit_r_profile()`")
  line <- 'source("scripts/00_setup.R")'
  write(x = line, file = ".Rprofile", append = TRUE)
}


#' Create README
#'
#' @description Creates a README.md file containing the structure of the repository and a footer
#'
#' @param repo (character) The repository name, or any other title, defaults to 'Repository'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' create_readme() # Creates a readme file
#' }
#'
create_readme <- function(repo = "Repository"){
  # Twee was created, of course, by awesome Jenny Brian:
  # https://gist.github.com/jennybc/2bf1dbe6eb1f261dfe60

  twee <- function(path, level = Inf) {

    fad <- list.files(path = path, recursive = TRUE, no.. = TRUE, include.dirs = TRUE)

    fad_split_up <- strsplit(fad, "/")

    too_deep <- lapply(fad_split_up, length) > level
    fad_split_up[too_deep] <- NULL

    jfun <- function(x) {
      n <- length(x)
      if(n > 1)
        x[n - 1] <- "|__"
      if(n > 2)
        x[1:(n - 2)] <- "   "
      x <- if(n == 1) c("-- ", x) else c("   ", x)
      x
    }
    fad_subbed_out <- lapply(fad_split_up, jfun)

    tree <- unlist(lapply(fad_subbed_out, paste, collapse = ""))

    return(tree)
  }

  writeLines(text = c(paste0("# ", repo,"\n\n"),
                      "## Repository structure \n",
                      "```",
                      twee(path = getwd(), level = 3),
                      "```",
                      "\n---------"),
             con = "README.md")
}

