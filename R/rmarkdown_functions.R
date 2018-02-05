#' Apply all SI knitr options
#' @description Applies all minterbrand custom knitr options.
#' @return Invisibly sets knitr options
#' @examples
#' minter_knitr_settings()
#' @export

minter_knitr_settings <- function(verbose = F) {
  # Echo=FALSE means the code does not show up in the document
  # Caching allows you to re-run the report quickly
  knitr::opts_chunk$set(echo=FALSE)
  knitr::opts_chunk$set(cache=TRUE)
  knitr::opts_chunk$set(message = T) #Use F to suppress all messages from chunks for finalized report
  knitr::opts_chunk$set(warning = T) #Use F to suppress all warnings from chunks for finalized report

  # Setting the default resolution of plots
  knitr::opts_chunk$set(dpi = 300)

  # Setting how numbers are displayed
  knit_hooks$set(inline = function(x) { #This puts a nice comma in large inline numbers to group by 3 digits
    prettyNum(x, big.mark=",")
  })

  if(verbose) message("knitr opts_chunk set to: echo = F, cache = T, message = T, warning = T, dpi = 300, comma bigmark for inline numbers.")

}


#' Draft a minterbrand PDF Report
#' @description Drafts a new minterbrand PDF RMarkdown report
#' @param file File name for the draft
#' @examples
#' minter_draft_pdf_report("myReport.Rmd")
#' @export
minter_draft_pdf_report <- function(file) {
  if(dirname(file) == ".") { #If path wasn't provided, place assume the working directory is the base dir and create subdir for markdown file.
    message("Directory not provided. Using working directory.")
    file <- file.path(getwd(), file)
  }
  
  if(tools::file_ext(file) != "Rmd") { #Fix the capitalization of Rmd
    message("Setting file extension to .Rmd")
    file <- paste0(tools::file_path_sans_ext(file), ".Rmd")
  }
  
  rmarkdown::draft(file = file,
                   template = "minter_pdf_report",
                   package = "minterbrand",
                   edit = F)
  
  file.edit(file.path(tools::file_path_sans_ext(file), basename(file)))
}

#' Draft a minterbrand HTML Report
#' @description Drafts a new minterbrand HTMLrmarkdown report
#' @param file File name for the draft
#' @examples
#' minter_draft_html_report("myReport.Rmd")
#' @export
minter_draft_html_report <- function(file) {
  if(dirname(file) == ".") { #If path wasn't provided, place assume the working directory is the base dir and create subdir for markdown file.
    message("Directory not provided. Using working directory.")
    file <- file.path(getwd(), file)
  }

  if(tools::file_ext(file) != "Rmd") { #Fix the capitalization of Rmd
    message("Setting file extension to .Rmd")
    file <- paste0(tools::file_path_sans_ext(file), ".Rmd")
  }

  rmarkdown::draft(file = file,
                   template = "minter_html_report",
                   package = "minterbrand",
                   edit = F)

  file.edit(file.path(tools::file_path_sans_ext(file), basename(file)))
}


#' Draft a minterbrand Report
#' @description Drafts a new minterbrand full rmarkdown report
#' @param file File name for the draft
#' @examples
#' minter_draft_full_report("My presentation.Rmd")
#' @export
minter_draft_full_report <- function(file) {
  if(dirname(file) == ".") { #If path wasn't provided, place assume the working directory is the base dir and create subdir for markdown file.
    message("Directory not provided. Using working directory.")
    file <- file.path(getwd(), file)
  }

  if(tools::file_ext(file) != "Rmd") { #Fix the capitalization of Rmd
    message("Setting file extension to .Rmd")
    file <- paste0(tools::file_path_sans_ext(file), ".Rmd")
  }

  rmarkdown::draft(file = file,
                   template = "minter_report",
                   package = "minterbrand",
                   edit = F)

  file.edit(file.path(tools::file_path_sans_ext(file), basename(file)))
}


#' Draft a minterbrand Presentation
#' @description Drafts a new minterbrand ioslides presentation.
#' @param file File name for the draft
#' @examples
#' minter_draft_presentation("My presentation.Rmd")
#' @export
minter_draft_presentation <- function(file) {

  if(dirname(file) == ".") { #If path wasn't provided, place assume the working directory is the base dir and create subdir for markdown file.
    message("Directory not provided. Using working directory.")
    file <- file.path(getwd(), file)
  }

  if(tools::file_ext(file) != "Rmd") { #Fix the capitalization of Rmd
    message("Setting file extension to .Rmd")
    file <- paste0(tools::file_path_sans_ext(file), ".Rmd")
  }

  rmarkdown::draft(file = file,
                  template = "minter_ioslides",
                  package = "minterbrand",
                  edit = F)
  file.edit(file.path(tools::file_path_sans_ext(file), basename(file)))
}
