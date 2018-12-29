#' Remodel selection
#'
#' An rstudio addin to convert a latex tabel to a tribble.
#'
#' @return nothing
#' @export
#'
remodel_selection <- function(){

  if (rstudioapi::isAvailable()){
    source_context <- rstudioapi::getSourceEditorContext()
    selection_content <- source_context$selection[[1]]$text

    if (nzchar(selection_content)){
      ## replace all friendlyeval functions in selection with rlang
      table_df <- remodel(selection_content)
      rstudioapi::modifyRange(location = source_context$selection[[1]]$range,
                              text = table_df,
                              id = source_context$id)

    } else {
      stop("I only remodel selections for now.")

    }

  }
}

remodel <- function(selection_content){

  table_lines <-
    stringr::str_replace_all(selection_content, "&\\s*\n", "& ") %>%
    stringr::str_replace_all("\n\\s*&", " &") %>%
    stringr::str_replace_all("(^|\\n)\\\\[:graph:]*(?=\\n|$)", "") %>%
    stringr::str_replace_all("\\\\.*", "") %>%
    stringr::str_replace_all("\n\n", "\n") %>%
    stringr::str_replace_all("(?<=^|\n)\\s*\n", "\n") %>%
    stringr::str_replace_all("&", ",")

    output_table <- read.csv(textConnection(table_lines), header = FALSE)

    datapasta::tribble_construct(output_table)
}
