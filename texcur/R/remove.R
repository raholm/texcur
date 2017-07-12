#' Removes digits from corpus
#'
#' It does not remove numbers embedded in words such as 'hello123', '123hello', 'h3ll0'.
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
remove_numbers <- function(corpus) {
    .check_corpus(corpus)
    .remove_numbers(corpus)
}

#' @keywords internal
.remove_numbers <- function(corpus) {
    .remove_numbers_helper <- function(text) {
        stringr::str_trim(stringr::str_replace_all(text, "\\b\\d+\\b", ""))
    }

    .remove(corpus, .remove_numbers_helper)
}

#' Removes non-alphanumeric characters from corpus
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
remove_non_alphanumeric <- function(corpus) {
    .check_corpus(corpus)
    .remove_non_alphanumeric(corpus)
}

#' @keywords internal
.remove_non_alphanumeric <- function(corpus) {
    .remove_non_alphanumeric_helper <- function(text) {
        stringr::str_trim(stringr::str_replace_all(text, "[[:punct:]^]", ""))
    }

    .remove(corpus, .remove_non_alphanumeric_helper)
}

#' Removes emails from corpus
#'
#' @export
remove_emails <- function(corpus)  {
    .check_corpus(corpus)
    .remove_emails(corpus)
}

#' @keywords internal
.remove_emails <- function(corpus) {
    .remove_emails_helper <- function(text) {
        stringr::str_trim(stringr::str_replace_all(text, "\\b\\S+@\\S+.\\S\\b", ""))
    }

    .remove(corpus, .remove_emails_helper)
}

#' Removes urls from corpus
#'
#' @export
remove_urls <- function(corpus) {
    .check_corpus(corpus)
    .remove_urls(corpus)
}

#' @keywords internal
.remove_urls <- function(corpus) {
    .remove_urls_helper <- function(text) {
        stringr::str_trim(stringr::str_replace_all(text, "\\b((https?|ftp|file):\\/\\/)?\\S+\\.\\S+\\b", ""))
    }

    .remove(corpus, .remove_urls_helper)
}

#' Removes extra whitespace
#'
#' @export
remove_whitespace <- function(corpus) {
    .check_corpus(corpus)
    .remove_whitespace(corpus)
}

#' @keywords internal
.remove_whitespace <- function(corpus) {
    .remove_whitespace_helper <- function(text) {
        ## stringr::str_trim(stringr::str_replace_all(extra_whitespace, "(?!([ \\t\\r\\n]\\s))\\s+", ""))
        ## stringr::str_trim(stringr::str_replace_all(text, "\\s+", " "))
        removed_spaces <- stringr::str_replace_all(text, "( ){2,}", " ")
        removed_tabs <- stringr::str_replace_all(removed_spaces, "(\t){2,}", "\t")
        removed_newlines <- stringr::str_replace_all(removed_tabs, "(\n){2,}", "\n")
        stringr::str_trim(removed_newlines)
    }

    .remove(corpus, .remove_whitespace_helper)
}

#' @keywords internal
.check_corpus <- function(corpus) {
    checkr::assert_type(corpus, "tbl_df")
    checkr::assert_subset("text", names(corpus))
    checkr::assert_character(corpus$text)
}

#' @keywords internal
.remove <- function(corpus, func) {
    corpus %>% dplyr::mutate(text=func(text))
}
