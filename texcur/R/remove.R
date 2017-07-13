#' Removes digits from corpus
#'
#' It does not remove numbers embedded in words such as 'hello123', '123hello', 'h3ll0'.
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
rm_numbers <- function(corpus) {
    .check_corpus(corpus)
    .rm_numbers(corpus)
}

#' @keywords internal
.rm_numbers <- function(corpus) {
    .rm_numbers_helper <- function(text) {
        stringr::str_trim(stringr::str_replace_all(text, "\\b\\d+\\b", ""))
    }

    .apply(corpus, .rm_numbers_helper)
}

#' Removes non-alphanumeric characters from corpus
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
rm_non_alphanumeric <- function(corpus) {
    .check_corpus(corpus)
    .rm_non_alphanumeric(corpus)
}

#' @keywords internal
.rm_non_alphanumeric <- function(corpus) {
    .rm_non_alphanumeric_helper <- function(text) {
        stringr::str_trim(stringr::str_replace_all(text, "[[:punct:]^]", ""))
    }

    .apply(corpus, .rm_non_alphanumeric_helper)
}

#' Removes emails from corpus
#'
#' @export
rm_emails <- function(corpus)  {
    .check_corpus(corpus)
    .rm_emails(corpus)
}

#' @keywords internal
.rm_emails <- function(corpus) {
    .rm_emails_helper <- function(text) {
        stringr::str_trim(stringr::str_replace_all(text, "\\b\\S+@\\S+.\\S\\b", ""))
    }

    .apply(corpus, .rm_emails_helper)
}

#' Removes urls from corpus
#'
#' @export
rm_urls <- function(corpus) {
    .check_corpus(corpus)
    .rm_urls(corpus)
}

#' @keywords internal
.rm_urls <- function(corpus) {
    .rm_urls_helper <- function(text) {
        stringr::str_trim(stringr::str_replace_all(text, "\\b((https?|ftp|file):\\/\\/)?\\S+\\.\\S+\\b", ""))
    }

    .apply(corpus, .rm_urls_helper)
}

#' Removes extra whitespace
#'
#' @export
rm_whitespace <- function(corpus) {
    .check_corpus(corpus)
    .rm_whitespace(corpus)
}

#' @keywords internal
.rm_whitespace <- function(corpus) {
    .rm_whitespace_helper <- function(text) {
        ## stringr::str_trim(stringr::str_replace_all(extra_whitespace, "(?!([ \\t\\r\\n]\\s))\\s+", ""))
        ## stringr::str_trim(stringr::str_replace_all(text, "\\s+", " "))
        removed_spaces <- stringr::str_replace_all(text, "( ){2,}", " ")
        removed_tabs <- stringr::str_replace_all(removed_spaces, "(\t){2,}", "\t")
        removed_newlines <- stringr::str_replace_all(removed_tabs, "(\n){2,}", "\n")
        stringr::str_trim(removed_newlines)
    }

    .apply(corpus, .rm_whitespace_helper)
}
