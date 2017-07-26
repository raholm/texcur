#' Transforms corpus to lowercase
#'
#' @param corpus Corpus to transform
#' @return A transformed corpus
#'
#' @export
tf_lowercase <- function(corpus) {
    .check_corpus(corpus)
    .apply(corpus, tolower)
}

#' Transforms corpus to tokens
#'
#' @param corpus Corpus to transform
#' @param token Definition of token (see \code{tidytext::unnest_tokens} for options)
#' @return A dataframe with token column containing each token
#'
#' @export
tf_tokenize <- function(corpus, token="words", ...) {
    .check_corpus(corpus)
    corpus %>%
        tidytext::unnest_tokens(token, text, token=token, to_lower=FALSE, ...)
}

#' Transforms tokens to corpus based on document id
#'
#' @param tokens Tokens to to merge
#' @param delim Delimiter between tokens
#' @return A dataframe with each document as id and text columns
#'
#' @export
tf_merge_tokens <- function(tokens, delim=" ") {
    .check_tokens(tokens, has.id=TRUE)
    checkr::assert_string(delim)

    other_vars <- tokens %>%
        dplyr::select(-token) %>%
        dplyr::distinct(id, .keep_all=TRUE)

    corpus <- tokens %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(text=paste0(token, collapse=delim))

    corpus %>%
        dplyr::left_join(other_vars, by="id")
}
