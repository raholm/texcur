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
#' @param tokenized_corpus Tokenized corpus to merge
#' @param delim Delimiter between tokens
#' @return A dataframe with each document as id and text columns
#'
#' @export
tf_merge_tokens <- function(tokenized_corpus, delim=" ") {
    .check_tokenized_corpus(tokenized_corpus, has.id=TRUE)
    checkr::assert_string(delim)

    tokenized_corpus %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(text=paste(token, sep="", collapse=delim),
               token=NULL) %>%
        dplyr::filter(row_number() == 1) %>%
        dplyr::ungroup()
}