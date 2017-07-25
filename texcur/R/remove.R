#' Removes based on regular expression
#'
#' @param corpus A dataframe of the corpus
#' @param pattern The regular expression
#' @return A filtered corpus
#'
#' @export
rm_regexp <- function(corpus, pattern) {
    .check_corpus(corpus)
    checkr::assert_string(pattern)
    .rm_regexp(corpus, pattern)
}

#' @keywords internal
.rm_regexp <- function(corpus, pattern) {
    .rm_regexp_helper <- function(text) {
        stringr::str_trim(stringr::str_replace_all(text, pattern, ""))
    }

    .apply(corpus, .rm_regexp_helper)
}

#' Removes numbers from corpus
#'
#' It does not remove numbers embedded in words such as 'hello123', '123hello', 'h3ll0'.
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
rm_numbers <- function(corpus) {
    rm_regexp(corpus, "\\b\\d+\\b")
}

#' Removes non-alphanumeric characters from corpus
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
rm_non_alphanumeric <- function(corpus) {
    rm_regexp(corpus, "[[:punct:]^]")
}

#' Removes emails from corpus
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
rm_emails <- function(corpus)  {
    rm_regexp(corpus, "\\b\\S+@\\S+.\\S\\b")
}

#' Removes urls from corpus
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
rm_urls <- function(corpus) {
    rm_regexp(corpus, "\\b((https?|ftp|file):\\/\\/)?\\S+\\.\\S+\\b")
}

#' Removes extra whitespace from corpus
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
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

#' Removes words from corpus
#'
#' @param corpus A dataframe of the corpus
#' @param tokens A dataframe containing the tokens of the corpus. If not specified, it will be created internally.
#' @param words A list of words
#' @param common_word_limit An integer defining the commonity in frequency
#' @param rare_word_limit An integer defining the rarity in frequency
#' @return A filtered corpus
#'
#' @export
rm_words <- function(corpus,
                     tokens=NULL,
                     words=NULL,
                     common_word_limit=NULL,
                     rare_word_limit=NULL,
                     ...) {
    .check_corpus(corpus, has.id=TRUE)
    .check_tokens(tokens, has.id=TRUE, null.ok=TRUE)
    checkr::assert_character(words, null.ok=TRUE)
    checkr::assert_integer(common_word_limit, lower=1, null.ok=TRUE)
    checkr::assert_integer(rare_word_limit, lower=1, null.ok=TRUE)

    if (is.null(tokens)) {
        tokens <- corpus %>% tf_tokenize(...)
    }

    token_counts <- tokens %>% dplyr::count(token)

    words_to_remove <- words

    common_words <- NULL
    rare_words <- NULL

    if (!is.null(common_word_limit)) {
        common_words <- .get_common_words(token_counts, common_word_limit)
    }

    if (!is.null(rare_word_limit)) {
        rare_words <- .get_rare_words(token_counts, rare_word_limit)
    }

    words_to_remove <- c(words_to_remove, common_words, rare_words)

    if (is.null(words_to_remove) | length(words_to_remove) == 0) return(corpus)

    .rm_words(corpus, words_to_remove, tokens)
}

#' @keywords internal
.get_common_words <- function(token_counts, common_word_limit) {
    common_words <- token_counts %>%
        dplyr::filter(n >= common_word_limit) %>%
        dplyr::mutate(n=NULL)

    common_words$token
}

#' @keywords internal
.get_rare_words <- function(token_counts, rare_word_limit) {
    rare_words <- token_counts %>%
        dplyr::filter(n <= rare_word_limit) %>%
        dplyr::mutate(n=NULL)

    rare_words$token
}

#' @keywords internal
.rm_words <- function(corpus, words, tokens) {
    words <- dplyr::data_frame(token=words)

    tokens %>%
        dplyr::mutate(row=row_number()) %>%
        dplyr::anti_join(words, by="token") %>%
        dplyr::arrange(row) %>%
        dplyr::mutate(row=NULL) %>%
        tf_merge_tokens(delim=" ")
}
