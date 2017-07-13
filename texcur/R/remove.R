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

#' Removes rare words from corpus
#'
#' @param corpus A dataframe of the corpus
#' @param rare_word_limit An integer defining the rarity in frequency
#' @param tokenized_corpus (Optional) A tokenized version of the corpus used to find word frequencies
#' @return A filtered corpus
#'
#' @export
rm_rare_words <- function(corpus, rare_word_limit, tokenized_corpus=NULL) {
    .check_corpus(corpus)
    .check_tokenized_corpus(tokenized_corpus, null.ok=TRUE)
    checkr::assert_integer(rare_word_limit, lower=1)

    if (is.null(tokenized_corpus)) {
        tokenized_corpus <- corpus %>% tf_tokenize(token="words")
    }

    rare_words <- .get_rare_words(tokenized_corpus, rare_word_limit)
    .rm_words(corpus, rare_words, tokenized_corpus)
}

#' @keywords internal
.get_rare_words <- function(tokenized_corpus, rare_word_limit) {
    rare_words <- tokenized_corpus %>%
        dplyr::count(token) %>%
        dplyr::filter(n <= rare_word_limit) %>%
        dplyr::mutate(n=NULL)

    rare_words$token
}

#' Removes common words from corpus
#'
#' @param corpus A dataframe of the corpus
#' @param common_word_limit An integer defining the "commonity" in frequency
#' @param tokenized_corpus (Optional) A tokenized version of the corpus used to find word frequencies
#' @return A filtered corpus
#'
#' @export
rm_common_words <- function(corpus, common_word_limit, tokenized_corpus=NULL) {
    .check_corpus(corpus)
    .check_tokenized_corpus(tokenized_corpus, null.ok=TRUE)
    checkr::assert_integer(common_word_limit, lower=1)

    if (is.null(tokenized_corpus)) {
        tokenized_corpus <- corpus %>% tf_tokenize(token="words")
    }

    common_words <- .get_common_words(tokenized_corpus, common_word_limit)
    .rm_words(corpus, common_words, tokenized_corpus)
}

#' @keywords internal
.get_common_words <- function(tokenized_corpus, common_word_limit) {
    common_words <- tokenized_corpus %>%
        dplyr::count(token) %>%
        dplyr::filter(n >= common_word_limit) %>%
        dplyr::mutate(n=NULL)

    common_words$token
}

#' Removes specified words from corpus
#'
#' @param corpus A dataframe of the corpus
#' @param rare_word_limit An integer defining the rarity in frequency
#' @param tokenized_corpus Not yet implemented
#' @return A filtered corpus
#'
#' @export
rm_words <- function(corpus, words, tokenized_corpus=NULL) {
    .check_corpus(corpus)
    .check_tokenized_corpus(tokenized_corpus, null.ok=TRUE)
    checkr::assert_character(words)

    .rm_words(corpus, words, tokenized_corpus)
}

#' Could do this using tokenized corpus with anti_join,
#' however it would require that we merge the remaining tokens
#' to their respective document
#'
#' @keywords internal
.rm_words <- function(corpus, words, tokenized_corpus=NULL) {
    pattern <- paste("\\b(", paste(words, collapse="|"), ")\\s?", sep="")
    corpus %>% rm_regexp(pattern)
}
