#' Removes based on regular expression
#'
#' @param corpus A dataframe of the corpus
#' @param pattern The regular expression
#' @return A filtered corpus
#'
#' @export
rm_regexp <- function(corpus, pattern) {
    .check_corpus(corpus)
    checkmate::assert_string(pattern)
    .rm_regexp(corpus, pattern)
}

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

.rm_whitespace <- function(corpus) {
    .rm_whitespace_helper <- function(text) {
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
#' @param common_word_limit An integer or numeric defining the commonity in frequency
#' @param rare_word_limit An integer or numeric defining the rarity in frequency
#' @param percentage If \code{TRUE}, then word limits will be interpreted as percentage, else as counts
#' @param inclusive If \code{TRUE}, then if the cutoff point is between the same token type all of them will be removed, else none of them will be removed. Only used if \code{percentage}=\code{TRUE}.
#' @return A filtered corpus
#'
#' @export
rm_words <- function(corpus,
                     tokens=NULL,
                     words=NULL,
                     common_word_limit=NULL,
                     rare_word_limit=NULL,
                     percentage=FALSE,
                     inclusive=FALSE,
                     ...) {
    .check_corpus(corpus, has_id=TRUE)
    .check_tokens(tokens, has_id=TRUE, null_ok=TRUE)
    checkmate::assert_character(words, null.ok=TRUE)
    checkmate::assert_logical(percentage, len=1)

    if (percentage) {
        checkmate::assert_numeric(common_word_limit, len=1, lower=0, upper=1, null.ok=TRUE)
        checkmate::assert_numeric(rare_word_limit, len=1, lower=0, upper=1, null.ok=TRUE)
    } else {
        checkmate::assert_numeric(common_word_limit, len=1, lower=1, null.ok=TRUE)
        checkmate::assert_numeric(rare_word_limit, len=1, lower=1, null.ok=TRUE)
    }

    if (checkr::is_null(tokens)) {
        tokens <- corpus %>% tf_tokenize(...)
    }

    token_counts <- tokens %>% dplyr::count(token)

    words_to_remove <- words

    common_words <- NULL
    rare_words <- NULL

    if (!is.null(common_word_limit)) {
        if (percentage)
            common_words <- get_common_words_by_percentage(token_counts, common_word_limit, inclusive)
        else
            common_words <- get_common_words_by_count(token_counts, common_word_limit)
    }

    if (!is.null(rare_word_limit)) {
        if (percentage)
            rare_words <- get_rare_words_by_percentage(token_counts, rare_word_limit, inclusive)
        else
            rare_words <- get_rare_words_by_count(token_counts, rare_word_limit)
    }

    words_to_remove <- c(words_to_remove, common_words, rare_words)

    if (is.null(words_to_remove) | length(words_to_remove) == 0) return(corpus)

    .rm_words(corpus, words_to_remove, tokens)
}

#' @export
get_common_words_by_count <- function(token_counts, common_word_limit) {
    common_words <- token_counts %>%
        dplyr::filter(n >= common_word_limit) %>%
        dplyr::select(token)

    common_words$token
}

#' @export
get_common_words_by_percentage <- function(token_counts, common_word_limit, inclusive) {
    count_limit <- ceiling(sum(token_counts$n) * common_word_limit)

    token_cumsum <- token_counts %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::mutate(n=cumsum(n))

    .get_words_by_percentage(token_cumsum, count_limit, inclusive)
}

#' @export
get_rare_words_by_count <- function(token_counts, rare_word_limit) {
    rare_words <- token_counts %>%
        dplyr::filter(n <= rare_word_limit) %>%
        dplyr::mutate(n=NULL)

    rare_words$token
}

#' @export
get_rare_words_by_percentage <- function(token_counts, rare_word_limit, inclusive) {
    count_limit <- ceiling(sum(token_counts$n) * rare_word_limit)

    token_cumsum <- token_counts %>%
        dplyr::arrange(n)
    token_cumsum$n <- cumsum(token_cumsum$n)

    .get_words_by_percentage(token_cumsum, count_limit, inclusive)
}

.get_words_by_percentage <- function(token_cumsum, count_limit, inclusive) {
    token_boundary <- token_cumsum %>%
        dplyr::mutate(b=n <= count_limit)

    boundary <- which.min(token_boundary$b)

    if (boundary == 1) {
        if (inclusive) return(token_boundary$token[1])
        else return(NULL)
    }

    if (inclusive & token_boundary$n[boundary - 1] / count_limit != 1)
        return(token_boundary$token[1:boundary])

    token_boundary$token[1:(boundary - 1)]
}

.rm_words <- function(corpus, words, tokens) {
    words <- dplyr::data_frame(token=words)

    tokens %>%
        dplyr::mutate(row=row_number()) %>%
        dplyr::anti_join(words, by="token") %>%
        dplyr::arrange(row) %>%
        dplyr::mutate(row=NULL) %>%
        tf_merge_tokens(delim=" ")
}
