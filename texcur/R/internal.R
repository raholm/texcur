#' @keywords internal
.check_corpus <- function(corpus) {
    checkr::assert_type(corpus, "tbl_df")
    checkr::assert_subset("text", names(corpus))
    checkr::assert_character(corpus$text)
}

#' @keywords internal
.check_tokenized_corpus <- function(tokenized_corpus, null.ok=FALSE, has.id=FALSE) {
    if (is.null(tokenized_corpus) & null.ok) return()


    checkr::assert_type(tokenized_corpus, "tbl_df")

    if (has.id) {
        checkr::assert_subset(c("id", "token"), names(tokenized_corpus))
        checkr::assert_character(tokenized_corpus$token)
        checkr::assert_character(tokenized_corpus$id)
    } else {
        checkr::assert_subset("token", names(tokenized_corpus))
        checkr::assert_character(tokenized_corpus$token)
    }
}

#' @keywords internal
.apply <- function(corpus, func) {
    corpus %>% dplyr::mutate(text=func(text))
}

#' @keywords internal
.check_incorrect_corpus_input <- function(func, ...) {
    incorrect_object_type <- data.frame()
    expect_error(func(incorrect_object_type, ...))

    incorrect_column_name <- dplyr::data_frame(body=c("hello", "world"))
    expect_error(func(incorrect_column_name, ...))

    incorrect_column_type <- dplyr::data_frame(text=c(123, 321))
    expect_error(func(incorrect_column_type, ...))
}

#' @keywords internal
.check_incorrect_tokenized_corpus_input <- function(func, ...) {
    incorrect_object_type <- data.frame()
    expect_error(func(incorrect_object_type, ...))

    incorrect_column_name <- dplyr::data_frame(text=c("hello", "world"))
    expect_error(func(incorrect_column_name, ...))

    incorrect_column_type <- dplyr::data_frame(token=c(123, 321))
    expect_error(func(incorrect_column_type, ...))
}
