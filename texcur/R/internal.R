#' @keywords internal
.check_corpus <- function(corpus, has.id=FALSE) {
    checkr::assert_type(corpus, "tbl_df")

    if (has.id) {
        checkr::assert_subset(c("id", "text"), names(corpus))
        checkr::assert_character(corpus$id)
        checkr::assert_character(corpus$text)
    } else {
        checkr::assert_subset("text", names(corpus))
        checkr::assert_character(corpus$text)
    }
}

#' @keywords internal
.check_tokens <- function(tokens, null.ok=FALSE, has.id=FALSE) {
    if (is.null(tokens) & null.ok) return()


    checkr::assert_type(tokens, "tbl_df")

    if (has.id) {
        checkr::assert_subset(c("id", "token"), names(tokens))
        checkr::assert_character(tokens$token)
        checkr::assert_character(tokens$id)
    } else {
        checkr::assert_subset("token", names(tokens))
        checkr::assert_character(tokens$token)
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
.check_incorrect_tokens_input <- function(func, ...) {
    incorrect_object_type <- data.frame()
    expect_error(func(incorrect_object_type, ...))

    incorrect_column_name <- dplyr::data_frame(text=c("hello", "world"))
    expect_error(func(incorrect_column_name, ...))

    incorrect_column_type <- dplyr::data_frame(token=c(123, 321))
    expect_error(func(incorrect_column_type, ...))
}
