#' @keywords internal
.check_corpus <- function(corpus, has_id=FALSE) {
    if (has_id) {
        checkr::assert_tidy_table(corpus, c("id", "text"))
        checkr::assert_character(corpus$id)
        checkr::assert_character(corpus$text)
    } else {
        checkr::assert_tidy_table(corpus, "text")
        checkr::assert_character(corpus$text)
    }
}

#' @keywords internal
.check_tokens <- function(tokens, null_ok=FALSE, has_id=FALSE) {
    if (is.null(tokens) & null_ok) return()

    if (has_id) {
        checkr::assert_tidy_table(tokens, c("id", "token"))
        ## checkr::assert_character(tokens$id)
        ## checkr::assert_character(tokens$token)
    } else {
        checkr::assert_tidy_table(tokens, "token")
        ## checkr::assert_character(tokens$token)
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


get_tokenizer_module <- function() {
    Rcpp::Module("mod_tokenizer", "texcur")
}
