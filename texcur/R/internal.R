#' @keywords internal
.check_corpus <- function(corpus, has_id=FALSE) {
    if (has_id) {
        checkmate::assert_subset(c("id", "text"), names(corpus))
    } else {
        checkmate::assert_subset(c("text"), names(corpus))
    }
}

#' @keywords internal
.check_tokens <- function(tokens, null_ok=FALSE, has_id=FALSE) {
    if (is.null(tokens) & null_ok) return()

    if (has_id) {
        checkmate::assert_subset(c("id", "token"), names(tokens))
    } else {
        checkmate::assert_subset(c("token"), names(tokens))
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
}

#' @keywords internal
.check_incorrect_tokens_input <- function(func, ...) {
    incorrect_object_type <- data.frame()
    expect_error(func(incorrect_object_type, ...))

    incorrect_column_name <- dplyr::data_frame(text=c("hello", "world"))
    expect_error(func(incorrect_column_name, ...))
}


get_tokenizer_module <- function() {
    Rcpp::Module("mod_tokenizer", "texcur")
}
