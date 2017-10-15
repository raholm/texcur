#' @title
#' Creates a word tokenizer.
#'
#' @description
#' Creates a word tokenizer.
#'
#' @importFrom methods new
#'
#' @export
create_word_tokenizer <- function()  {
    module <- get_tokenizer_module()
    class <- module$WordTokenizer
    new(class)
}


#' @title
#' Creates a word index tokenizer.
#'
#' @description
#' Creates a word index tokenizer.
#'
#' @importFrom methods new
#'
#' @export
create_word_index_tokenizer <- function()  {
    module <- get_tokenizer_module()
    class <- module$WordIndexTokenizer
    new(class)
}
