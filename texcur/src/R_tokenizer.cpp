#include <Rcpp.h>

#include "def.h"
#include "tokenizer.h"

using namespace texcur;

Rcpp::IntegerMatrix wit_transform(WordIndexTokenizer* const tokenizer,
                              const Rcpp::DataFrame& documents) {
  Rcpp::IntegerVector document_ids = documents["id"];
  Rcpp::StringVector texts = documents["text"];
  Matrix<WordIndex> tokens = tokenizer->transform(texts);

  size_t nrows = 0;

  for (auto const& token : tokens)
    nrows += token.size();

  // First column is the token id
  // Second column is the doc id
  // third column is the token
  Rcpp::IntegerMatrix result(nrows, 3);

  unsigned global_idx = 0;

  for (unsigned doc_id = 0; doc_id < tokens.size(); ++doc_id) {
    auto current_tokens = tokens.at(doc_id);
    auto current_doc_id = document_ids(doc_id);

    for (unsigned token_id = 0; token_id < current_tokens.size(); ++token_id) {
      result(global_idx, 1) = token_id + 1;
      result(global_idx, 2) = current_doc_id;
      result(global_idx, 3) = current_tokens.at(token_id);

      ++global_idx;
    }
  }

  return result;
}

Rcpp::StringVector wit_revert(WordIndexTokenizer* const tokenizer,
                              const Rcpp::IntegerVector& tokens) {
  auto transformer = tokenizer->get_transformer();
  auto words = transformer.revert(Rcpp::as<WordIndexVector>(tokens));
  return words;
}

RCPP_MODULE(mod_tokenizer) {
  Rcpp::class_<WordIndexTokenizer>("WordIndexTokenizer")
    .default_constructor()
    .method("transform", &wit_transform)
    .method("revert", &wit_revert)
    ;
}
