#include <Rcpp.h>

#include "def.h"
#include "tokenizer.h"

using namespace texcur;

Rcpp::DataFrame wit_transform(WordIndexTokenizer* const tokenizer,
                              const Rcpp::DataFrame& documents) {
  Rcpp::IntegerVector document_ids = documents["id"];
  Rcpp::StringVector texts = documents["text"];
  Matrix<WordIndex> tokens = tokenizer->transform(texts);

  size_t nrows = 0;

  for (auto const& token : tokens)
    nrows += token.size();

  Rcpp::IntegerVector res_doc_ids(nrows);
  Rcpp::IntegerVector res_token_ids(nrows);
  Rcpp::IntegerVector res_tokens(nrows);

  unsigned global_idx = 0;

  for (unsigned doc_id = 0; doc_id < tokens.size(); ++doc_id) {
    auto current_tokens = tokens.at(doc_id);
    auto current_doc_id = document_ids(doc_id);

    for (unsigned token_id = 0; token_id < current_tokens.size(); ++token_id) {
      res_doc_ids(global_idx) = current_doc_id;
      res_token_ids(global_idx) = token_id + 1;
      res_tokens(global_idx) = current_tokens.at(token_id);

      ++global_idx;
    }
  }

  return Rcpp::DataFrame::create(Rcpp::Named("id") = std::move(res_token_ids),
                                 Rcpp::Named("doc") = std::move(res_doc_ids),
                                 Rcpp::Named("token") = std::move(res_tokens));
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
