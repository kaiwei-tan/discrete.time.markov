% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrete.time.markov.text.R
\name{get.transitions.list.text}
\alias{get.transitions.list.text}
\title{Lists All Transitions from Text Data}
\usage{
get.transitions.list.text(documents, n, punct = "none")
}
\arguments{
\item{documents}{a \emph{string} representing a single document or a \emph{vector} of multiple document strings.}

\item{n}{number of words (excluding punctuation) in a state.}

\item{punct}{how the function deals with punctuation. If \code{'none'}, removes all punctuation from input. If \code{'end'}, only punctuation at the end of the document (if any) are considered as states. If \code{'period'}, all punctuation at the end of the document (if any) are considered as periods.}
}
\value{
A list of all start states and end states.
}
\description{
Helper function for \code{\link{get.transitions.text}} and \code{\link{get.transition.matrix.text}}. Given text data, returns all state transitions in list form.
}
\details{
\strong{This is a helper function. Please instead use \code{\link{get.transitions.text}} to get a list of all state transitions with probabilities/counts, or \code{\link{get.transition.matrix.text}} to get a transition probability matrix.}

  Each state is defined as a a group of \code{n} consecutive words. All end-sentence punctuation, depending on input for the \code{punct} parameter, are also considered as states by themselves.

  Assumes each string (each document) contains one sentence each. Also assumes all sentences are in the format "the quick, brown fox jumps over the lazy dog." (i.e. all lowercase, with a period to mark the end of a sentence). Ignores mid-sentence punctuation. If your input text data is not in this format, please clean it accordingly prior to using this function.

  Currently does not support non-English characters (or emojis).
}
\seealso{
\code{\link{get.transitions.text}}, \code{\link{get.transition.matrix.text}}
}
