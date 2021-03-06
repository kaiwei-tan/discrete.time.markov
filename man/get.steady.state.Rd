% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrete.time.markov.R
\name{get.steady.state}
\alias{get.steady.state}
\title{Computes Steady-state Probabilities}
\usage{
get.steady.state(transition_prob_matrix)
}
\arguments{
\item{transition_prob_matrix}{a transition probability matrix, which can be generated from data using \code{\link{get.transition.matrix}}. \emph{Must be in matrix data type}.}
}
\value{
A dataframe containing steady-state probabilities for each state.
}
\description{
Given a transition probability matrix, calculates the steady-state (long-run) probabilties of each state.
}
\examples{
# Generate random sequence of states
states <- sample(c('up', 'down', 'same'), 10000, replace=TRUE)
# Create transition probability matrix
x <- get.transition.matrix(states, option='prob', output_type='matrix')
# Get steady state probabilities
get.steady.state(x)

}
\seealso{
\code{\link{get.transition.matrix}}
}
