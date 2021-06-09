#' @title Lists All Transitions from Data
#'
#' @description Given a series of states, calculates all possible transitions and returns a dataframe of counts or transition probabilities sorted by start state and end state.
#'
#'   This output is useful for subsequently drawing state transition diagrams using \code{\link{igraph::graph_from_data_frame}}.
#'
#' @param states a \emph{vector} with values corresponding to states in a Markov chain model.
#' @param option if \code{0} or \code{"prob"}, returns transition probabilites; if \code{1} or \code{"count"}, returns counts of each transition from the data.
#'
#' @return A dataframe containing either transition probabilities or counts.
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @examples
#' # Generate random sequence of states
#' states <- sample(c('up', 'down', 'same'), 10000, replace=TRUE)
#' # Calculate transition probabilities
#' get.transitions(states, option='prob')
#'
#' @seealso \code{\link{get.transition.matrix}}
get.transitions <- function(states, option='prob') {

  # List of transitions
  transitions_list <- list(start=c(), end=c())
  for (i in 1:(length(states)-1)) {
    transitions_list$start <- c(transitions_list$start, states[i])
    transitions_list$end <- c(transitions_list$end, states[i+1])
  }

  # List of all states
  states_list <- unique(states) %>% sort()

  # Count transitions for each start and end state
  transitions_df <-
    transitions_list %>%
    as.data.frame() %>%
    group_by(start, end) %>%
    summarize(count=n(), .groups='drop') %>%
    arrange(start, end)

  if (option == 1 | option == 'count') {
    return(transitions_df)
  }

  if (option == 0 | option == 'prob') {
    # Count total transitions for each start state
    transitions_df_total <-
      transitions_list %>%
      as.data.frame() %>%
      group_by(start) %>%
      summarize(total=n(), .groups='drop')

    # Compute probability values for each start and end state
    transitions_df <-
      left_join(transitions_df, transitions_df_total, by='start') %>%
      mutate(prob=count/total) %>%
      select(c(start, end, prob))

    return(transitions_df)
  }
}

#' @title Computes Transition Probability Matrix
#'
#' @description Given a series of states, computes the transition probability matrix or returns a matrix of transition counts.
#'
#' @param states a \emph{vector} with values corresponding to states in a Markov chain model.
#' @param option if \code{0} or \code{"prob"}, returns transition probabilites; if \code{1} or \code{"count"}, returns counts of each transition from the data.
#' @param output_type if \code{"matrix"}, outputs a \emph{matrix}; if \code{"df"}, outputs a \emph{dataframe}.
#'
#' @return A matrix or dataframe containing transition probabilities or counts.
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom reshape2 dcast
#'
#' @examples
#' # Generate random sequence of states
#' states <- sample(c('up', 'down', 'same'), 10000, replace=TRUE)
#' # Create transition probability matrix
#' x <- get.transition.matrix(states, option='prob', output_type='matrix')
#'
#' @seealso
#' \code{\link{get.transitions}}, \code{\link{get.steady.state}}
get.transition.matrix <- function(states, option='prob', output_type='matrix') {
  transitions_df <- get.transitions(states, option=option)

  # Get list of states
  states_list <- unique(states)

  # Add in start states that were not accounted for (leads to nowhere, hence absorbing state)
  states_start_missing <- setdiff(states_list, transitions_df$start)
  if (length(states_start_missing) > 0) {
    transitions_df <- rbind(transitions_df,
                            data.frame(start=states_start_missing,
                                       end=states_start_missing,
                                       prob=1))
  }

  # Add in end states that were not accounted for (no state leads to them, hence probability = 0)
  states_end_missing <- setdiff(states_list, transitions_df$end)
  if (length(states_end_missing) > 0) {
    transitions_df <- rbind(transitions_df,
                            data.frame(start=rep(states_end_missing),
                                       end=states_end_missing,
                                       prob=0))
  }

  # Convert data frame from long to wide, hence obtaining our transition matrix
  transition_matrix_df <- dcast(transitions_df, start ~ end, value.var=colnames(transitions_df)[3])

  # Will generate NAs where zeros are supposed to be, hence replace them
  transition_matrix_df[is.na(transition_matrix_df)] <- 0

  if (output_type == 1 | output_type == 'df') {
    return(transition_matrix_df)
  }

  if (output_type == 0 | output_type == 'matrix') {
    transition_matrix <-
      transition_matrix_df %>%
      select(-'start') %>%
      data.matrix()

    rownames(transition_matrix) <- transition_matrix_df$start
    return(transition_matrix)
  }
}

#' @title Computes Steady-state Probabilities
#'
#' @description Given a transition probability matrix, calculates the steady-state (long-run) probabilties of each state.
#'
#' @param transition_prob_matrix a transition probability matrix, which can be generated from data using \code{\link{get.transition.matrix}}. \emph{Must be in matrix data type}.
#'
#' @return A dataframe containing steady-state probabilities for each state.
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @examples
#' # Generate random sequence of states
#' states <- sample(c('up', 'down', 'same'), 10000, replace=TRUE)
#' # Create transition probability matrix
#' x <- get.transition.matrix(states, option='prob', output_type='matrix')
#' # Get steady state probabilities
#' get.steady.state(x)
#'
#' @seealso \code{\link{get.transition.matrix}}
get.steady.state <- function(transition_prob_matrix) {
  n <- nrow(transition_prob_matrix)

  # LHS
  LHS <- t(transition_prob_matrix)
  rownames(LHS) <- NULL
  colnames(LHS) <- NULL
  for (i in 1:n) {
    LHS[i,i] = LHS[i,i] - 1
  }
  LHS <- rbind(LHS, matrix(rep(1, n), nrow=1))

  # RHS
  RHS <- matrix(c(rep(0, n), 1), ncol=1)

  # Solve and present results in data frame
  if (is.null(rownames(transition_prob_matrix))) {
    states = seq(1, n)
  } else {
    states = rownames(transition_prob_matrix)
  }

  result <-
    data.frame(state = states,
               prob = qr.solve(LHS, RHS) %>% as.vector())

  return(result)
}
