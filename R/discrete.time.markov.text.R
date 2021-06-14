#' @title Lists All Transitions from Text Data
#'
#' @description Helper function for \code{\link{get.transitions.text}} and \code{\link{get.transition.matrix.text}}. Given text data, returns all state transitions in list form.
#'
#' @details \emph{This is a helper function. Please instead use \code{\link{get.transitions.text}} to get a list of all state transitions with probabilities/counts, or \code{\link{get.transition.matrix.text}} to get a transition probability matrix.}
#'
#'   Each state is defined as a a group of \code{n} consecutive words. All end-sentence punctuation, depending on input for the \code{punct} parameter, are also considered as states by themselves.
#'
#'   Assumes each string (each document) contains one sentence each. Also assumes all sentences are in the format "the quick, brown fox jumps over the lazy dog." (i.e. all lowercase, with a period to mark the end of a sentence). Ignores mid-sentence punctuation. If your input text data is not in this format, please clean it accordingly prior to using this function.
#'
#'   Currently does not support non-English characters (or emojis).
#'
#' @param documents a \emph{string} representing a single document or a \emph{vector} of multiple document strings.
#' @param n number of words (excluding punctuation) in a state.
#' @param punct how the function deals with punctuation. If \code{'none'}, removes all punctuation from input. If \code{'end'}, only punctuation at the end of the document (if any) are considered as states. If \code{'period'}, all punctuation at the end of the document (if any) are considered as periods.
#'
#' @return A list of all start states and end states.
#' @export
#'
#' @seealso \code{\link{get.transitions.text}}, \code{\link{get.transition.matrix.text}}
get.transitions.list.text <- function(documents, n, punct='none') {

  if (punct == 'none') {
    documents_states <-
      documents %>%
      gsub("[^[:alnum:][:space:]'’]", ' ', ., perl=TRUE) %>%
      str_squish() %>%
      strsplit(' ')

  }

  else if (punct == 'end' | punct == 'period') {

    # Create function to separate any punctuation used to end a sentence/phrase
    split.end.punctuation <- function(document) {
      return(list(end_punct = gsub('.*(?=[[:punct:]]$)', '', document, perl=TRUE),
                  remainder = gsub('[[:punct:]]$', '', document, perl=TRUE)))
    }

    documents_states <- c()
    end_punct <- c()

    for (i in 1:length(documents)) {
      document_split <- documents[i] %>% split.end.punctuation()
      documents_states <- c(documents_states, document_split$remainder)

      if (punct == 'period') {
        end_punct <- c(end_punct, '.')
      }

      else {
        end_punct <- c(end_punct, document_split$end_punct)
      }
    }

    if (punct == 'end') {
      documents_states <-
        documents_states %>%
        gsub("[^[:alnum:][:space:]'’](?!$)", ' ', ., perl=TRUE) %>%
        gsub("(?=[^[:alnum:][:space:]'’])", ' \\1', ., perl=TRUE) %>%
        str_squish() %>%
        strsplit(' ')
    }

    else if (punct == 'period') {
      documents_states <-
        documents_states %>%
        gsub("[^[:alnum:][:space:]'’](?!$)", ' ', ., perl=TRUE) %>%
        gsub("(?<=.)[^[:alnum:][:space:]'’]$", '.', ., perl=TRUE) %>%
        gsub("(?=[^[:alnum:][:space:]'’])", ' \\1', ., perl=TRUE) %>%
        str_squish() %>%
        strsplit(' ')
    }
  }

  else {
    stop('Input for punct not recognized')
  }

  # List of transitions
  transitions_list <- list(start_state=c(), end_state=c())

  for (i in 1:length(documents_states)) {
    temp <- documents_states[[i]]
    k <- length(temp)
    transitions_count = k - n

    # State-to-state transitions
    if (transitions_count > 0) {
      for (j in 1:transitions_count) {
        transitions_list$start_state <-
          c(transitions_list$start_state, paste(temp[j:(j+n-1)], collapse=' '))
        transitions_list$end_state <-
          c(transitions_list$end_state, paste(temp[(j+1):(j+n)], collapse=' '))
      }
    }

    # Last state for each document, which serves as an absorbing state
    if (transitions_count >= 0) {
      if (punct == 'end' | punct == 'period') {
        transitions_list$start_state <-
          c(transitions_list$start_state, paste(temp[(k-n+1):k], collapse=' '), end_punct[i])
        transitions_list$end_state <-
          c(transitions_list$end_state, end_punct[i], end_punct[i])
      }
    }
  }

  return(transitions_list)
}

#' @title Lists All Transitions from Text Data
#'
#' @description Given text data, calculates all possible transitions between pre-defined states and returns a dataframe of counts or transition probabilities sorted by start state and end state.
#'
#' @details Each state is defined as a a group of \code{n} consecutive words. All end-sentence punctuation, depending on input for the \code{punct} parameter, are also considered as states by themselves.
#'
#'   Assumes each string (each document) contains one sentence each. Also assumes all sentences are in the format "the quick, brown fox jumps over the lazy dog." (i.e. all lowercase, with a period to mark the end of a sentence). Ignores mid-sentence punctuation. If your input text data is not in this format, please clean it accordingly prior to using this function.
#'
#'   Currently does not support non-English characters (or emojis).
#'
#'   The output for this function is useful for subsequently drawing state transition diagrams using \code{\link{igraph::graph_from_data_frame}}.
#'
#'   To get a transition probability matrix instead, use \code{\link{get.transition.matrix.text}}.
#'
#' @param documents a \emph{string} representing a single document or a \emph{vector} of multiple document strings.
#' @param n number of words (excluding punctuation) in a state.
#' @param option if \code{0} or \code{"prob"}, returns transition probabilities; if \code{1} or \code{"count"}, returns counts of each transition from the data.
#' @param punct how the function deals with punctuation. If \code{'none'}, removes all punctuation from input. If \code{'end'}, only punctuation at the end of the document (if any) are considered as states. If \code{'period'}, all punctuation at the end of the document (if any) are considered as periods.
#'
#' @return A dataframe containing either transition probabilities or counts.
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stringr str_squish
#'
#' @seealso \code{\link{get.transitions}} for the general, non-text version.
get.transitions.text <- function(documents, n, option='prob', punct='none') {

  # List of all transitions
  transitions_list <- get.transitions.list.text(documents=documents, n=n, punct=punct)

  # Count transitions for each start and end state
  transitions_df <-
    transitions_list %>%
    as.data.frame() %>%
    group_by(start_state, end_state) %>%
    summarize(count=n(), .groups='drop') %>%
    arrange(start_state, end_state)

  if (option == 1 | option == 'count') {
    return(transitions_df)
  }

  if (option == 0 | option == 'prob') {
    # Count total transitions for each start state
    transitions_df_total <-
      transitions_list %>%
      as.data.frame() %>%
      group_by(start_state) %>%
      summarize(total=n(), .groups='drop')

    # Compute probability values for each start and end state
    transitions_df <-
      left_join(transitions_df, transitions_df_total, by='start_state') %>%
      mutate(prob=count/total) %>%
      select(c(start_state, end_state, prob))

    return(transitions_df)
  }
}

#' @title Computes Transition Probability Matrix for Text Data
#'
#' @description Given text data, computes the transition probability matrix between pre-defined states or returns a matrix of transition counts.
#'
#' @details Each state is defined as a a group of \code{n} consecutive words. All end-sentence punctuation, depending on input for the \code{punct} parameter, are also considered as states by themselves.
#'
#'   Assumes each string (each document) contains one sentence each. Also assumes all sentences are in the format "the quick, brown fox jumps over the lazy dog." (i.e. all lowercase, with a period to mark the end of a sentence). Ignores mid-sentence punctuation. If your input text data is not in this format, please clean it accordingly prior to using this function.
#'
#'   Currently does not support non-English characters (or emojis).
#'
#'   Use \code{\link{get.steady.state}} on the output of this function to compute steady-state / long-run probabilities for each word/phrase.
#'
#' @param documents a \emph{string} representing a single document or a \emph{vector} of multiple document strings.
#' @param n number of words (excluding punctuation) in a state.
#' @param option if \code{0} or \code{"prob"}, returns transition probabilities; if \code{1} or \code{"count"}, returns counts of each transition from the data.
#' @param output_type if \code{"matrix"}, outputs a \emph{matrix}; if \code{"df"}, outputs a \emph{dataframe}.
#' @param punct how the function deals with punctuation. If \code{'none'}, removes all punctuation from input. If \code{'end'}, only punctuation at the end of the document (if any) are considered as states. If \code{'period'}, all punctuation at the end of the document (if any) are considered as periods.
#'
#' @return A matrix or dataframe containing transition probabilities or counts.
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom reshape2 dcast
#'
#' @seealso \code{\link{get.transition.matrix}} for the general, non-text version.
get.transition.matrix.text <- function(documents, n, option='prob', output_type='matrix', punct='none') {

  transitions_list <- get.transitions.list.text(documents=documents, n=n, punct=punct)
  transitions_df <- get.transitions.text(documents=documents, n=n, option=option, punct=punct)

  # List of all states
  states_list <-
    unique(c(transitions_list$start_state, transitions_list$end_state))

  # Add in start states that were not accounted for (leads to nowhere, hence absorbing state)
  states_start_missing <- setdiff(states_list, transitions_df$start_state)
  if (length(states_start_missing) > 0) {
    transitions_df <- rbind(transitions_df,
                            data.frame(start_state=states_start_missing,
                                       end_state=states_start_missing,
                                       prob=1))
  }

  # Add in end states that were not accounted for (no state leads to them, hence probability = 0)
  states_end_missing <- setdiff(states_list, transitions_df$end_state)
  if (length(states_end_missing) > 0) {
    transitions_df <- rbind(transitions_df,
                            data.frame(start_state=rep(states_end_missing),
                                       end_state=states_end_missing,
                                       prob=0))
  }

  # Convert data frame from long to wide, hence obtaining our transition matrix
  transition_matrix_df <- dcast(transitions_df, start_state ~ end_state, value.var=colnames(transitions_df)[3])

  # Will generate NAs where zeros are supposed to be, hence replace them
  transition_matrix_df[is.na(transition_matrix_df)] <- 0

  if (output_type == 1 | output_type == 'df') {
    return(transition_matrix_df)
  }

  if (output_type == 0 | output_type == 'matrix') {
    transition_matrix <-
      transition_matrix_df %>%
      select(-'start_state') %>%
      data.matrix()

    rownames(transition_matrix) <- transition_matrix_df$start_state
    return(transition_matrix)
  }
}
