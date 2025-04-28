#' lstm_model Class
#'
#' A custom Long Short-Term Memory (LSTM) neural network module implemented
#' using `torch` for time series or sequential data prediction.
#'
#' @description
#' This module defines an LSTM model followed by a dropout layer and a fully
#' connected (linear) output layer. It is designed to take in sequential input
#' data and output predictions, typically used for regression or forecasting
#' tasks.
#'
#' @details
#' - An LSTM layer processes the input sequences.
#' - A dropout layer randomly zeroes some elements of the tensor during
#'   training, helping prevent overfitting.
#' - A fully connected (linear) layer maps the hidden states to the output.
#'
#' @section Forward Pass Details:
#' - Initializes hidden state (`h0`) and cell state (`c0`) as zeros.
#' - Passes the input `x` and initial states through the LSTM layer.
#' - Takes the output from the last time step.
#' - Applies dropout regularization.
#' - Passes through a fully connected layer to produce the output.
#'
#' @param input_dim (int) Number of features in the input at each time step.
#' @param hidden_dim (int, default = 32) Number of features in the hidden state.
#' @param output_dim (int, default = 1) Dimension of the final output.
#' @param dropout (numeric, default = 0.2) Dropout probability after the LSTM.
#'
#' @return
#' An `nn_module` representing the LSTM model. The `forward()` method takes a
#' tensor `x` of shape `(batch_size, sequence_length, input_dim)` and returns a
#' tensor of shape `(batch_size, output_dim)`.
#'
#' @examples
#' \dontrun{
#' library(torch)
#'
#' model <- lstm_model(input_dim = 10, hidden_dim = 32, output_dim = 1,
#'                     dropout = 0.2)
#'
#' # Simulate a batch of 5 sequences, each with 7 time steps and 10 features
#' x <- torch_randn(5, 7, 10)
#'
#' # Get model output
#' y_pred <- model(x)
#' print(y_pred$shape)  # Should be (5, 1)
#' }
#' @export


lstm_model <- torch::nn_module(
  "lstm_model",
  initialize = function(input_dim, hidden_dim = 32, output_dim = 1,
                        dropout = 0.2) {
    self$hidden_dim <- hidden_dim
    self$lstm <- nn_lstm(input_dim, hidden_dim, batch_first = TRUE)
    self$dropout <- nn_dropout(dropout)
    self$fc <- nn_linear(hidden_dim, output_dim)
  },

  forward = function(x) {
    # Initialize hidden state with zeros
    batch_size <- x$size(1)
    h0 <- torch_zeros(1, batch_size, self$hidden_dim)
    c0 <- torch_zeros(1, batch_size, self$hidden_dim)

    # Forward propagate LSTM
    lstm_out <- self$lstm(x, list(h0, c0))

    # Extract the output from the lstm
    out <- lstm_out[[1]]

    # Get the last time step output
    n <- out$size(2)
    out <- out[, n, ]

    # Apply dropout
    out <- self$dropout(out)

    # Apply the fully connected layer
    out <- self$fc(out)

    out
  }
)
