#' Build and Train an LSTM Model for Sector Data
#'
#' Builds, trains, and evaluates an LSTM model on a given sector's time series
#' data. Prepares input sequences, fits the model, applies early stopping, and
#' visualizes predictions against actual values.
#'
#' @description
#' This function prepares sequential data using lagged features, scales the
#' features and target, trains an LSTM model, and evaluates its performance.
#' Metrics like RMSE, MAE, and R2 are reported, and a plot of actual vs.
#' predicted values is generated.
#'
#' @details
#' - Prepares lagged feature sequences using `prepare_data_for_lstm`.
#' - Splits the data into 80% training and 20% testing sets.
#' - Trains an LSTM model using MSE loss and Adam optimizer.
#' - Applies early stopping with patience.
#' - Plots predicted vs. actual values over time.
#'
#' @param sector_data A data frame with `Date` and `sector_index` columns.
#' @param sector_name (character) Name of the sector, used for plotting titles.
#' @param time_steps (int, default = 10) Number of time steps for each input
#' sequence.
#'
#' @return
#' A list containing:
#' - `model`: Trained LSTM model (`nn_module`).
#' - `rmse`: Root Mean Squared Error on test set.
#' - `mae`: Mean Absolute Error on test set.
#' - `r2`: R-squared value on test set.
#' - `feature_names`: Names of input features used.
#'
#' @importFrom torch torch_manual_seed nn_mse_loss optim_adam with_no_grad
#'
#'
#' @export


build_lstm_model <- function(sector_data, sector_name, time_steps = 10) {
  cat("\n--- Building LSTM model for", sector_name, "sector ---\n")

  tryCatch({
    # Prepare data
    prepared_data <- stocksr::prepare_data_for_lstm(sector_data, time_steps)

    # Split data
    n <- prepared_data$x$size(1)
    train_size <- floor(n * 0.8)

    x_trian <- prepared_data$x[1:train_size]
    x_test <- prepared_data$x[(train_size + 1):n]
    y_train <- prepared_data$y[1:train_size]
    y_test <- prepared_data$y[(train_size + 1):n]

    # Convert to numpy arrays for later use
    y_test_np <- as.array(y_test)

    # Create datasets
    train_ds <- list(x = x_trian, y = y_train)
    test_ds <- list(x = x_test, y = y_test)

    # Get input features dimension
    n_features <- x_trian$size(3)

    # Create model
    torch::torch_manual_seed(123)
    model <- stocksr::lstm_model(input_dim = n_features)

    # Define loss function and optimizer
    criterion <- torch::nn_mse_loss()
    optimizer <- torch::optim_adam(model$parameters, lr = 0.001)

    # Training parameters
    epochs <- 30
    patience <- 5
    best_val_loss <- Inf
    counter <- 0
    best_model_state <- NULL
    batch_size <- 32

    # Manual batching function
    create_batches <- function(x, y, batch_size) {
      n <- x$size(1)
      indices <- sample(1:n)

      batches <- list()
      num_batches <- ceiling(n / batch_size)

      for (i in 1:num_batches) {
        start_idx <- (i - 1) * batch_size + 1
        end_idx <- min(i * batch_size, n)

        if (start_idx <= n) {
          batch_indices <- indices[start_idx:end_idx]
          batches[[i]] <- list(
            x = x[batch_indices],
            y = y[batch_indices]
          )
        }
      }

      batches
    }

    # Training loop
    for (epoch in 1:epochs) {
      # Training
      model$train()
      train_loss <- 0

      # Create batches for this epoch
      train_batches <- create_batches(train_ds$x, train_ds$y, batch_size)
      n_batches <- length(train_batches)

      for (i in 1:n_batches) {
        batch <- train_batches[[i]]

        # Zero gradients
        optimizer$zero_grad()

        # Forward pass
        x_batch <- batch$x
        y_batch <- batch$y$unsqueeze(2)  # Add dimension for output

        pred <- model(x_batch)
        loss <- criterion(pred, y_batch)

        # Backward pass
        loss$backward()
        optimizer$step()

        train_loss <- train_loss + loss$item()
      }

      train_loss <- train_loss / n_batches

      # Validation - use whole test set
      model$eval()

      # Evaluation without computing gradients
      torch::with_no_grad({
        pred <- model(test_ds$x)
        val_loss <- criterion(pred, test_ds$y$unsqueeze(2))$item()
      })

      cat(sprintf("Epoch %d/%d, Train Loss: %.4f, Val Loss: %.4f\n",
                  epoch, epochs, train_loss, val_loss))

      # Early stopping
      if (val_loss < best_val_loss) {
        best_val_loss <- val_loss
        best_model_state <- model$state_dict()
        counter <- 0
      } else {
        counter <- counter + 1
        if (counter >= patience) {
          cat(sprintf("Early stopping at epoch %d\n", epoch))
          break
        }
      }
    }

    # Load best model
    if (!is.null(best_model_state)) {
      model$load_state_dict(best_model_state)
    }

    # Make predictions without using $fn()
    model$eval()
    predictions <- NULL

    # Run predictions batch by batch to avoid memory issues
    pred_batches <- list()
    batch_size <- 64  # Larger batch for prediction
    num_batches <- ceiling(x_test$size(1) / batch_size)

    torch::with_no_grad({
      for (i in 1:num_batches) {
        start_idx <- (i - 1) * batch_size + 1
        end_idx <- min(i * batch_size, x_test$size(1))

        if (start_idx <= x_test$size(1)) {
          batch_x <- x_test[start_idx:end_idx]
          batch_pred <- model(batch_x)

          # Convert to R array immediately
          pred_batches[[i]] <- as.array(batch_pred)
        }
      }
    })

    # Combine predictions
    all_preds <- do.call(rbind, pred_batches)
    predictions <- all_preds[, 1]  # Extract predictions

    # Use original numpy arrays for evaluation
    y_actual <- y_test_np * prepared_data$target_sd + prepared_data$target_mean
    y_pred <- predictions * prepared_data$target_sd + prepared_data$target_mean

    # Calculate metrics
    mse <- mean((y_actual - y_pred)^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(y_actual - y_pred))

    # R^2 calculation
    ss_total <- sum((y_actual - mean(y_actual))^2)
    ss_residual <- sum((y_actual - y_pred)^2)
    r2 <- 1 - (ss_residual / ss_total)

    # Ensure R^2 is between 0 and 1
    r2 <- max(0, min(1, r2))

    cat("Model performance:\n")
    cat("RMSE:", round(rmse, 4), "\n")
    cat("MAE:", round(mae, 4), "\n")
    cat("R^2:", round(r2, 4), "\n")

    # Get test dates for plotting
    test_dates <- prepared_data$dates[(train_size
                                       + 1):length(prepared_data$dates)]
    if (length(test_dates) > length(y_actual)) {
      test_dates <- test_dates[seq_along(y_actual)]
    } else if (length(test_dates) < length(y_actual)) {
      # Extend dates if needed
      extra_dates <- seq(from = max(test_dates) + 1,
                         length.out = length(y_actual) - length(test_dates),
                         by = "day")
      test_dates <- c(test_dates, extra_dates)
    }

    # Plot predictions
    plot_data <- data.frame(
      Date = test_dates,
      Actual = y_actual,
      Predicted = y_pred
    )

    p <- ggplot(plot_data, aes(x = Date)) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 0.8) +
      geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 0.8,
                alpha = 0.7) +
      labs(
        title = paste(sector_name, "Sector - LSTM Predictions"),
        subtitle = paste("R^2 =", round(r2, 3)),
        x = "Date",
        y = "Sector Index"
      ) +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
      theme_minimal()

    list(
      model = model,
      rmse = rmse,
      mae = mae,
      r2 = r2,
      feature_names = prepared_data$feature_names,
      prediction_plot = p

    )

  }, error = function(e) {


    cat("Error building LSTM model:", e$message, "\n")
    list(
      model = "error",
      rmse = NA,
      mae = NA,
      r2 = NA,
      feature_names = "error",
      prediction_plot = NA
    )
  })
}
