{
  "nbformat": 4,
  "nbformat_minor": 0,
  "metadata": {
    "colab": {
      "provenance": [],
      "authorship_tag": "ABX9TyPCEiG112jX0IXLqWdm2GM8",
      "include_colab_link": true
    },
    "kernelspec": {
      "name": "ir",
      "display_name": "R"
    },
    "language_info": {
      "name": "R"
    }
  },
  "cells": [
    {
      "cell_type": "markdown",
      "metadata": {
        "id": "view-in-github",
        "colab_type": "text"
      },
      "source": [
        "<a href=\"https://colab.research.google.com/github/Ryunoshin3150/test/blob/main/example.R\" target=\"_parent\"><img src=\"https://colab.research.google.com/assets/colab-badge.svg\" alt=\"Open In Colab\"/></a>"
      ]
    },
    {
      "cell_type": "code",
      "metadata": {
        "colab": {
          "base_uri": "https://localhost:8080/"
        },
        "id": "d66ab815",
        "outputId": "5aeae307-6bed-4f08-ac28-3e70b851c435"
      },
      "source": [
        "if (!requireNamespace(\"plspm\", quietly = TRUE)) {\n",
        "  install.packages(\"plspm\")\n",
        "}\n",
        "library(plspm)\n",
        "print(\"plspm package installation and loading attempted.\")"
      ],
      "execution_count": 41,
      "outputs": [
        {
          "output_type": "stream",
          "name": "stdout",
          "text": [
            "[1] \"plspm package installation and loading attempted.\"\n"
          ]
        }
      ]
    },
    {
      "cell_type": "code",
      "metadata": {
        "colab": {
          "base_uri": "https://localhost:8080/"
        },
        "id": "305c68ec",
        "outputId": "06af708a-598e-43f9-cfcf-487234577021"
      },
      "source": [
        "library(MASS)\n",
        "library(plspm)\n",
        "\n",
        "# Redefine necessary variables (as they might be reset between code blocks)\n",
        "SAMPLE <- 100\n",
        "MODEL <- \"\\nA =~ x1 + x2 + x3\\nB =~ x4 + x5 + x6\\nB ~ A\\n\"\n",
        "\n",
        "Lambda <- (diag(2) * 0.7)[rep(1:2, each = 3), ]\n",
        "Psi <- matrix(c(1, 0.3, 0.3, 1), 2, 2)\n",
        "Sigma <- Lambda %*% Psi %*% t(Lambda)\n",
        "diag(Sigma) <- 1\n",
        "rownames(Sigma) <- colnames(Sigma) <- paste(\"x\", 1:6, sep = \"\")\n",
        "\n",
        "# Ensure `blocks`, `paths`, `modes`, `scheme`, `tol` are defined\n",
        "blocks <- list(\n",
        "  A = c(\"x1\", \"x2\", \"x3\"),\n",
        "  B = c(\"x4\", \"x5\", \"x6\")\n",
        ")\n",
        "\n",
        "paths <- matrix(c(\n",
        "  0, 0,\n",
        "  1, 0\n",
        "), nrow = 2, byrow = TRUE)\n",
        "rownames(paths) <- colnames(paths) <- c(\"A\", \"B\")\n",
        "\n",
        "modes <- c(\"A\", \"A\")\n",
        "scheme <- \"path\"\n",
        "tol <- 1e-06\n",
        "\n",
        "# Generate datasets (reduced to 100 for quick test)\n",
        "cat(\"\\nGenerating 100 datasets for plspm simulation...\\n\")\n",
        "\n",
        "# For plspm, mclapply might not be directly applicable with its structure for simulation.\n",
        "# We will use a standard loop to simulate `matrixpls.sim` behavior.\n",
        "\n",
        "num_datasets <- 100\n",
        "plspm_sim_results <- list()\n",
        "\n",
        "for (i in 1:num_datasets) {\n",
        "  # Generate data for each simulation\n",
        "  data_sim <- mvrnorm(SAMPLE, rep(0, 6), Sigma)\n",
        "  colnames(data_sim) <- paste0(\"x\", 1:6)\n",
        "  data_sim_df <- as.data.frame(data_sim)\n",
        "\n",
        "  # Run plspm\n",
        "  res <- tryCatch({\n",
        "    plspm(data_sim_df, path_matrix = paths, blocks = blocks, modes = modes, scheme = scheme, tol = tol)\n",
        "  }, error = function(e) {\n",
        "    NULL # Return NULL if plspm fails for a dataset\n",
        "  })\n",
        "\n",
        "  if (!is.null(res)) {\n",
        "    # Extract relevant coefficients. The primary coefficient to compare is B ~ A.\n",
        "    # In plspm, this is found in inner_model$B$Estimate for the 'A' predictor.\n",
        "    plspm_sim_results[[i]] <- res$inner_model$B[\"A\", \"Estimate\"]\n",
        "  } else {\n",
        "    plspm_sim_results[[i]] <- NA # Store NA if simulation failed\n",
        "  }\n",
        "}\n",
        "\n",
        "# Clean and convert results to a numeric vector\n",
        "plspm_estimates <- unlist(plspm_sim_results)\n",
        "plspm_estimates <- plspm_estimates[!is.na(plspm_estimates)]\n",
        "\n",
        "cat(\"\\n=== PLSPM Simulation Results ===\\n\")\n",
        "cat(\"Mean estimate (B ~ A):\", mean(plspm_estimates), \"\\n\")\n",
        "cat(\"SD estimate (B ~ A):\", sd(plspm_estimates), \"\\n\")\n",
        "cat(paste0(\"Number of successful simulations: \", length(plspm_estimates), \"/\", num_datasets, \"\\n\"))\n",
        "\n",
        "# Plot results (if enough successful simulations)\n",
        "if (length(plspm_estimates) > 1) {\n",
        "  png(\"plspm_simulation_results.png\", width = 800, height = 600)\n",
        "  plot(density(plspm_estimates), xlim = c(-0.5, 1),\n",
        "       main = \"PLS-SEM Simulation Results (plspm)\",\n",
        "       xlab = \"Estimate (B ~ A)\", ylab = \"Density\")\n",
        "  dev.off()\n",
        "  cat(\"\\nPlot saved to: plspm_simulation_results.png\\n\")\n",
        "}\n",
        "\n",
        "cat(\"\\n\\u2713 PLS-SEM simulation with plspm completed successfully!\\n\")\n"
      ],
      "execution_count": 42,
      "outputs": [
        {
          "output_type": "stream",
          "name": "stdout",
          "text": [
            "\n",
            "Generating 100 datasets for plspm simulation...\n",
            "\n",
            "=== PLSPM Simulation Results ===\n",
            "Mean estimate (B ~ A): 0.2584302 \n",
            "SD estimate (B ~ A): 0.08574251 \n",
            "Number of successful simulations: 100/100\n",
            "\n",
            "Plot saved to: plspm_simulation_results.png\n",
            "\n",
            "✓ PLS-SEM simulation with plspm completed successfully!\n"
          ]
        }
      ]
    }
  ]
}