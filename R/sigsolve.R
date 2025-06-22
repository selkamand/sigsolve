#' Signature fitting
#'
#' Identify the most likely combination of known mutational signatures
#' which explain the mutational profile observed in a sample of interest.
#'
#' @param catalogue catalogue of mutations observed in your sample. A named vector where names are channel and values are counts.
#' @param signatures a channel X signature matrix where values represent fractional contributions of each channels.
#' @param method how should fitting be performed.
#' \strong{nnls}: Use non-negative least squares method
#'
#' @return a named vector with 1 element per signature. Values indicate the estimated number of mutation drawn from that signature.
#' @export
#'
#' @examples
#' observed_counts <- c(
#' "C>A" = 183,
#' "C>G" = 779,
#' "C>T" = 588,
#' "T>A" = 706,
#' "T>C" = 384,
#' "T>G" = 127
#' )
#'
#' signatures <- simulate_signature_matrix(
#'   signatures = c("sig1", "sig2", "sig3", "sig4"),
#'   channels = c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G"),
#'   seed=1
#' )
#'
#'
#' # fitting using nnls method
#' sig_solve(observed_counts, signatures, method = "nnls")
#'
sig_solve <- function(catalogue, signatures, method = "nnls"){
  channels <- names(catalogue)
  channels_signatures <- rownames(signatures)
  signature_names <- colnames(signatures)

  # Assertions
  if(is.null(channels))
    stop("catalogue must be a named vector")

  if(any(nchar(channels) == 0))
    stop("All elements in catalogue vector must be named")

  if(!is.matrix(signatures))
    stop("signatures must be represented as a matrix")

  if(!is.numeric(signatures))
    stop("signatures must be represented as a numeric matrix, not ", as.character(class(signatures)))

  if(is.null(channels_signatures))
    stop("signature matrix must contain rownames describing the channels")

  if(is.null(signature_names))
    stop("signature matrix must contain column names describing the signature names")

  if(!setequal(channels, channels_signatures)){
    unique_to_cat <- setdiff(channels, channels_signatures)
    unique_to_sigs <- setdiff(channels_signatures, channels)
    stop(
    "Channels described cataloge and signature dataframe must be identical.\n",
    "Channels unique to catalogue: [", paste0(unique_to_cat, collapse=", "), "]\n",
    "Channels unique to signature: [", paste0(unique_to_sigs, collapse=", "), "]",
    )
  }

  #Check order is identical
  if(!identical(channels, channels_signatures)){
    stop("Order of channels in cataloge and signature collection must be identical")
  }

  # From here we can assume inputs are valid, named, with channels sorted in the identical order
  if(method == "nnls"){
   rlang::check_installed(pkg = "nnls", reason = paste0("to fit signatures to data when method = ", method))
    nnls <- nnls::nnls(A = signatures, b = catalogue)
    x_nnls <- nnls$x
  }
  else{
   stop("No implementation for method = [", method, "]")
  }
}

#' Simulate a Signature Matrix
#'
#' This function simulates a matrix where each column corresponds to a signature, and each entry in the matrix is a random proportion of a set of channels. The proportions in each column sum to 1.
#'
#' @param signatures A vector of signature names to be used as column names in the resulting matrix.
#' @param channels A vector of channel names to be used as row names in the resulting matrix.
#' @param digits Integer value indicating the number of decimal places to which the proportions should be rounded. Default is 2.
#' @param seed An optional seed value for reproducibility. Default is NULL, meaning no seed is set.
#'
#' @return A matrix where rows represent channels and columns represent signatures. Each entry is a randomly generated proportion, and the values in each column sum to 1.
#' @export
#'
#' @examples
#' signatures <- c("Signature1", "Signature2", "Signature3")
#' channels <- c("Channel1", "Channel2", "Channel3")
#' simulate_signature_matrix(signatures, channels, digits = 2, seed = 123)
simulate_signature_matrix <- function(signatures, channels, digits=2, seed = NULL){
  if(!is.null(seed)) {set.seed(seed)}

  ls_data <- lapply(seq_along(signatures), \(x){
    props <- random_numbers_that_sum_to_one(length(channels))
    round(props, digits = 2)
    })

  names(ls_data) <- signatures
  mx <- do.call(args = ls_data, "cbind")
  rownames(mx) <- channels
  return(mx)
}


random_numbers_that_sum_to_one <- function(n){
  nums <- runif(n)
  nums/sum(nums)
}
