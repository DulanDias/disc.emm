_mutual_information <- function(continuous_var, binned_var) {
  # Calculate the joint probability distribution
  joint_prob <- prop.table(table(binned_var, continuous_var), 2)

  # Calculate the marginal probability distributions
  marginal_discrete <- prop.table(table(binned_var))
  marginal_continuous <- density(continuous_var)$y

  # Calculate the mutual information
  return(sum(joint_prob * log2(joint_prob / (marginal_discrete %*% marginal_continuous))))
}
