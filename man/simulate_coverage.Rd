% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coverage.R
\name{simulate_coverage}
\alias{simulate_coverage}
\title{Simulate a GIA model with an assumed error structure}
\usage{
simulate_coverage(
  n_sims = 10,
  n_boot = 100,
  verbose = TRUE,
  experimental_grid,
  model_par,
  alpha = 0.05,
  noise_par = c(a0 = 2, a1 = 0.01),
  GIA_fn = base_GIA,
  S_fn = calc_S_base,
  fn_list = NULL
)
}
\arguments{
\item{n_sims}{number of coverage simulations}

\item{n_boot}{number of bootstraps to use in each simulation}

\item{verbose}{logical indicating whether we should use print statements.  Default is TRUE}

\item{experimental_grid}{data frame with columns 'dose_A' and 'dose_B'}

\item{model_par}{named vector of parameters corresponding to those used in GIA_fn()}

\item{alpha}{alpha level used to produce confidence intervals for each bootstrap}

\item{noise_par}{named vector for the noise parameter.  Must have names "a0" and "a1".  See \code{?base_gia} for more details.}

\item{GIA_fn}{function used to calculate GIA.  Default is base_GIA().}

\item{S_fn}{function to calculate S}

\item{fn_list}{additional parameters to pass to GIA_fn}
}
\value{
list with the following entries
\describe{
\item{interaction_cov}{This is the percent of times 0 was in the (1-alpha)\% confidence interval for the interaction term "tau_1" from the simulated results}
\item{params_cov}{This is the percent of times the true model parameter (those from model_par) lies in the (marginal) 95\% confidence interval for that model parameter.}
\item{tau_pos}{This is the percent of times the (1-alpha)\% CI of "tau_1" was completely above 0.}
\item{tau_neg}{This is the percent of times (1-alpha)\% CI of "tau_1" is completely below zero}
}
}
\description{
Simulate a GIA model with an assumed error structure
}
\examples{
df <- loewesadditivity::cyrpa_ripr
df$dose_A <- df$CyRPA
df$dose_B <- df$RIPR
data <- fortify_gia_data(df)
model_params <- c("beta_A" = .247, "beta_B" = .224,
                  "gamma_A" = .734, "gamma_B" = .806,
                  "tau_1" = .28, "tau_2" = -.28)
experimental_grid <- make_grid(par = model_params,
                               n = 5)
n_boot <- 100
n_sims <- 10
GIA_fn <- base_GIA
S_fn <- calc_S_base
fn_list <- NULL
alpha <- .05
verbose <- TRUE
## NOT RUN
##out <- simulate_coverage(n_sims = n_sims,
  ##                      n_boot = n_boot,
   ##                     verbose = TRUE,
    ##                    experimental_grid = experimental_grid,
     ##                   model_par = model_params,
     ##                   alpha = .05,
   ##                     noise_par = c("a0" = 3, "a1" = .01),
   ##                     GIA_fn = base_GIA,
   ##                     fn_list = NULL)
##out
}
