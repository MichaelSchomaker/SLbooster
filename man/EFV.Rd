\name{EFV}
\alias{EFV}
\docType{data}
\title{
Pharmacoepidemiological HIV treatment data 
}
\description{
A hypothetical, simulated dataset which is line with the data-generating process of Schomaker et al. (2023) and inspired by the data of Bienczak et al. (2017); see references below.
}
\usage{data(EFV)}
\format{
  A data frame with 5000 observations on the following variables:
  \describe{
    \item{\code{sex}}{The patient's sex}
    \item{\code{metabolic}}{Metabolism status (slow, intermediate, extensive) related to the single nucleotide polymorphisms in the CYP2B6 gene, which is relevant
for metabolizing evafirenz and directly affects its concentration in the body.}
    \item{\code{log_age}}{log(age) at baseline}
    \item{\code{NRTI}}{Nucleoside reverse transcriptase inhibitor (NRTI) component of HIV treatment, i.e.
abacavir, stavudine or zidovudine.}
    \item{\code{weight.0}}{log(weight) at time 0 (baseline)}
    \item{\code{efv.0}}{Efavirenz concentration at time 0 (baseline). Binarized into high (=1) and low (=0).}
    \item{\code{VL.0}}{Elevated viral load (viral failure) at time 0 (baseline)}
    \item{\code{adherence.1}}{Adherence at time 1 (if 0, then signs of non-adherence)}
    \item{\code{weight.1}}{log(weight) at time 1}
    \item{\code{efv.1}}{Efavirenz concentration at time 1. Binarized into high (=1) and low (=0).}
    \item{\code{VL.1}}{Elevated viral load (viral failure) at time 1}
    \item{\code{adherence.2}}{Adherence at time 2 (if 0, then signs of non-adherence)}
    \item{\code{weight.2}}{log(weight) at time 2}
    \item{\code{efv.2}}{Efavirenz concentration at time 2. Binarized into high (=1) and low (=0).}
    \item{\code{VL.2}}{Elevated viral load (viral failure) at time 2}
    \item{\code{adherence.3}}{Adherence at time 3 (if 0, then signs of non-adherence)}
    \item{\code{weight.3}}{log(weight) at time 3}
    \item{\code{efv.3}}{Efavirenz concentration at time 3. Binarized into high (=1) and low (=0).}
    \item{\code{VL.3}}{Elevated viral load (viral failure) at time 3}
    \item{\code{adherence.4}}{Adherence at time 4 (if 0, then signs of non-adherence)}
    \item{\code{weight.4}}{log(weight) at time 4}
    \item{\code{efv.4}}{Efavirenz concentration at time 4. Binarized into high (=1) and low (=0).}
    \item{\code{VL.4}}{Elevated viral load (viral failure) at time 4}
  }
}

\references{
Schomaker M, McIlleron H, Denti P, Diaz I. (2024) \emph{Causal Inference for Continuous Multiple Time Point Interventions}, Statistics in Medicine, 43(28):5380-5400, also: \emph{https://arxiv.org/abs/2305.06645}.

Bienczak et al. (2017) \emph{Determinants of virological outcome and adverse events in African children treated with paediatric nevirapine fixed-dose-combination tablets}, AIDS, \emph{31:905-915}
}
\examples{
data(EFV)
str(EFV)
}
\keyword{Datasets}
\keyword{efavirenz}