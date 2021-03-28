# Report

The full report can be found <a href="https://github.com/kh-w/Cramer_Lundberg_ruin_model/blob/main/project_report_20190503.pdf">here</a>.

# Abstract

Ruin theory is motivated by the practical issue of solvency. Back in 1903, Swedish actuary
Filip Lundberg introduced the Cramer-Lundberg model to understand the behaviour of the
surplus process of insurance contracts. It became the foundation of modern ruin theory after
Harald Cramer republished his work in 1930s.

Based on the classic Cramer-Lundberg model, there are different variants/extensions. For
example: Sparre Andersen model[2](the waiting time between claims is not restricted to be
a Poisson distribution), Classical Cramer-Lundberg Model with Stochastic Investment Return
[1].

This project explained the mathematical structure of the Classic Cramer-Lundberg Model
which is widely used by insurance industry. The simplest form of the model only consists
initial capital, constant premium rate and an assumed claims distribution. Under this simple
structure, we will show that the survival probability increases as the initial capital increases.
Furthermore, if the surplus process lasts forever, we will show that probability of ruin is 100%
if the premium rate does not meet a criteria called ”net profit condition”.

The simplest form of Cramer-Lundberg model is not enough for practical use for business.
Therefore, throughout the derivation of the model, we should seek possibilities to implement
additional features. One of the most intuitive and important feature is to implement investment
income from bonds. Assuming the investment return is constant throughout the time horizon
(from now to forever), i.e. no interest rate risks and no reinvestment risks, the model behaves
differently compare with no investments. We will show that the net profit condition is not
required for ultimate survival as long as the initial capital is large enough to earn sufficient investment
income to compensate the premium shortfall. This model could be a reference when
pricing a single premium product.

In this project, we will explore the Cramer-Lundberg model under three common claims
distributions: Exponential, Hyper-exponential and Erlang distributions. The theoretical curve
will be verified by simulations written in R[3].
