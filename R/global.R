
# This fixes a NOTE during checks due to undefined variable names - used in
# ggplot2 syntax.

utils::globalVariables(c(".data"))
