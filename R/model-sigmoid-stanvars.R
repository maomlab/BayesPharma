#' @note stanvar script of a sigmoid dose response equation.
#' For log dose greater than negative infinity (dose greater than 0), the
#' sigmoid dose response equation is used. ec50 is the half maximal response,
#' hill is the slope, top is the maximum response, and bottom is the minimum
#' response. For log dose equal to negative infinity (dose equal to 0), if hill
#' is positive, then the bottom value is returned. If hill is negative, then the
#' top value is returned.
#'
dr_stanvar <- brms::stanvar(
  scode = paste(
    "   real sigmoid(",
    "      real ec50,",
    "      real hill,",
    "      real top,",
    "      real bottom,",
    "      real log_dose) {",
    "        if( log_dose > negative_infinity() ) {",
    "          return (bottom + (top - bottom) /",
    "            (1 + 10^((ec50 - log_dose)*hill)));",
    "        } else { ",
    "           if( hill > 0) {",
    "            return bottom;",
    "            } else {",
    "               return top;",
    "            }",
    "        }",
    "   }", sep = "\n"),
  block = "functions")