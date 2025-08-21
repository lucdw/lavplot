\name{lvp_format_label}
\alias{lvp_format_label}
\title{Provides formatted label for use in tikz, svg or rplot}
\usage{
lvp_format_label(label = "", value = "", show = TRUE,
                 svgIdxFontSize = 20L, svgDy = 7L)
}
\arguments{
\item{label}{A character string in the format 'name' or 'name_index' or
'name=value' or 'name_index=value'. If the value is specified in parameter
\code{label} and parameter \code{value} is empty, the value in \code{label}
will be used.}
\item{value}{A character string specifying a value or empty.}
\item{show}{A logical indicating that the result should be plotted in an Rplot.}
\item{svgIdxFontSize}{An integer specifying font size to use for the subscript in svg.}
\item{svgDy}{An integer specifying the distance to move the baseline of the subscript in svg.}
}
\value{
a list with members svg, tikz and r, giving the result.
}
\description{
Creates the code to use in tikz, svg or R to show the label. The label is
printed in an xy-plot show = TRUE.
}
\details{
If both \code{label} and \code{value} are empty, the resulting codes are
also empty and nothing will be shown.

If \code{label} is empty and \code{value} is not, processing is done as if
\code{label} was specified and \code{value} was empty.

If \code{label} contains the string "1van", the label value is set to "1", this
is to allow distinct names for regression intercepts and still label them as "1".

If the 'name' in \code{label} is a Greek character or 'varepsilon', it is attempted to
generate code which shows the Greek symbol. If there is an 'index' part in
\code{label}, it is attempted to generate code which displays this value as a
subscript. In the svg code the values \code{svgIdxFontSize} and \code{svgDy} are
used for the subscript in the generated code. If a 'value' is present, it is attempted
to show this value after the label, with an equal sign between both.
}
\examples{
lvp_format_label("beta_10", 0.65)
lvp_format_label("A_i,j=0.45")
lvp_format_label("Gamma")
lvp_format_label(value="1.2345")
}
