\name{lvp_format_label}
\alias{lvp_format_label}
\title{Provides formatted label for use in tikz, svg or rplot}
\usage{
lvp_format_label(label = "", value = "", show = TRUE,
                 svgIdxFontSize = 20L, svgDy = 7L)
}
\arguments{
\item{label}{A character string in the format name or name_index or name=value or
name_index=value. If the value is specified in parameter label and parameter
value is empty, the value in the label will be used.}
\item{value}{A character string specifying a value or empty.}
\item{show}{A logical indicating that the result should be plotted in an Rplot.}
\item{svgIdxFontSize}{An integer specifying font size to use for the subscript in svg.}
\item{svgDy}{An integer specifying the distance to move the baseline of the subscript in svg.}
}
\value{
a list with members svg (character), tikz (character) and r (expression),
giving the result.
}
\description{
Creates the code to use in tikz, svg or R to show the label. The label is
printed in an xy-plot show = TRUE.
}
\examples{
lvp_format_label("beta_10", 0.65)
lvp_format_label("A_i,j=0.45")
lvp_format_label("Gamma")
lvp_format_label(value="1.2345")
}
