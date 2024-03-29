% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explain_plurals.R
\name{po_explain_plurals}
\alias{po_explain_plurals}
\title{Explain plural message criteria verbally}
\usage{
po_explain_plurals(language, index)
}
\arguments{
\item{language}{A single locale code. See \code{\link[=translate_package]{translate_package()}} for details.}

\item{index}{Optional. If supplied, a 0-based index to
explain for a given language. If not supplied, all plurals
for the supplied language are described.}
}
\description{
The \code{nplural} syntax in .po file metadata can be hard to grok,
even for native speakers. This function tries to de-mystify
this by providing verbal expressions of which numbers apply
to which index in the \code{msgstr} array.
}
