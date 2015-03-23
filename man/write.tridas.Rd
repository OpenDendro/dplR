\encoding{UTF-8}
\name{write.tridas}
\alias{write.tridas}

\title{ Write Tree Ring Data Standard (TRiDaS) file }

\description{ This function writes measured or derived (standardized,
  averaged) series of values to a TRiDaS format file.  Some metadata are
  also supported. }

\usage{
write.tridas(rwl.df = NULL, fname, crn = NULL, prec = NULL, ids = NULL,
    titles = NULL, crn.types = NULL, crn.titles = NULL,
    crn.units = NULL, tridas.measuring.method = NA,
    other.measuring.method = "unknown", sample.type = "core",
    wood.completeness = NULL, taxon = "",
    tridas.variable = "ring width", other.variable = NA,
    project.info = list(type = c("unknown"), description = NULL,
                        title = "", category = "", investigator = "",
                        period = ""),
    lab.info = data.frame(name = "", acronym = NA, identifier = NA,
                          domain = "", addressLine1 = NA,
                          addressLine2 = NA, cityOrTown = NA,
                          stateProvinceRegion = NA, postalCode = NA,
                          country = NA),
    research.info = data.frame(identifier = NULL, domain = NULL,
                               description = NULL),
    site.info = list(type = "unknown", description = NULL, title = ""),
    random.identifiers = FALSE, identifier.domain = lab.info$name[1],
    \dots)
}

\arguments{

  \item{rwl.df}{ \code{data.frame} containing tree-ring ring widths in
    millimetres with the series in columns and the years as rows.  The
    series \acronym{ID}s are the column names and the years are the row
    names.  This type of \code{data.frame} is produced by
    \code{\link{read.tucson}}, \code{\link{read.compact}} and
    \code{\link{read.tridas}}.  Defaults to \code{NULL}: no measurement
    series are written. }

  \item{fname}{ \code{character} vector giving the file name of the rwl
    file }

  \item{crn}{ \code{data.frame} or a list of \code{data.frame}s
    containing tree-ring chronologies.  Accepts \code{data.frame}s of
    the type produced by \code{\link{chron}}.  Additionally, allows
    several chronologies per \code{data.frame}.  Any column of the
    \code{data.frame}(s) with a name starting with \code{"samp.depth"}
    is interpreted as a sample depth.  The rest of the columns are
    interpreted as chronologies whose titles are determined from the
    column names (optionally from parameter \code{\var{crn.titles}}).
    Chronology columns and sample depth columns are paired in order so
    that the first chronology gets the first sample depth column, second
    chronology gets the second set of sample depths, etc.  If there are
    less sample depth columns than chronologies, the sample depths are
    recycled.  Defaults to \code{NULL}: no chronologies are written. }

  \item{prec}{ optional \code{numeric} indicating the rounding precision
    of the output file when writing the data contained in
    \code{\var{rwl.df}}.  Defaults to \code{NULL}: no rounding is done
    and the measurements are written in (non-integer) millimetres.
    Possible \code{numeric} values are 0.001, 0.01, 0.05, 0.1, 1, 10,
    100 and 1000, which cause the data to be transformed to micrometres,
    1/100th millimetres, 1/20th millimetres, 1/10 millimetres,
    (millimetres), centimetres, decimetres or metres, respectively, and
    then rounded to integral values.  Data rounded to decimetres are
    written in centimetres (values always ending in zero).  Otherwise,
    the matching unit is used in the file. }

  \item{ids}{ optional \code{data.frame} with column one named
    \code{"tree"} giving the \code{numeric} \acronym{ID} of the tree,
    column two named \code{"core"} giving the \code{numeric}
    \acronym{ID} of the core, optional column three named
    \code{"radius"} giving the \code{numeric} \acronym{ID} of the
    radius, and optional column four named \code{"measurement"} giving
    the \code{numeric} \acronym{ID} of the measurement.  If column
    \code{"measurement"} exists, column \code{"radius"} must also exist.
    Defaults to one core, radius and measurement per tree:\preformatted{
  data.frame(tree=1:n.col, core=rep(1,n.col),
             radius=rep(1,n.col), measurement=rep(1,n.col))}
    where \code{\var{n.col}} is the number of columns in
    \code{\var{rwl.df}}. }

  \item{titles}{ optional \code{data.frame} with column one named
    \code{"tree"} giving the title of the tree, column two named
    \code{"core"} giving the title of the core, column three named
    \code{"radius"} giving the title of the radius, and column four
    named \code{"measurement"} giving the title of the measurement.  By
    default, \code{\var{titles}} is \code{NULL}, and the title hierarchy
    is automatically created out of the column names of
    \code{\var{rwl.df}}, taking \code{\var{ids}} into account. }

  \item{crn.types}{ \code{character} vector or a \code{list} of
    \code{character} vectors giving the types of the derived series in
    \code{\var{crn}}.  A single vector is interpreted as one type per
    \code{data.frame} in \code{\var{crn}}, recycled if not long enough.
    A list of vectors is interpreted as one list element per
    \code{data.frame}.  In this case, the list is recycled to the
    correct length.  After that, the vectors inside the list are
    recycled to match the number of derived series in each
    \code{data.frame} of \code{\var{crn}}.  The default is to write
    empty \samp{<type>} elements. }

  \item{crn.titles}{ optional \code{character} vector or a \code{list}
    of \code{character} vectors giving the titles of the derived series
    in \code{\var{crn}}.  The interpretation is the same as with
    \code{\var{crn.types}}, except that the default is to derive the
    titles from the column names of \code{\var{crn}}.  Also \code{NA}
    means that the column name is used.  }

  \item{crn.units}{ optional \code{character} vector or a \code{list} of
    \code{character} vectors giving the units of the derived series in
    \code{\var{crn}}.  The interpretation is the same as with
    \code{\var{crn.types}}, except that the default is to mark the
    series as \samp{<unitless>}.  Also \code{NA} means
    \samp{<unitless>}.  }

  \item{tridas.measuring.method}{ \code{character} vector giving the
    measuring method used to acquire each series of \code{\var{rwl.df}}.
    Partial matching is used to replace these with the complete terms in
    \code{\link{tridas.vocabulary}}.  If the vector is shorter than the
    number of columns in \code{\var{rwl.df}}, it is recycled to the
    correct length.  The default is to use the information in
    \code{\var{other.measuring.method}} instead.  Also, \code{NA} in any
    position of the vector means that the measuring method information
    for that series is looked up in \code{\var{other.measuring.method}}.
    }

  \item{other.measuring.method}{ \code{character} vector giving the
    measuring method used to acquire each series of \code{\var{rwl.df}}.
    In contrast to \code{\var{tridas.measuring.method}}, these are
    free-form strings in English.  If the vector is shorter than the
    number of columns in \code{\var{rwl.df}}, it is recycled to the
    correct length.  The default value is \code{"unknown"}.  }

  \item{sample.type}{ optional \code{character} vector giving the type
    of the samples, corresponding to \code{"core"} in \code{\var{ids}}.
    The length of the vector, however, must match the number of columns
    in \code{\var{rwl.df}}, or it is recycled to the correct length.  If
    there are several measurements per sample, some elements of
    \code{\var{sample.type}} are redundant.  The default is to use
    \code{"core"} for all series.  }

  \item{wood.completeness}{ optional \code{data.frame} giving wood
    completeness information for the measurement series in
    \code{\var{rwl.df}}.  The number of rows must match the number of
    columns in \code{\var{rwl.df}}.  The columns are expected to be a
    subset of the following (descriptions almost directly quoted from
    TRiDaS specification):

    \describe{

      \item{n.unmeasured.inner}{Field for recording whether there are
        any rings at the inner (i.e. towards pith) edge of the sample
        that have not been measured.  Typically used to note when rings
        are too damaged to measure.  Non-negative integral value.}

      \item{n.unmeasured.outer}{Field for recording whether there are
        any rings at the outer (i.e. towards bark) edge of the sample
        that have not been measured.  Typically used to note when rings
        are too damaged to measure.  Non-negative integral value.}

      \item{pith.presence}{Whether the pith is present or absent.
        Each element must be a partial match with the contents of
        category \code{"complex presence / absence"} in
        \code{\link{tridas.vocabulary}}.}

      \item{heartwood.presence}{Whether the outer (youngest) heartwood
        is present and if so whether it is complete.

        Category \code{"complex presence / absence"} in
        \code{\link{tridas.vocabulary}}.}

      \item{n.missing.heartwood}{Estimated number of missing heartwood
        rings to the pith.  Non-negative integral value.}

      \item{missing.heartwood.foundation}{Description of the way the
        estimation of how many heartwood rings are missing was made and
        what the certainty is.  Free-form string.}

      \item{sapwood.presence}{Whether the sapwood is present or not.

        Category \code{"complex presence / absence"}.}

      \item{n.sapwood}{Number of sapwood rings measured.  Non-negative
        integral value.}

      \item{last.ring.presence}{Last ring under the bark is present or
        absent.

        Category \code{"presence / absence"}.}

      \item{last.ring.details}{If the last ring under the bark is
        present, include information about the completeness of this ring
        and/or season of felling.  Free-form string.}

      \item{n.missing.sapwood}{Estimated number of missing sapwood rings
        to the bark.  Non-negative integral value.}

      \item{missing.sapwood.foundation}{Description of the way the
        estimation of how many sapwood rings are missing was made and
        what the certainty is.  Free-form string.}

      \item{bark.presence}{Bark is present or absent.

        Category \code{"presence / absence"} in
        \code{\link{tridas.vocabulary}}.}

    }
  }

  \item{taxon}{ \code{character} string.  The most detailed taxonomic
    name known for this element (species, genus, family etc).
    Preferably from the \href{http://www.catalogueoflife.org/}{Catalogue
      of Life} controlled vocabulary.  The same string is used for all of
    \code{\var{rwl.df}}.  The default value is an empty string, but a
    proper value should really be given.  }

  \item{tridas.variable}{ \code{character} string.  Measured variable
    (ring width, earlywood, latewood etc) taken from the TRiDaS
    controlled vocabulary (\code{\link{tridas.vocabulary}}, category
    \code{"variable"}).  The same string is used for all of
    \code{\var{rwl.df}}.  Defaults to \code{"ring width"}.  }

  \item{other.variable}{ \code{character} string.  Measured variable as
    a free-form string.  The same string is used for all of
    \code{\var{rwl.df}}.  This is only used if
    \code{\var{tridas.variable}} is \code{NA}.  }

  \item{project.info}{ \code{list} containing information about the
    project.  Elements are the following (includes quotes from the
    TRiDaS specification):

    \describe{

      \item{type}{\code{character} vector.  The type(s) of the project.
        Defaults to \code{"unknown"}.}

      \item{description}{\code{character} string.  A description of the
        project.  Defaults to \code{NULL}: no description.}

      \item{title}{\code{character} string.  The title of the project.
        Defaults to an empty string.}

      \item{category}{\code{character} string.  Category of research
        this project falls into.  Defaults to an empty string.}

      \item{investigator}{\code{character} string.  Principal
        investigator of this project.  Defaults to an empty string.}

      \item{period}{\code{character} string.  When the
        dendrochronological project took place.  Could consist of a
        start- and end-date.  If unknown it should be estimated.
        Defaults to an empty string.}

    }
  }

  \item{lab.info}{ \code{data.frame}. Information about the
    dendrochronological research laboratories where this work was done.
    One row per laboratory.  Defaults to one laboratory with an empty
    name and no other information.  The columns are expected to be a
    subset of the following:

    \describe{

      \item{name}{Name of the laboratory}

      \item{acronym}{Optional acronym of the laboratory}

      \item{identifier}{Optional identifier of the laboratory}

      \item{domain}{The domain which the identifier of the laboratory is
        applicable to.  Could be the \acronym{URL} of the
        organization\enc{’}{'}s server or the name of the organization
        as long as it is not ambiguous.}

      \item{addressLine1}{First address line}

      \item{addressLine2}{Second address line}

      \item{cityOrTown}{City or town}

      \item{stateProvinceRegion}{State, province or region}

      \item{postalCode}{Postal code.  Beware of ignored leading zeros
        if these are given in \code{numeric} or \code{integer} type
        values.  It is always safe to use \code{character} values.}

      \item{country}{Country}

    }
  }

  \item{research.info}{ optional \code{data.frame} with information
    about the systems in which the research project is
    registered.  Columns are the following:

    \describe{

      \item{identifier}{Identifier}

      \item{domain}{Domain which the identifier is applicable to}

      \item{description}{General description}

    }
  }

  \item{site.info}{ \code{list} containing information about the site
    (\samp{<object>}).  Elements are the following, and all are
    \code{character} strings:

    \describe{

      \item{type}{Type of the site.  Defaults to \code{"unknown"}.}

      \item{description}{Description.  Defaults to no description.}

      \item{title}{Title of the site.  Defaults to an empty string.}

    }
  }

  \item{random.identifiers}{ \code{logical} flag.  If \code{TRUE},
    unique random identifiers are created with \code{\link{uuid.gen}}
    and attached to each \samp{<project>} (one in the file),
    \samp{object} (site, one in the file), \samp{<element>} (tree),
    \samp{<sample>} (core), \samp{<radius>}, \samp{<measurementSeries>}
    (measurement) and \samp{<derivedSeries>} element in the resulting
    TRiDaS file.  }

  \item{identifier.domain}{ \code{character} string.  The domain which
    the random identifiers are applicable to.  Could be the
    \acronym{URL} of the organization\enc{’}{'}s server or the name of the
    organization as long as it is not ambiguous.  Defaults to the name
    of the first laboratory in \code{\var{lab.info}}.  }

  \item{\dots}{ Unknown arguments are accepted but not used. }

}

\details{
  The Tree Ring Data Standard (TRiDaS) is described in Jansma et. al
  (2010).  Information about the current version of the standard can be
  found from \href{http://www.tridas.org/}{TRiDaS website}.
}

\value{ \code{\var{fname}} }

\references{
  Jansma, E., Brewer, P. W., and Zandhuis, I. (2010) TRiDaS 1.1: The
  tree-ring data standard.  \emph{Dendrochronologia}, \bold{28}(2),
  99--130.

  TRiDaS \enc{–}{--} The Tree Ring Data Standard.
  \url{http://www.tridas.org/}.
}

\author{ Mikko Korpela }

\note{ This is an early version of the function.  Bugs are likely to
  exist, and parameters are subject to change.  }

\seealso{ \code{\link{write.rwl}}, \code{\link{write.tucson}},
  \code{\link{write.compact}}, \code{\link{write.crn}},
  \code{\link{read.tridas}} }

\examples{library(utils)
## Write raw ring widths
data(co021)
fname1 <- write.tridas(rwl.df = co021,
    fname = tempfile(fileext=".xml"), prec = 0.01,
    site.info = list(title = "Schulman old tree no. 1, Mesa Verde",
                     type = "unknown"),
    taxon = "Pseudotsuga menziesii var. menziesii (Mirb.) Franco",
    project.info = list(investigator = "E. Schulman",
                        title = "", category = "",
                        period = "", type = "unknown"))
print(fname1) # tempfile used for output

## Write mean value chronology of detrended ring widths
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = TRUE)
fname2 <- write.tridas(crn = ca533.crn,
    fname = tempfile(fileext=".xml"),
    taxon = "Pinus longaeva D.K. Bailey",
    project.info =
        list(investigator = "Donald A. Graybill, V.C. LaMarche, Jr.",
             title = "Campito Mountain", category = "",
             period = "", type = "unknown"))
print(fname2) # tempfile used for output

unlink(c(fname1, fname2)) # remove the files
}
\keyword{ IO }
