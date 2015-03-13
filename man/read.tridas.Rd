\encoding{UTF-8}
\name{read.tridas}
\alias{read.tridas}

\title{ Read Tree Ring Data Standard (TRiDaS) File }

\description{ This function reads in a TRiDaS format \acronym{XML} file.
  Measurements, derived series and various kinds of metadata are
  supported. }

\usage{
read.tridas(fname, ids.from.titles = FALSE,
            ids.from.identifiers = TRUE, combine.series = TRUE,
            trim.whitespace = TRUE, warn.units = TRUE)
}

\arguments{

  \item{fname}{ \code{character} vector giving the file name of the
    TRiDaS file. }

  \item{ids.from.titles}{ \code{logical} flag indicating whether to
    override the (tree, core, radius, measurement) structure imposed by
    the element hierarchy (element, sample, radius, measurementSeries)
    of the file.  If \code{TRUE}, measurement series will be rearranged
    by matching titles in the file at the aforementioned four levels of
    the hierarchy.  Defaults to \code{FALSE}, i.e. the element hierarchy
    of the file will be used. }

  \item{ids.from.identifiers}{ \code{logical} flag indicating whether to
    (partially) override the element hierarchy of the file.  If
    \code{TRUE}, measurement series will be grouped according to
    matching identifiers at the measurementSeries level, where
    identifiers are available.  The changes caused by this option are
    applied on top of the structure imposed by the file or computed from
    matching titles if \code{\var{ids.from.titles} == TRUE}.  Defaults
    to \code{TRUE}. }

  \item{combine.series}{ \code{logical} flag indicating whether to
    combine two or more measurement series with the same set of (tree,
    core, radius, measurement) \acronym{ID} numbers.  Each set of
    combined measurement series will be represented by one column of a
    resulting \code{data.frame}.  Overlapping years of combined series do not
    produce a warning.  If several data points are available for a given
    year, the function chooses one in a rather arbitrary manner.  This
    option can only have effect when \code{\var{ids.from.titles} ||
      \var{ids.from.identifiers}}. }

  \item{trim.whitespace}{ \code{logical} flag indicating whether to
    replace repeated white spaces in the text content of the file with
    only one space.  Defaults to \code{TRUE}, i.e. excess white space
    will be trimmed from the text. }

  \item{warn.units}{ \code{logical} flag indicating whether to warn
    about unitless measurements and \dQuote{strange} units.  The
    function expects measurements in units that can be converted to
    millimetres.  Defaults to \code{TRUE}: warnings will be given.  For
    example, density measurements will trigger warnings, which can be
    disabled by setting this option to \code{FALSE}. }
  
}

\details{

  The Tree Ring Data Standard (TRiDaS) is described in Jansma et. al
  (2010).  Information about the current version of the standard can be
  found from \href{http://www.tridas.org/}{TRiDaS website}.

  The parameters used for rearranging (\code{\var{ids.from.titles}},
  \code{\var{ids.from.identifiers}}) and combining
  (\code{\var{combine.series}}) measurement series only affect the four
  lowest levels of document structure: element, sample, radius,
  measurementSeries.  Series are not reorganized or combined at the
  upper structural levels (project, object).

}

\value{
  A list with a variable number of components according to the contents
  of the input file.  The possible list components are:

  \item{measurements}{A \code{data.frame} or a list of \code{data.frame}s with
    the series in columns and the years as rows.  Contains measurements
    (\samp{<measurementSeries>}) with known years.  The series
    \acronym{ID}s are the column names and the years are the row
    names.  The series \acronym{ID}s are derived from \samp{<title>}
    elements in the input file.  Each unique combination of
    \samp{<project>}, \samp{<object>}, \samp{<unit>}, \samp{<taxon>},
    and \samp{<variable>} gets a separate \code{data.frame}. }

  \item{ids}{A \code{data.frame} or a list of \code{data.frame}s with columns
    named \code{"tree"}, \code{"core"}, \code{"radius"}, and
    \code{"measurement"}, together giving a unique \code{numeric} \acronym{ID}
    for each column of the \code{data.frame}(s) in
    \code{\var{measurements}}.

    If \code{!\var{combine.series} && (\var{ids.from.titles} || \var{ids.from.identifiers})}, some rows may be non-unique. }
  
  \item{titles}{A \code{data.frame} or a list of \code{data.frame}s with
    columns named \code{"tree"}, \code{"core"}, \code{"radius"}, and
    \code{"measurement"}, containing the \samp{<title>} hierarchy of
    each column of the \code{data.frame}(s) in \code{\var{measurements}}. }
  
  \item{wood.completeness}{A \code{data.frame} or a list of \code{data.frame}s
    containing wood completeness information.  Column names are a subset
    of the following, almost self-explanatory set:
    \code{"pith.presence"}, \code{"heartwood.presence"},
    \code{"sapwood.presence"},\cr
    \code{"last.ring.presence"}, \code{"last.ring.details"},
    \code{"bark.presence"},\cr
    \code{"n.sapwood"}, \code{"n.missing.heartwood"},
    \code{"n.missing.sapwood"},\cr
    \code{"missing.heartwood.foundation"},
    \code{"missing.sapwood.foundation"},\cr
    \code{"n.unmeasured.inner"}, \code{"n.unmeasured.outer"}. }
  
  \item{unit}{A \code{character} vector giving the unit of the
    measurements.  Length equals the number of \code{data.frame}s in
    \code{\var{measurements}}. }
  
  \item{project.id}{A \code{numeric} vector giving the project
    \acronym{ID}, i.e. the position of the corresponding
    \samp{<project>} element), of the measurements in each
    \code{data.frame} in \code{\var{measurements}}.  Length equals the
    number of \code{data.frame}s. }
  
  \item{project.title}{A \code{character} vector giving the title of the
    project of each \code{data.frame} in \code{\var{measurements}}.
    Length equals the number of \code{data.frame}s. }

  \item{site.id}{A \code{data.frame} giving the site \acronym{ID}
    (position of \samp{<object>} element(s) within a \samp{<project>})
    of each \code{data.frame} in \code{\var{measurements}}.  May have
    several columns to reflect the possibly nested \samp{<object>}
    elements. }
  
  \item{site.title}{A \code{data.frame} giving the site
    (\samp{<object>}) title of each \code{data.frame} in
    \code{\var{measurements}}.  May have several columns to reflect the
    possibly nested \samp{<object>} elements. }
  
  \item{taxon}{A \code{data.frame} showing the taxonomic name for each
    \code{data.frame} in \code{\var{measurements}}.  Contains some of
    the following columns: \code{"text"}, \code{"lang"},
    \code{"normal"}, \code{"normalId"}, \code{"normalStd"}.  The first
    two are a free-form name and its language, and the rest are related
    to a normalized name. }
  
  \item{variable}{A \code{data.frame} showing the measured variable of
    each \code{data.frame} in \code{\var{measurements}}.  Contains some
    of the following columns: \code{"text"}, \code{"lang"},
    \code{"normal"}, \code{"normalId"}, \code{"normalStd"},
    \code{"normalTridas"}.  The first two are a free-form name and its
    language, and the rest are related to a normalized name. }
  
  \item{undated}{A \code{list} of measurements with unknown years,
    together with metadata.  Elements are a subset of the following:

    \describe{

      \item{data}{A \code{numeric} vector or a list of such vectors
        containing measurement series }

      \item{unit}{A \code{character} vector giving the unit of the
        measurements.  Length equals the number of measurement series in
        \code{\var{undated}$\var{data}} }

      \item{ids}{A \code{data.frame} with columns named \code{"tree"},
        \code{"core"}, \code{"radius"}, and \code{"measurement"},
        together giving a \code{numeric} \acronym{ID} for each measurement
        series in \code{\var{undated}$\var{data}}.  The rows are
        guaranteed to be unique only when comparing measurement series
        with the same \code{\var{project.id}} and \code{\var{site.id}},
        but not if \code{\var{ids.from.titles} ||
          \var{ids.from.identifiers}}. }

      \item{titles}{A \code{data.frame} with columns named
        \code{"tree"}, \code{"core"}, \code{"radius"}, and
        \code{"measurement"}, containing the \samp{<title>} hierarchy of
        each measurement series in \code{\var{undated}$\var{data}} }
      
      \item{project.id}{A \code{numeric} vector giving the project
        \acronym{ID} of each measurement series in
        \code{\var{undated}$\var{data}} }
      
      \item{project.title}{A \code{character} vector giving the project
        title of each measurement series in
        \code{\var{undated}$\var{data}} }
      
      \item{site.id}{A \code{data.frame} giving the site \acronym{ID} of
        each measurement series in \code{\var{undated}$\var{data}} }

      \item{site.title}{A \code{data.frame} giving the site title of
        each measurement series in \code{\var{undated}$\var{data}} }

      \item{variable}{A \code{data.frame} containing the variable of
        each measurement series in \code{\var{undated}$\var{data}} }

      \item{taxon}{A \code{data.frame} containing taxonomic names of
        each measurement series in \code{\var{undated}$\var{data}} }

      \item{wood.completeness}{A \code{data.frame} containing wood
        completeness information of each measurement series in
        \code{\var{undated}$\var{data}} }
      
    }
  }

  \item{derived}{A \code{list} of calculated series of values, together
    with metadata.  Elements are a subset of the following:

    \describe{

      \item{data}{A \code{numeric} vector or a list of such vectors
        containing calculated series of values.}

      \item{link}{A \code{list} of \code{data.frame}s, one for each
        series in \code{\var{derived}$\var{data}}, giving links to the
        measurements used to form the corresponding derived series.
        Each \code{data.frame} has a subset of the following columns:
        \code{"idRef"} (reference to a series in the same file),
        \code{"xLink"} (\acronym{URI}), \code{"identifier"}, and
        \code{"domain"} (identifier and its domain, not necessarily in
        the same file).}

      \item{project.id}{A \code{numeric} vector giving the project
        \acronym{ID} of each derived series in
        \code{\var{derived}$\var{data}}}

      \item{id}{A \code{numeric} vector giving the \acronym{ID} (order
        of appearance in the project) of each derived series in
        \code{\var{derived}$\var{data}}}

      \item{title}{A \code{character} vector giving the title of each
        derived series in \code{\var{derived}$\var{data}}}

      \item{project.title}{A \code{character} vector giving the project
        title of each derived series in \code{\var{derived}$\var{data}}}

      \item{unit}{A \code{character} vector giving the unit of the
        derived series.  Length equals the number of series in
        \code{\var{derived}$\var{data}}.}

      \item{standardizing.method}{A \code{character} vector giving the
        standardizing method of the derived series.  Length equals the
        number of series in \code{\var{derived}$\var{data}}.}

      \item{variable}{A \code{data.frame} containing the variable of
        each series in \code{\var{derived}$\var{data}}}
      
    }
  }

  \item{type}{A \code{data.frame} containing the type of various
    entities, and metadata related to each \samp{type} element.
    Contents are \code{NA} where the metadata is not applicable (e.g.,
    no \code{\var{tree.id}} when the \samp{type} element refers to a
    project).  Columns are a subset of the following:

    \describe{

      \item{text}{The text of the \samp{type} element}

      \item{lang}{The language of the text}

      \item{normal}{The normalized name of the type}

      \item{normalId}{The \acronym{ID} value of the type in the standard
        dictionary}

      \item{normalStd}{The name of the standard}

      \item{project.id}{The \acronym{ID} of the project}

      \item{site.id}{One or more columns with this prefix, depending on
        the maximum depth of the \samp{<object>} hierarchy.  Gives the
        \acronym{ID} of the site where the \samp{<type>} element
        appeared.}

      \item{tree.id}{The \acronym{ID} of the tree}

      \item{core.id}{The \acronym{ID} of the core}

      \item{derived.id}{The \acronym{ID} of the derived series}

      \item{project.title}{The title of the project}

      \item{site.title}{One or more columns with this prefix, depending
        on the maximum depth of the \samp{<object>} hierarchy.  Gives the
        title of the site where the \samp{<type>} element appeared.}

      \item{tree.title}{The title of the tree}

      \item{core.title}{The title of the core}

      \item{derived.title}{The title of the derived series}

    }
  }

  \item{comments}{A \code{data.frame} containing comments to various
    entities, and metadata related to each \samp{comments} element.
    Contents are \code{NA} where the metadata is not applicable.  Columns
    are a subset of the following:

    \describe{

      \item{text}{The text of the \samp{comments} element}

      \item{project.id}{The \acronym{ID} of the project}

      \item{site.id}{One or more columns with this prefix, depending on
        the maximum depth of the \samp{<object>} hierarchy.  Gives the
        \acronym{ID} of the site.}

      \item{tree.id}{The \acronym{ID} of the tree}

      \item{core.id}{The \acronym{ID} of the core}

      \item{radius.id}{The \acronym{ID} of the radius}

      \item{measurement.id}{The \acronym{ID} of the measurement series}

      \item{derived.id}{The \acronym{ID} of the derived series}

      \item{project.title}{The title of the project}

      \item{site.title}{One or more columns with this prefix, depending
        on the maximum depth of the \samp{<object>} hierarchy.  Gives the
        title of the site.}

      \item{tree.title}{The title of the tree}

      \item{core.title}{The title of the core}

      \item{radius.title}{The title of the radius}

      \item{measurement.title}{The title of the measurement series}

      \item{derived.title}{The title of the derived series}

    }
  }

  \item{identifier}{A \code{data.frame} containing identifiers of
    various entities, and metadata related to each \samp{identifier}
    element.  Contents are \code{NA} where the metadata is not
    applicable.  Columns are a subset of the following:

    \describe{

      \item{text}{The text of the \samp{identifier} element}

      \item{domain}{The domain which the identifier is applicable to}

      \item{project.id}{The \acronym{ID} of the project}

      \item{site.id}{One or more columns with this prefix, depending on
        the maximum depth of the \samp{<object>} hierarchy.  Gives the
        \acronym{ID} of the site.}

      \item{tree.id}{The \acronym{ID} of the tree}

      \item{core.id}{The \acronym{ID} of the core}

      \item{radius.id}{The \acronym{ID} of the radius}

      \item{measurement.id}{The \acronym{ID} of the measurement series}

      \item{derived.id}{The \acronym{ID} of the derived series}

      \item{project.title}{The title of the project}

      \item{site.title}{One or more columns with this prefix, depending
        on the maximum depth of the \samp{<object>} hierarchy.  Gives the
        title of the site.}

      \item{tree.title}{The title of the tree}

      \item{core.title}{The title of the core}

      \item{radius.title}{The title of the radius}

      \item{measurement.title}{The title of the measurement series}

      \item{derived.title}{The title of the derived series}

    }
  }

  \item{remark}{A \code{list} of remarks concerning individual measured
    or derived values, with some of the following items:

    \describe{

      \item{measurements}{Remarks related to measurements with a known
        year.  A \code{data.frame} with the following columns:

        \describe{

          \item{text}{The remark}

          \item{frame}{Index to a \code{data.frame} in
            \code{\var{measurements}}}

          \item{row}{Index to a row of the \code{data.frame}}

          \item{col}{Index to a column of the \code{data.frame}}

        }
      }
      
      \item{undated}{Remarks related to measurements without a known
        year.  A \code{data.frame} with the following columns:

        \describe{

          \item{text}{The remark}

          \item{series}{Index to a series in
            \code{\var{undated}$\var{data}}}

          \item{idx}{Index to a value in the series}

        }
      }

      \item{derived}{Remarks related to derived values.  A
        \code{data.frame} with the following columns:

        \describe{

          \item{text}{The remark}

          \item{series}{Index to a series in
            \code{\var{derived}$\var{data}}}

          \item{idx}{Index to a value in the series}

        }
      }
    }
  }

  \item{laboratory}{A \code{data.frame} or a \code{list} of
    \code{data.frame}s with one item per project.  Each
    \code{data.frame} contains information about the research
    laboratories involved in the project.  Columns are a subset of the
    following:

    \describe{

      \item{name}{Name of the laboratory}

      \item{acronym}{Acronym of the name}

      \item{identifier}{Identifier}

      \item{domain}{Domain which the identifier is applicable to}

      \item{addressLine1}{Address}

      \item{addressLine2}{Another address line}

      \item{cityOrTown}{City or town}

      \item{stateProvinceRegion}{State, province or region}

      \item{postalCode}{Postal code}

      \item{country}{Country}

    }
  }

  \item{research}{A \code{data.frame} or a \code{list} of
    \code{data.frame}s with one item per project.  Each
    \code{data.frame} contains information about the systems in which
    the research project is registered.  Columns are the following:

    \describe{

      \item{identifier}{Identifier}

      \item{domain}{Domain which the identifier is applicable to}

      \item{description}{General description}

    }
  }

  \item{altitude}{A \code{data.frame} containing the altitude of
    trees.  Columns are the following:

    \describe{

      \item{metres}{The altitude in metres}

      \item{project.id}{The \acronym{ID} of the project}

      \item{site.id}{One or more columns with this prefix, depending on
        the maximum depth of the \samp{<object>} hierarchy.  Gives the
        \acronym{ID} of the site.}

      \item{tree.id}{The \acronym{ID} of the tree}

      \item{project.title}{The title of the project}

      \item{site.title}{One or more columns with this prefix, depending
        on the maximum depth of the \samp{<object>} hierarchy.  Gives the
        title of the site.}

      \item{tree.title}{The title of the tree}

    }
  }

  \item{preferred}{A \code{data.frame} containing links to preferred
    measurement series.  Columns are a subset of the following:

    \describe{

      \item{idRef}{Reference to a series in the same file}

      \item{xLink}{\acronym{URI}}

      \item{identifier}{Identifier of a series not necessarily in the
        same file}

      \item{domain}{Domain which the identifier is applicable to}

      \item{project.id}{The \acronym{ID} of the project}

      \item{site.id}{One or more columns with this prefix, depending on
        the maximum depth of the \samp{<object>} hierarchy.  Gives the
        \acronym{ID} of the site.}

      \item{tree.id}{The \acronym{ID} of the tree}

      \item{project.title}{The title of the project}

      \item{site.title}{One or more columns with this prefix, depending on
        the maximum depth of the \samp{<object>} hierarchy.  Gives
        the title of the site.}

      \item{tree.title}{The title of the tree}

    }
  }
}

\references{
  Jansma, E., Brewer, P. W., and Zandhuis, I. (2010) TRiDaS 1.1: The
  tree-ring data standard.  \emph{Dendrochronologia}, \bold{28}(2),
  99--130.

  TRiDaS \enc{–}{--} The Tree Ring Data Standard.
  \url{http://www.tridas.org/}.
}

\author{ Mikko Korpela }

\note{ This is an early version of the function.  Bugs are likely to
  exist, and parameters and return values are subject to change.  Not
  all metadata defined in the TRiDaS specification is supported --
  unsupported elements are quietly ignored. }

\seealso{ \code{\link{read.rwl}}, \code{\link{read.tucson}},
  \code{\link{read.compact}}, \code{\link{read.fh}},
  \code{\link{write.tridas}} }

\keyword{ IO }
