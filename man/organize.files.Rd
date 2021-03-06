% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Organize.R
\name{organize.files}
\alias{organize.files}
\title{Organizes files according to type.}
\usage{
organize.files('Data', 'Organized', '.jpg', TRUE)
}
\arguments{
\item{source_folder_path}{path of the folder in which the files are present.}

\item{organized_folder_path}{path of the folder where the files needs to be stored.}

\item{pattern_file}{pattern to look for in the file name.}

\item{delete_source}{boolean value which toggles the option to delete the organized files.}
}
\value{
NA
}
\description{
This function scans the source folder for the user provided type and then
copies it to the other folder which can be used for organization. The organized
folder doesn't have to exist.
}
\examples{
## To organize jpeg files from Data folder to Organized folder and delete
the source files.

organize.files('Data', 'Organized', '.jpg', TRUE)
}
