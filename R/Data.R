#' Stage record and ADCP discharge measurements
#' of the Saint-Laurent river at Lauzon and Neuville
#'
#' A data frame containing information about the stage record in meters,
#' referenced to the sea mean level (SML) from 16/06/2009 until 26/08/2009
#' provided by Pascal, Matte and Bourgault Daniel.
#' Additionally, information on ADCP discharge measurements is available
#' from the gauging campaign conducted on 21/08/2009
#'
#' @format
#' \describe{
#'  \item{year}{Year}
#'  \item{month}{Month}
#'  \item{day}{Day}
#'  \item{hour}{Hour}
#'  \item{minute}{Minute}
#'  \item{second}{Second}
#'  \item{date}{Date with time}
#'  \item{h1}{Real value, stage record upstream, at Neuville}
#'  \item{u.h1}{Uncertainty of the stage upstream expressed as standard deviation}
#'  \item{h2}{real value, stage record downstram, at Lauzon}
#'  \item{u.h2}{Uncertainty of the stage downstream expressed as standard deviation}
#'  \item{Q}{Discharge ADCP measurement}
#'  \item{uQ}{Uncertainty of the discharge expressed as standard deviation}
#' }
#'
#' @source \url{https://codeocean.com/capsule/8881837/tree/v2}
"Saint_Laurent_F2"
