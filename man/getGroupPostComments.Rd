\name{getGroupPostComments}


\alias{getGroupPostComments}


\title{
Get Comments to a Post on a Group's Wall
}


\description{
Returns a data.frame with publicly available information about comments to a post on a group's wall.
}


\usage{
getGroupPostComments(group_id, post_id, access_token)
}


\arguments{
  \item{group_id}{A numeric id for the VK group. Available from the group's page (see Details).}
  \item{post_id}{A numeric id for the post on the group's wall.}
  \item{access_token}{Your access token (see ?makeAccessToken).}
}


\details{
Many VK groups have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 

}


\value{
A data.frame with columns:
\item{id}{Comment ID}
\item{from_id}{Comment author's ID}
\item{date}{Date when the comment was posted}
\item{text}{Comment text}
\item{likes_count}{Number of likes the comment got}
\item{reply_to_user}{User ID whom this comment replies to (NA if not a reply)}
\item{reply_to_comment}{Comment ID to which this comment replies to (NA if not a reply)}
}

\references{
VK API Methods: Wall (\url{https://vk.com/dev/wall.getComments})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
groupid = AAAAAAAA
postid = BBBBBB
dat <- getGroupInfo(group_id = groupid, post_id = postid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}

