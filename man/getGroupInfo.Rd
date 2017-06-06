\name{getGroupInfo}


\alias{getGroupInfo}


\title{
Get Information on a VK Group
}


\description{
Returns a data.frame with publicly available information on a VK group by its id.
}


\usage{
getGroupInfo(group_id, access_token, links = TRUE)
}


\arguments{
  \item{group_id}{
A numeric id for the VK group. Available from the group's page (see Details).
}
  \item{access_token}{
Your access token (see ?makeAccessToken).
}
  \item{links}{
If TRUE (default), includes a data.frame with information on the links posted on the group's webpage as a columns of the returned data.frame. 
}
}


\details{
Many VK groups have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 


}


\value{
A data.frame with columns:
\item{id}{group ID}
\item{name}{group name}
\item{screen_name}{group screen name (if any)}
\item{is_closed}{group type (0 - open, 1 - closed, 2 - private)}
\item{description}{group description}
\item{members_count}{number of group members}
\item{status}{group status}
\item{fixed_post}{fixed wall post ID}
\item{verified}{a flag for verified accounts (1 - yes, 0 - no)}
\item{links}{a data.frame with information on public page links posted on the group page (optional, default). See details at https://vk.com/dev/objects/group}

}


\references{
VK API Methods: Community (\url{https://vk.com/dev/objects/group})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
groupid = AAAAAAAA
dat <- getGroupInfo(group_id = groupid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}

