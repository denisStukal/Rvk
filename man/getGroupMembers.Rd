\name{getGroupMembers}


\alias{getGroupMembers}


\title{
Get Information on Members of a VK Group by Group ID
}


\description{
Returns a data.frame with publicly available information on members of a VK group by group id.
}



\usage{
getGroupMembers(group_id, access_token, num_universities = 1, \cr num_schools = 1, num_jobs = 1, count = 1000)
}


\arguments{
  \item{group_id}{ A numeric id for the VK group. Available from the group's page (see Details).}
  \item{access_token}{Your access token (see ?makeAccessToken).}
  \item{num_universities}{Number of universities to be returned (default is 1). Universities are returned from the last to the first, thus by default the last university is returned.}
  \item{num_schools}{Number of schools to be returned (default is 1). Schools are returned from the last to the first, thus by default the last school is returned.}
  \item{num_jobs}{Number of jobs to be returned (default is 1). Jobs are returned from the last to the first, thus by default the last job is returned.}
  \item{count}{The number of group members to be returned. The maximum possible number allowed by VK API is 1000 (default).}
}


\details{
Many VK groups have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 
}


\value{
A data.frame with columns:
\item{id}{group member's user ID}
\item{first_name}{group member's first name}
\item{last_name}{group member's last name}
\item{...}{additional fields depend on publicly available information}
}
\references{
VK API Methods: Group Members (\url{https://vk.com/dev/groups.getMembers})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
groupid = AAAAAAAA
dat <- getGroupMembers(group_id = groupid, access_token = mytoken, num_universities = 3, num_schools = 2, num_jobs = 4)
str(dat)
}
}


\keyword{methods}

