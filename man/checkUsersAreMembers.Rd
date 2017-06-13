\name{checkUsersAreMembers}


\alias{checkUsersAreMembers}


\title{
Check if Users are Members of a Group
}


\description{
Returns a Boolean vector.
}


\usage{
checkUsersAreMembers(user_ids, group_id, access_token)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user_ids}{A numeric user ID. Available from the group's page (see Details).}
  \item{group_id}{A numeric ID for the VK group. Available from the group's page (see Details).}
  \item{access_token}{Your access token (see ?makeAccessToken).}
}


\details{
Many VK groups and users have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 
}


\value{A Boolean vector (TRUE if a user is a member, FALSE otherwise).}


\references{
VK API Methods: Groups (\url{https://vk.com/dev/groups.isMember})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
groupid = AAAAAAAA
userids = c(BBBBBBB, CCCCCCC, DDDDDDD)
dat <- checkUsersAreMembers(user_ids = userids, 
    group_id = groupid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}

