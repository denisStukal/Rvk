\name{getGroupPostLikes}


\alias{getGroupPostLikes}


\title{
Get IDs of all Users who Liked a Post on a Group Page
}


\description{
Returns a vector of user IDs who liked a post on a group page.
}


\usage{
getGroupPostLikes(group_id, post_id, access_token)
}


\arguments{
  \item{group_id}{A numeric ID for the VK group. Available from the group's page (see Details).}
  \item{post_id}{A numeric ID of a post on a group page.}
  \item{access_token}{Your access token (see ?makeAccessToken).}
}


\details{
Many VK groups and users have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 
}


\value{
A vector of user IDs.
}


\references{
VK API Methods: Likes (\url{https://vk.com/dev/likes.getList})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
groupid = AAAAAAAA
postid = BBBBBB
dat <- getGroupPostLikes(group_id = groupid, 
    post_id = postid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}

