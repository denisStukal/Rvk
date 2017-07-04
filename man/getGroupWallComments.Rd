\name{getGroupWallComments}


\alias{getGroupWallComments}


\title{
Get Comments to Posts from a Group Page
}


\description{
Returns a data.frame with information about comments to posts on a group page. 
}


\usage{
getGroupWallComments(group_id, num_posts = 'all', access_token, verbose = FALSE)
}


\arguments{
  \item{group_id}{A numeric ID of the VK group. Available from the group's page (see Details).}
  \item{num_posts}{Number of posts to consider. Either 'all' (default) or a positive integer.}
  \item{access_token}{ Your access token (see ?makeAccessToken).}
  \item{verbose}{Boolean: if FALSE (default), informational messages are suppressed.}
}


\details{
Many VK groups and users have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 
}


\value{
Returns a data.frame with columns:
\item{comment_id}{A numeric ID of the comment.}
\item{commenter_id}{A numeric ID of the user who left the comment. }
\item{date}{Date of the comment. }
\item{text}{Text of the comment.}
\item{likes_count}{Number of likes the comment received.}
\item{reply_to_user}{The numeric ID of the user whom the comment replies to (if the comment is a reply), or NA}
\item{reply_to_comment}{The numeric ID of the comment which the comment replies to (if the comment is a reply), or NA}
\item{user_id_wall}{User ID on whose wall the comment was posted. }
\item{to_post_id}{Post ID which the comment was left on.}
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
dat <- getGroupWallComments(group_id = groupid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}

