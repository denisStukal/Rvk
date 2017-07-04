\name{searchGroupWall}


\alias{searchGroupWall}


\title{
Get Posts/Replies from a Group Page that Mention a Query String
}

\description{
Returns a data.frame with publicly available information about posts/replies that mention a query string.
}

\usage{
searchGroupWall(group_id, query, access_token, verbose = FALSE)
}


\arguments{
  \item{group_id}{ A numeric ID of the VK group. Available from the group's page (see Details). }
  \item{query}{A string to search on a group page.}
  \item{access_token}{ Your access token (see ?makeAccessToken). }
  \item{verbose}{Boolean: suppress informational messages if FALSE (default).}
}


\details{
Many VK groups have their ID as part of their URL address (e.g. https://vk.com/publicGROUPID, where GROUPID is a sequence of numbers). In the event a group has a screen name that does not show its ID in the URL, one can access it by clicking on any group post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-GROUPID_POSTID). Pass GROUPID (without the minus sign in front of it) to the function call. 
}


\value{
A data.frame with columns:
\item{object_id}{A numeric ID of a post or reply mentioning the query string.}
\item{from_user_id}{A numeric user ID who wrote the post or reply.}
\item{group_id_wall}{A numeric ID of the group on which wall the post or reply was made.}
\item{date}{Date.}
\item{date_POSIXct}{POSIX date (number of seconds since January 1, 1970).}
\item{post_type}{Post or reply.}
\item{to_post_id}{For replies: a numeric ID of the post the reply replies to. NA for posts.}
\item{text}{Text mentioning the query string.}
\item{comments_count}{Number of comments the post/reply received.}
\item{likes_count}{Number of likes the post/reply received.}
\item{reposts_count}{Number of reposts the post/reply received.}
\item{views_count}{Number of views the post/reply received.}
}


\references{
VK API Methods: Wall (\url{https://vk.com/dev/wall.search})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
groupid = AAAAAAAA
myquery = 'Obama'
dat <- searchGroupWall(group_id = groupid, query = myquery, access_token = mytoken)
str(dat)
}
}


\keyword{methods}

