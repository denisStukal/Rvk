\name{searchUserWall}


\alias{searchUserWall}


\title{
Get Posts/Replies from a User's Page that Mention a Query String
}

\description{
Returns a data.frame with publicly available information about posts/replies that mention a query string.
}

\usage{
searchUserWall(user_id, query, access_token, verbose = FALSE)
}


\arguments{
  \item{user_id}{A numeric user ID. Available from the user's page (see Details). }
  \item{query}{A string to search on a user's page.}
  \item{access_token}{ Your access token (see ?makeAccessToken). }
  \item{verbose}{Boolean (default: FALSE), whether information about scraping iterations should be printed.}
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 
 
}


\value{
A data.frame with columns:
\item{object_id}{A numeric ID of a post or reply mentioning the query string.}
\item{from_user_id}{A numeric user ID who wrote the post or reply.}
\item{user_id_wall}{A numeric ID of the user on whose wall the post or reply was made.}
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
userid = AAAAAAAA
myquery = 'Obama'
dat <- searchUserWall(user_id = userid, query = myquery, access_token = mytoken)
str(dat)
}
}


\keyword{methods}

