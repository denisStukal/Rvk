\name{getUserWall}


\alias{getUserWall}


\title{
Get Posts from a User Wall
}


\description{
Returns a data.frame with publicly available information about posts from a user wall
}


\usage{
getUserWall(user_id, access_token)
}


\arguments{
  \item{user_id}{Numeric user ID. Available from user page (see Details).}
  \item{access_token}{Your access token (see ?makeAccessToken).}
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 
}


\value{
A data.frame with columns:
\item{id}{Post ID}
\item{date}{Date when a post was made}
\item{date_POSIXct}{Number of seconds since January 1, 1970 to the moment when a post was made}
\item{text}{Post text}
\item{comments_count}{Number of comments to a post}
\item{likes_count}{Number of likes of a post}
\item{reposts_count}{Number of reposts of a post}
\item{reposted}{Flag for a repost (1 if a post is a repost) }
\item{attachment}{Flag for attachments in a post}
Additional columns with information about reposts and attachments. See References for details. 
}


\references{
VK API Methods: Wall (\url{https://vk.com/dev/wall.get})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
userid = AAAAAAAA
dat <- getUserWall(user_id = userid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}
