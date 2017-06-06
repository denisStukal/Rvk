\name{getUserPostComments}


\alias{getUserPostComments}


\title{
Get Comments to a User's Post
}


\description{
Returns a data.frame with publicly available information on comments to a post.
}


\usage{
getUserPostComments(user_id, post_id, access_token)
}



\arguments{
  \item{user_id}{ A numeric ID of the user comments to whose post are requested. Available from the user's page (see Details). }
  \item{post_id}{ A numeric ID of the post comments to which are requested. }
  \item{access_token}{ Your access token (see ?makeAccessToken). }
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

}


\value{
A data.frame with columns (can vary due to information availability):
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
userid = AAAAAAAA
postid = BBBBBB
dat <- getUserPostComments(user_id = userid, post_id = postid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}
