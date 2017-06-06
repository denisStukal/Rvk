\name{getUserPostReposts}


\alias{getUserPostReposts}


\title{
Get Reposts of a Post on a User's Wall
}


\description{
Returns a data.frame with publicly available information about reposts of a post on a users's wall.

}


\usage{
getUserPostReposts(user_id, post_id, access_token)
}



\arguments{
  \item{user_id}{ A numeric id for the VK group. Available from the group's page (see Details). }
  \item{post_id}{A numeric id for the post on the group's wall. }
  \item{access_token}{ Your access token (see ?makeAccessToken). }
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

}


\value{
A data.frame with columns:
\item{post_id}{Repost ID}
\item{user_id}{ID of the user who made the repost}
\item{date}{Date of the repost}
\item{text}{Text of the repost}
\item{source_type}{Platform type used to make the repost (possible values: vk, widget, api, rss, sms)}
\item{device}{Device used to make the repost (possible values: iphone, wphone, android)}
\item{url}{Source URL address}
\item{num_comments}{Number of comments the repost got}
\item{num_likes}{Number of likes the repost got}
\item{num_reposts}{Number of reposts the repost got}
\item{num_views}{Number of views the repost got}
Some of the columns may be absent if information unavailable. 
}


\references{
VK API Methods: Wall (\url{https://vk.com/dev/wall.getReposts})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
userid = AAAAAAAA
postid = BBBBBB
dat <- getUserPostReposts(user_id = userid, post_id = postid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}

