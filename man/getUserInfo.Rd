\name{getUserInfo}


\alias{getUserInfo}


\title{
Get Information about a User
}


\description{
Returns a data.frame with publicly available information about a user.
}


\usage{
getUserInfo(user_ids, access_token, num_universities = 1, num_schools = 1, num_jobs = 1)
}


\arguments{
  \item{user_ids}{A numeric user ID. Available from the user's page (see Details).}
  \item{access_token}{ Your access token (see ?makeAccessToken). }
  \item{num_universities}{Number of universities to be returned (default is 1). See Details.}
  \item{num_schools}{Number of schools to be returned (default is 1). See Details.}
  \item{num_jobs}{Number of jobs to be returned (default is 1). See Details.}
}


\details{
Many users have their ID as part of their URL address (e.g. https://vk.com/idUSERID, where USERID is a sequence of numbers). In the event users have screen names that do not show their ID in the URL, one can access them by clicking on the user's post (or photo, video, etc.): the post will open with a new URL of the form https://vk.com/SCREENNAME?w=wall-USERID_POSTID). Pass USERID (without the minus sign in front of it) to the function call. 

Universities, schools, and jobs are sorted from last to first by the start year (if available). Last position is returned by default.

}


\value{
A data.frame with columns (can vary due to information availability):
\item{id}{User ID}
\item{first_name}{User's first name}
\item{last_name}{User's last name}
\item{sex}{User's sex (available options: female, male)}
\item{screen_name}{User's screen name}
\item{bdate}{User's birth date}
\item{city}{City code the user specified in the "Contacts" section}
\item{country}{Country code the user specified in the "Contacts" section}
\item{relation}{User's relationship status (1 – single; 2 – in a relationship; 3 – engaged; 4 – married; 5 – it's complicated; 6 – actively searching; 7 – in love; 8 — in a civil union; 0 — not specified.)}
\item{...}{Other fields if available}
}


\references{
VK API Methods: Users (\url{https://vk.com/dev/users.get})
}


\author{
Denis Stukal \url{denis.stukal@nyu.edu}
}


\examples{
\dontrun{
mytoken = 'XXXXXXXXXXXXXX'
userid = AAAAAAAA
dat <- getUserInfo(user_id = userid, access_token = mytoken)
str(dat)
}
}


\keyword{methods}

