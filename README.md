Rvk: R Interface to VK (VKontakte) API
--------------------------------------

Hi! Welcome to **Rvk**, the 1st R package to access VK data!

VK is a highly popular social network in Russia and other that has an
open API allowing to access and analyze large amounts of data. This
package provides a set of functions to extract publicly available data
from VK.

### Installation

You can easily install the package from GitHub:

    install.packages('devtools')
    devtools::install_github('denisStukal/Rvk')

### Authorization

Most functions require that you pass your **access token** as a function
argument. An **access token** is a unique set of symbols that works as
your password to the API calls. If you have already used VK API and have
a token, you can skip this section and jump immediately into accessing
and analyzing VK data. Otherwise, please read documentation for the
makeAccessToken() function:

    library(Rvk)
    ?makeAccessToken
