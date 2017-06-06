Rvk: R Interface to VK (VKontakte) API
--------------------------------------

Hi! Welcome to **Rvk**, the 1st R package to access VK data!

VK is a highly popular social network in Russia and some other countries
that has an open API allowing to access and analyze large amounts of
data. This package provides a set of functions to extract publicly
available data from VK.

### Installation

You can easily install the package from GitHub:

    install.packages('devtools')
    devtools::install_github('denisStukal/Rvk')

### Authorization

Most functions require that you pass your **access token** as a function
argument. An **access token** is a unique set of symbols that works as
your password to API calls. If you have already used VK API and have a
token, you can skip this section and jump immediately to the next
section to start working with VK data. Otherwise, please read the
documentation for the makeAccessToken() function:

    library(Rvk)
    ?makeAccessToken
