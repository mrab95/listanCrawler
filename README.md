#listanCrawler - alpha

#####About
Sends a HTTP GET request to a domain, searches the body of recieved HTML-page for any URLs found. URLs are then sorted and duplicates are counted and removed.

#####Current State
Basic functionality almost OK, Might break anytime.

#####Goal
Search a website recursively (all local URLs) for any URL matching specified domain or keyword. The result is then saved with sites and their frequency to file. Keywords and file types to allow can also be read from file.

###TODO
* Clean up code

* Save and read from textfile

* Only store URLs to choosen sites

* Allow saving URLs containing keywords supplied

* Allow to search recursively on all URLs on same domain, or similar

* Optimize (concurrency, tail-recursion, ..)
