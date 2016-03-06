##listanCrawler - alpha

#####Basic functionality OK

#####Might break anytime



Goal: Search a website recursively for any URL matching specified domain or keyword, save the result and their frequency to file. Consider how to allow for concurrent searching.


Sends a HTTP GET request to a domain, searches the body of recieved HTML-page for any URLs found.

###TODO:

- Sort all URLs found

- Save to textfile

- Only store URLs to choosen sites

- Allow saving URLs containing keywords supplied

- Allow to search recursively on all URLs on same domain, or similar

- Optimize
