Dugnutt:  brutal many-paths-explored 'dig'

Dugnutt is a tool that aims to help diagnose a class of problems
I repeatedly encountered when investigating DNS configurations:
for example, two servers serving different data for the same
zone, or glue records do not aligh with the authoritative zone.

Use:

  dugnutt 'server.domain.com.' A

Dugnutt will churn producing trace output for several minutes,
and then provide every answer to the specified query that could
be found by persuing as many different resolution paths as
possible (rather than, in a traditional resolver, returning the
first result found).


* Related tools

A Tool for DNS Delegation Trust Graphing
https://github.com/mandatoryprogrammer/trusttrees

dnstrace searches for all DNS servers that can affect the resolution of records of type t under the domain name fqdn, starting from the root server r.
https://cr.yp.to/djbdns/debugging.html
