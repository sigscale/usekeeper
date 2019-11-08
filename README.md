# [SigScale](http://www.sigscale.org) UseKeeper

[Open Issues](https://sigscale.atlassian.net/projects/USE/issues/?filter=allopenissues "Open Issues")  

UseKeeper provides a storage and reporting service for usage records (e.g. "CDR").

It as a Component in the [TM Forum](https://www.tmforum.org)
Open Digital Architecture ([ODA](https://www.tmforum.org/oda/ODA)).

### Graphical User Interface (GUI)
A web front end built with Google [Polymer](https://www.polymer-project.org)
web components for
[material design](https://material.io/guidelines/material-design/introduction.html) 
provides simple guided management of Usage Specifications and paginated filtered
views of Usage records.

#### [REST](https://en.wikipedia.org/wiki/Representational_state_transfer)
Supports the [TM Forum](https://www.tmforum.org)
[Open API](https://www.tmforum.org/open-apis/) for Usage Management
([TMF635](https://www.tmforum.org/resources/interface/tmf635-usage-management-api-rest-specification-r14-5-0/)).

#### [Erlang](http://www.erlang.org)
All operations may be performed using the Erlang public API, either manually
on the command line [shell](http://erlang.org/doc/man/shell.html), or through
custom Erlang module development.

### [IPDR](https://www.tmforum.org/ipdr)
The Internet Protocol (IP) Detail Record (IPDR) is an industry standard
exchange format for usage records within the Internet Service Provider (ISP)
ecosystem. Usage records may be exported as IPDR format logs which may be
transfered with SFTP/SCP for offline processing.

