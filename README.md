Jingle Nodes Relay
==================

[![Logo](http://jinglenodes.googlecode.com/svn-history/r146/trunk/sl-Jingle.jpg)](http://blog.jinglenodes.org)

What is Jingle Nodes?
---------------------

Jingle Nodes is an XMPP Extension that enable users to share and discover P2P Media Relays that can be used to enable Voice and Video Chat via Jingle.

It is the simplest solution to communicate *freely* with your friends without being attached to a closed service providers like Skype, Telecom nor SIP Provider!

Jingle Nodes comes in place with the goal of making easy to setup relay for Jingle Clients, making the task of setting them up close to trivial. It also allow every buddy in your contact list to be a potential Node, enabling also P2P sharing.

Communication Sharing made simple!

XEP Location: [Jingle Nodes XEP](http://xmpp.org/extensions/xep-0278.html)

Technically, Jingle Nodes is a Standard Jingle RTP Server/Proxy, intend to provide easy to use Relay that can be used in ICE-UDP and also on RAW-UDP Jingle Clients.

Why Jingle Nodes?
-----------------

In the creation of the very first version of the Jingle Protocol, one of the main goals was to achieve a P2P enable protocol, that would depend on XMPP for routing, but would be also able to negotiate sessions and exchange content without main proxy servers like present SIP deployments.

After 5 years we still don't have any massive deployments containing and fully supported the current specifications, and even the closer to the specification ones are suffering when P2P is not possible and relay is required and there is no available ones.

That is the problem Jingle Nodes proposes to solve.

[![Schema](http://www.gliffy.com/pubdoc/1765239/M.jpg)](http://www.gliffy.com/pubdoc/1765239/L.jpg)

This project is sponsored by NLnet Foundation:  
[![NLnet Foundation](http://www.nlnet.nl/image/logo.gif)](http://www.nlnet.nl)

The Jingle Nodes Logo is a generous contribution from:  
[Fernando Lins - Graphic and Interface Designer](http://fernandolins.net)

Installation
------------

This project can be downloaded from this page as git project and must be compiled with [rebar](git://github.com/basho/rebar.git) and [Erlang/OTP](http://erlang.org). The commands perform:

```
git clone git://github.com/bosqueviejo/jinglenodes.git
cd jinglenodes
rebar get-deps compile generate
```

In the ```rel/jinglenodes``` directory should be the embeded generation. Can be packed as zip or tar file to put on production.

Configuration
-------------

The configuration file is in ```rel/jinglenodes/etc/app.config```, you need to configure the following sections:

```erlang
    {jn_component, [
        {jid, "mycomponent.myserver.com"},
        {port_range, {10000, 50000}},
        {throttle, {10, 60}},
        {public_ip, "88.88.88.88"},
        {handler, jingle_handler},
        {broadcast, "events.server.com"},
        {discount, 60}
    ]},
```

Note that the info related to [ecomponent](https://github.com/pepeyuilop/ecomponent) can be reviewed in it own site.

The params means:

  * ```jid```, the component JID.
  * ```port_range```, the range for select ports (could be from 1024 to 65535).
  * ```throttle```, has two values:
    * ```max_per_period``` (the first) is the time (in seconds) between checks relay channels.
    * ```period_seconds``` (the second) is the timeout (in seconds) for channels.
  * ```public_ip```, is the host as an IP in the IQ request channel result.
  * ```handler```, the handler to be used. The fix value in this moment is ```jingle_handler```.
  * ```broadcast```, the server to send the notify events.
  * ```discount```, reduce the notification time of call duration.
Compiling and Installing

XML parsing library (REQUIRED)

    Tested libraries are:
           . Expat: recommended. Tested: 2.0.1
           . LibXML2: only experimental support.

    o  OpenSSL (optional)
       It's the only TLS engine supported for now.
           . Tested version:            0.9.8e

    o  zlib (optional)
       It's the only compression engine supported for now.
           . Tested version: 1.2.3

Suggested Library Install 
 build-essential make autoconf libxml2 libxml2-dev zlib1g zlib1g-dev libssl-dev libcrypto++ libcrypto++-dev libexpat1-dev

Building

    rebar clean
    rebar get-deps
    rebar compile
    rebar generate
