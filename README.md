jinglenodes
===========

Jingle Nodes Relay

[[http://blog.jinglenodes.org http://jinglenodes.googlecode.com/svn-history/r146/trunk/sl-Jingle.jpg]]

* What is Jingle Nodes?

Jingle Nodes is an XMPP Extension that enable users to share and discover P2P Media Relays that can be used to enable Voice and Video Chat via Jingle.

It is the simplest solution to communicate *freely* with your friends without being attached to a closed service providers like Skype, Telecom nor SIP Provider!

Jingle Nodes comes in place with the goal of making easy to setup relay for Jingle Clients, making the task of setting them up close to trivial. It also allow every buddy in your contact list to be a potential Node, enabling also P2P sharing.

Communication Sharing made simple!

XEP Location: [[http://xmpp.org/extensions/xep-0278.html Jingle Nodes XEP]]

_Technically, Jingle Nodes is a Standard Jingle RTP Server/Proxy, intend to provide easy to use Relay that can be used in ICE-UDP and also on RAW-UDP Jingle Clients._

* Why Jingle Nodes?

In the creation of the very first version of the Jingle Protocol, one of the main goals was to achieve a P2P enable protocol, that would depend on XMPP for routing, but would be also able to negotiate sessions and exchange content without main proxy servers like present SIP deployments.

After 5 years we still don't have any massive deployments containing and fully supported the current specifications, and even the closer to the specification ones are suffering when P2P is not possible and relay is required and there is no available ones.

That is the problem Jingle Nodes proposes to solve.

[[http://www.gliffy.com/pubdoc/1765239/L.jpg http://www.gliffy.com/pubdoc/1765239/M.jpg]]

This project is sponsored by NLnet Foundation:
[[http://www.nlnet.nl http://www.nlnet.nl/image/logo.gif]]

The Jingle Nodes Logo is a generous contribution from:
[[http://fernandolins.net Fernando Lins - Graphic and Interface Designer]]

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

Building

rebar clean
rebar get-deps
rebar compile
rebar generate
