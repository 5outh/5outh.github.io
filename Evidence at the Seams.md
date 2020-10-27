---
date: 2020-10-12T08:40
title:
tags:
  - Software
  - Drafts
---

In all software projects with any significant purpose, a number of "layers"
or will arise. Communication between, and separation of those individual
elements is core to what we do as software builders. The more consolidated
the component we are working on, the more focused we can be, etc. Being
_aware_ of the way this communication operates is quite important in figuring
out and working on systems. But all communication is lossy. User input has to
be parsed to be understood. Passing around the entire state of the
application to each function is wasteful, so we operate on substrates. Data
is pulled from databases piecemeal, and gaining full context of that data can
be incredibly costly. So we work with abstractions. We take what we need from
broad, sweeping contexts and pass substrates between components.

Working, as we do, in this way, a number of natural _seams_ are built. In a
command-line application, there is a seam between the user and the program.
Similarly there may be a seam between a javascript application and its backend
power-source (an API, perhaps). In data systems, there is an additional seam
between these backend systems and their data source(s). In service-oriented
architectures, there exist seams between services. Channels of data over which
context is lost.

When context is lost, errors occur. We validate web forms (the validations are
forgotten when the data is passed). We parse data through command-line programs
(if parsing fails, the program explodes). We save data in third-party APIs (then
forget if they're stored or not). We hash passwords (then forget that they're
hashed). Forgetfulness is pervasive in software. We have to be vigilant in
fighting against it.

Alexis King writes about one approach for dealing with this problem in their
essay [Parse, don't
validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/).
The idea is simple: when validating, also transform data into a stronger
representation of that data that cannot be misinterpreted. This is a powerful
idea and a great way to think about passing data between seams. However, I
believe that this idea can be generalized.

## An Example

Say that I want to collect a credit card, stored in a stripe account, and then
use that stripe-credit-card in a backend application without actually storing
the data directly, out of security concerns (and because stripe manages it
better). Ideally the transfer of credit-card data exists only between the client
and stripe in this circumstance. I want nothing to do with your credit card
info. I do not want the liability. I do, however, need access to the card,
through stripe. The way to do this is reasoably clear:

1. User types in credit card info.
2. App calls stripe with that info to create a new credit card instance and
   passes back some identifier to locate it within stripe.
3. User posts form, which contains stripe id, but not any of the other credit
   card info.

Now the application has evidence that the credit card was created
successfully and can be used by the system. This doesn't look a lot like
parsing (that will happen separately), but indeed satisfies much the same
purpose. The identifier for the credit card serves as evidence that the credit
card is valid and has been stored in Stripe. Passing this through the form and
handling it, rather than the raw credit card information, in our backend, is
safe.

## Another Example

Assume we are accessing a database from some server. That database has a schema
that, among other things, guarantees that users have a unique email. When saving
a new user, then, the database checks for evidence that their email is unique.

Because of this, one can assume that all users that are stored in the database
have a unique email. If they didn't, it would have been impossible to create
them in the first place. In this case, the database is providing evidence of
email uniqueness on save; the seam between the server and the database itself.

In the other direction, we might want to verify that the user's email is, in
fact, a valid email address. For this operation, parsing is the way to go.
Transform the raw text from the database into a bonafide `Email` via some
parsing mechanism, and evidence has been provided that the user's email is
valid.

## Evidence must live with the data along each seam

## Examples of where not providing evidence causes problems

- lack of a CSRF token
- No evidence of hashing a password
- No email parse
- No constraint validation
- No evidence of user being logged in (cookie)
  - in order to do X, X must be logged in at the handler seam
- No evidence of authorization (see our authorization story at Freckle)
- Value may be null if non-nullness not evident
- Type info is lost (types are a form of evidence at function seams)
- nosql/schemaless databases, schema drift
- idempotency errors (database migration was attempted applied more than once
  since no evidence exists of previous migration occurring)
-

NOTE: borrowing in Rust frees memory at function-seams. Maybe there is something
there?

## Platonic ideals

obviously you have to make a call about when to provide evidence and how strong
that evidence is, talk about how this is just a heuristic and you don't need to
keep every single invariant validated in memory at all times, but it is good to
keep in mind where the holes are. where is evidence of truth _not_ being
provided where it absolutely must be?
