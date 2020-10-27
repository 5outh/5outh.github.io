---
date: 2020-10-12T08:40
title:
tags:
  - Haskell
  - Software
  - Drafts
---

## Context

When dealing with data flow in information systems, it is probably a good idea
to pin that data to a given schema in some way. In a typical web application,
for example, this can be seen in Form Validation, API request validation, type
checking, and database schemae/constraints, to name a few. However, user data is
inherently ambiguous: if we want to allow any sort of complex user expressions,
we must deal with unstructured (to some degree) data. The natural question that
arises is, then, **along what seam does this unstructured user data become
schematized**? In other words, where do we wrangle user-provided unstructured
data into a normalized form that our application can understand and work with?

This blog post lays out a method, [[Progressive Pinning]], to help with
decisionmaking in this regard.

## Application Structure

For the purposes of illustration, I will focus on the application of this
approach to a common problem in web apps: accepting user registration. The
structure of this application will be as follows:

1. A frontend form that accepts an email address, password, credit card number,
   and optional (freeform) address.
2. An API (endpoint) that accepts data from the form as JSON
3. A database that stores the user records, which is constrainted to a given schema.

This is simple enough to flesh out in a short blog post, but complex enough to
warrant some questions about where our data is sanitized. For this example, the
following constraints must be met:

1. Only one user per email address.
2. Email addresses are not blocked by a spam-prevention filter.
3. Passwords satisfy a minimum length requirement (12 characters).
4. Credit card, address, and email address are all valid.
5. Passwords are securely hashed.

### NOTE: One-way flow

## Pinning data on the Frontend

- Email, password, cc# are all required
- Credit card number is validated via a library (easier here, due to existing library)

## Pinning data on the Backend

- Password is hashed immediately, application only sees password at the seam
- Email is validated (easier here, due to existing library)
- Passwords are hashed
- Spam filter check

- \*\* Is credit card info assumed correct? Can we validate this? Maybe stripe,
  for example, provides something here, where you can validate on frontend ->
  pass stripe code to backend as evidence.

## Pinning data in the Database

- Emails are unique

## The other way: data fetches

Other way: can assume all validated, frontend parses into final representation
and to be sure constraints are valid

### NOTES/Extensions

Providing "evidence at the seams" seems a reasonable follow-up. This generalizes
the principle of "Parse, Don't Validate": parsing doesn't necessarily always
**look** like parsing, at least. Sometimes it can be calling an external API for
validation and providing an artifact, or relying on an implicit database
constraint (unique email, e.g., which can be constructed as `Unique Email` maybe)
knowing the constraint.
