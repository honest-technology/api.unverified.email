# unverified.email

[![Build status](https://badge.buildkite.com/4338542261546f44f15b79a71bbba84b88e0472e4f81a2ded4.svg)](https://buildkite.com/unverified-dot-email/api)

Unverified.email helps testing if the applications send emails correctly. 

This is the code that powers the api.

When working with emails in the applications I write, I often need 
a way to verify that what is being sent out includes all the data
that I wanted the emails to contain and that it is in the right
format.

Usually I would set up mailhog or mailtrap server. I hope that
this could be useful to have a service that might help others too, 
although I understand that it might be too simple for now. Please
open an issue with suggestions you have in mind on how to improve
it. I will consider carefully if to implement them though :)

### Usage

1. Issue a GET request to https://api.unverified.email/create
   The response will contain a mailbox that for sending emails to
2. Issue a GET request to https://api.unverified.email/receive/<mailbox_id> 
   to receive all the emails that have been sent to that address
  
#### Notes:
- *Warning*: the service is not yet finalised. However I am very certain
  it can be used by developers and in your pipelines and we can iron out
  the small kinks together if they appear. Just open an issue please, if
  something needs attention.
- You can create as many mailboxes as you want.
- The mailboxes and your emails will be stored for 5 minutes only, after which
  They will be deleted. So do use the `/create` endpoint freely.
- The `/receive` endpoint will wait for emails to be delivered for at least 15 
  seconds, so you should not need to hit the url repeatedly.

### Developing

There is a `./do` bash script in the root folder that provides shortcuts
to most of the tasks. I guess the `deploy` command won't
work for you since you don't have credentials to ssh to 
unverified.email servers, but everything else should work fine.

- you will need the haskell tool [stack](https://tech.fpcomplete.com/haskell/get-started),
to build and test the project.
- you will need `hlint`, and `shellcheck` installed to run the lint checks.



