[![Build Status](https://travis-ci.org/stackbuilders/twitter-feed.svg?branch=master)](https://travis-ci.org/stackbuilders/twitter-feed) [![Hackage](https://img.shields.io/hackage/v/twitter-feed.svg)](http://hackage.haskell.org/package/twitter-feed)

# Twitter Feed

This package is used for retrieving a users' timeline via the Twitter timeline
API (OAuth). It retrieves the timeline with entities, and links the usernames
and links found in the feed.

It is currently used to retrieve the Twitter feed that is displayed on our
web site, <http://www.stackbuilders.com>.

## Usage

You must pass your OAuth credentials to the client. Create them as follows:

```haskell
import Web.Authenticate.OAuth

myoauth :: OAuth
myoauth = newOAuth
    { oauthServerName     = "api.twitter.com"
    , oauthConsumerKey    = "your consumer key"
    , oauthConsumerSecret = "your consumer secret"
    }

mycred :: Credential
mycred = newCredential "your oauth token"
                       "your oauth token secret"
```

Next, you can call the `timeline` function directly. The arguments to the
timeline function are:

1. OAuth token
1. OAuth Credential
1. Number of tweets to retrieve
1. Whether to exclude replies in feed
1. Username for which to retrieve tweets

```haskell
λ: res <- timeline myoauth mycred 3 False "stackbuilders"
λ: res
Right [SimpleTweet {body = "Ven a nuestro evento de Stack U en Quito el 22 de febrero - Ruby y programaci\243n funcional <a target=\"_blank\" href=\"http://t.co/XHCbwvA8TI\">stackbuilders.com/news/ven-al-ev\8230</a>", tweetId = "434472043862433792"},SimpleTweet {body = "<a target=\"_blank\" href=\"//twitter.com/_eightb\">@_eightb</a> <a target=\"_blank\" href=\"//twitter.com/filipebarcos\">@filipebarcos</a> prove that we didn't use ghcjs! :)", tweetId = "431932790423420929"},SimpleTweet {body = "RT <a target=\"_blank\" href=\"//twitter.com/filipebarcos\">@filipebarcos</a>: w00t!! <a target=\"_blank\" href=\"//twitter.com/stackbuilders\">@stackbuilders</a> just launched their new website! <a target=\"_blank\" href=\"http://t.co/JUD5ALkotF\">stackbuilders.com</a> and it's built in haskell!", tweetId = "431929704388775936"}]
```

Your response will be an `IO Either String [SimpleTweet]`.

## Contributing

Contributions are welcome to this library. Fork, modify, make sure the tests
pass, and open a PR.

## LICENSE

MIT, see the LICENSE file in this repo.

## Authors

Justin Leitgeb (Twitter: [@justinleitgeb](http://twitter.com/justinleitgeb),
Github: [@jsl](https://github.com/jsl)) and
Andrés Torres (Github: [AndresRicardoTorres](https://github.com/AndresRicardoTorres)).
