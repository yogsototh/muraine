# Muraine

> Eat octopussies

Absorb github API events.

See <yannesposito.com> for the blog post.

## INSTALL

1. Install Haskell
2. clone the repository
3. `cabal install -j --only-dependencies`
4. `cabal build muraine`
4. `cabal build nats-consumer`

## Usage

Install nats

```
> brew install gnatsd
```

Launch it:

```
> gnatsd
```

And in another terminal:

```
> cabal run GITHUB_LOGIN GITHUB_PASSWORD
```

This will send the datas to `NATS`.

To read the content you could:

```
> ./dist/build/nats-consumer/nats-consumer ghevents
```

