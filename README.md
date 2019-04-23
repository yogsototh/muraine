> This project has two homes.
> It is ok to work in github, still, for a better decentralized web
> please consider contributing (issues, PR, etc...) throught:
>
> https://gitlab.esy.fun/yogsototh/muraine

---


# Muraine

> Eat octopussies

Absorb github API events.

See <yannesposito.com> for the blog post.

## INSTALL

1. Install [`stack`](http://docs.haskellstack.org/en/stable/README.html)
2. Install [nats](https://nats.io): `brew install gnatsd`
2. clone the repository
3. `stack setup`
4. `stack build`

## Usage

Launch nats:

```
> gnatsd
```

And in another terminal:

```
> stack exec muraine -- GITHUB_LOGIN GITHUB_PASSWORD
```

This will send the datas to `NATS`.

To read the content you could use `nats-consumer`:

```
> nats-consumer ghevents
```

