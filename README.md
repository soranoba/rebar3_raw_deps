rebar3_raw_deps
=======
[![hex.pm version](https://img.shields.io/hexpm/v/rebar3_raw_deps.svg)](https://hex.pm/packages/rebar3_raw_deps)

Provider for supporting the raw deps.
It is a plugin of [rebar3](https://github.com/erlang/rebar3).

## Overview

Rebar3 don't support to "raw deps" that is supported by rebar2. ([see also](https://github.com/erlang/rebar3/issues/110))

However, It is often necessary in real world.

This plugin is one of the way to deal with this.

## Usage

```erlang:rebar.config
{plugins, [rebar3_raw_deps]}.

{deps, [
        %% It is not a OTP application.
        {mydeps, ".*", {git, "git://github.com/soranoba/mydeps.git", {branch, "master"}}}
       ]}.
```

```bash
$ rebar3 do raw,compile
```

`rebar3 raw` is a need to perform before each of the commands of rebar3.

It can not probably be automatically executed using the hooks.

## License
[MIT License](LICENSE)
