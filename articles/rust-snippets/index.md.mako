---
title: Rust state-of-the-art template
subtitle: An opinionated set of standard crates
date: 2023-04-05
...

# Introduction

This article is essentially a collection of code snippets written in
rust with no clear theme. They include "raw" rust and state-of-the-art
crates.

I'll probably change this article as things evolve. I plan on removing
snippets about crates that are no longer a consensus, for intance.

For the examples below, we are using an example crate called
`snippets`, with a binary called `tool`.


# Top level

${"##"} Tool-specific `main` function

When creating a tool, we can write a placeholder `src/bin/tool.rs`
file that calls a few default initialization functions that should
only be called once, and then calls a tool-specific `main` function
coming from somewhere referenced by `lib.rs`.

This tool-specific `main` function does all initialization and
looks like the following:

```rust
${snippets_src_libmain_rs}
```

We can also put this code in the top-level `main` function (example
below) when it's more convenient.


${"##"} Top-level `main` function

The top-level `main` function that goes in the `src/bin/tool.rs` is
pretty much always like the following:

```rust
${snippets_src_bin_tool_rs}
```

It's worth making this `main` function the only `pub` in the crate if
you want to use a standard CI that includes [cargo-semver-checks] like
[ghworkflow-rust].


${"##"} Crates

For these the functions above to compile, we need some crates:

```bash
cargo add clap --features derive
cargo add tracing
cargo add tracing-subscriber --features env-filter,tracing-log
cargo add color_eyre
```

What we get by using the batteries-included `main` above:

- [clap] creates a command-line argument parser from the struct, with
  type checking and help derived from the documentation.
- [tracing] provides the `#[instrument]` attribute, which makes adding
  logs very convenient and is also used by `color_eyre` for error
  reporting.
- [color_eyre] an error report handler that captures span traces
  provided by `#[instrument]` and has a colorful output. This
  effectively prints something similar to a backtrace when errors are
  not handled and "bubble up" to `main`.
- [tracing-subscriber] sets up the destination of the traces. In the
  particular case above, we are configuring the filter using the
  [`RUST_LOG`] environment variable, and printing the messages to
  `stderr` when they are enabled.

It's worth noting that there is a whole lot that can be done with
tracing, including directing it to
[OpenTelemetry](https://docs.rs/tracing-opentelemetry/latest/tracing_opentelemetry/)
and/or using it for
[profiling](https://docs.rs/tracing-timing/latest/tracing_timing/),
maybe with [flamegraphs](https://crates.io/crates/tracing-flame).

Be aware, though, that the setup above directs logs to the tracing
infra and not the other way around - meaning that if we increment the
setup above to direct traces to the logging infra, we then effectively
set up a tracing-logging loop.


${"##"} Async tool with tokio

Async versions are not so different. Add the crate:

```bash
cargo add tokio --features=macros,rt,rt-multi-thread
```

Add `async` to the tool-specific `main`:

```rust
${snippets_async_src_libmain_rs}
```

And add `#[tokio::main]` and `async` to the top-level `main`:

```rust
${snippets_async_src_bin_tool_rs}
```

Again, it's worth making this `main` function the only `pub` in the
crate if you want to use a standard CI that includes
[cargo-semver-checks] like [ghworkflow-rust].


# Errors


${"##"} Error type creation, crateless

- Built-in error trait: <https://doc.rust-lang.org/std/error/trait.Error.html>
- Example adapted from: <https://web.mit.edu/rust-lang_v1.25/arch/amd64_ubuntu1404/share/doc/rust/html/book/first-edition/error-handling.html>

```rust
${snippets_src_myerrorcrateless_rs}
```


${"##"} Error type creation with `thiserror`

<https://docs.rs/thiserror/latest/thiserror/>

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DataStoreError {
    #[error("data store disconnected")]
    Disconnect(#[from] io::Error),
    #[error("the data for key `{0}` is not available")]
    Redaction(String),
    #[error("invalid header (expected {expected:?}, found {found:?})")]
    InvalidHeader {
        expected: String,
        found: String,
    },
    #[error("unknown data store error")]
    Unknown,
}
```

${"##"} Error handling with `eyre`

- `eyre` trait: <https://docs.rs/eyre/latest/eyre/>
- `color_eyre` handler: <https://docs.rs/color-eyre/latest/color_eyre/>

```rust
use color_eyre::{eyre::eyre, Result};

fn get_cluster_info() -> Result<ClusterMap> {
    let config = std::fs::read_to_string("cluster.json")?;
    let map: ClusterMap = serde_json::from_str(&config)?;
    let opt = None;
    let fromopt = opt.ok_or_else(|| eyre!("error message"));
    Ok(map)
}
```


# Traits for instantiation

These traits create instances of the type they implement.
Consider the following example type for the sections below:

```rust
#[derive(Debug)]
struct MyType {
    value: usize,
    // ...
}
```


${"##"} Default

This can be auto-derived if we want to use the default for all the
members. Otherwise, we have to define it manually.

<https://doc.rust-lang.org/std/default/trait.Default.html>

```rust
impl Default for MyType {
    fn default() -> Self {
        Self { value: 10 }
    }
}
```


${"##"} From

<https://doc.rust-lang.org/std/convert/trait.From.html>

```rust
impl From<usize> for MyType {
    fn from(value: usize) -> Self {
        MyType { value }
    }
}
```


${"##"} TryFrom

<https://doc.rust-lang.org/stable/std/convert/trait.TryFrom.html>

```rust
impl TryFrom<u32> for MyType {
    type Error = &'static str;
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Ok(MyType {
            value: value as usize,
        })
    }
}
```


# Traits for string conversion


${"##"} Debug

<https://doc.rust-lang.org/std/fmt/trait.Debug.html>

```rust
use std::fmt;

impl fmt::Debug for MyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MyType")
            .field("value", &self.value)
            .finish()
    }
}
```


${"##"} Display

<https://doc.rust-lang.org/std/fmt/trait.Display.html>

```rust
use std::fmt;

impl fmt::Display for MyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
```


${"##"} FromStr

<https://doc.rust-lang.org/std/str/trait.FromStr.html>

```rust
use std::{num, str::FromStr};

impl FromStr for MyType {
    type Err = num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value = s.parse::<usize>()?;
        Ok(MyType { value })
    }
}
```


# Sync I/O


${"##"} Running external commands

<https://doc.rust-lang.org/std/process/struct.Command.html>

<https://doc.rust-lang.org/std/process/struct.Child.html>

Run the command and get the whole output directly:

```rust
Command::new("ls")
    .arg("-l")
    .arg("/")
    .output()?;
```

Spawn a child, read output line-by-line:

```rust
${snippets_src_cmd_rs}
```


# Async I/O with `tokio`

${"##"} Running external commands

<https://docs.rs/tokio/latest/tokio/process/>

```
cargo add tokio --features=macros,rt,rt-multi-thread,process
```

```rust
${snippets_async_src_cmd_rs}
```


# Misc snippets (i.e. unclassified)

${"##"} Entry

<https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html>

Or "how to efficiently update a collection entry that may not be there".

```rust
use std::collections::HashMap;
let mut hashmap: HashMap<i32, Vec<i32>> = HashMap::new();
let key = 5;
// The or_* methods return &mut v, which means we can also::
let entry = hashmap.entry(key).or_default();
entry.push(9);
// These 3 options also do the same:
hashmap.entry(key).or_default().push(9);
hashmap.entry(key).or_insert(vec![]).push(9);
hashmap.entry(key).or_insert_with(|| vec![]).push(9);
```


[clap]: https://docs.rs/clap/latest/clap/
[tracing]: https://docs.rs/tracing/latest/tracing/
[tracing-subscriber]: https://docs.rs/tracing-subscriber/latest/tracing_subscriber
[color_eyre]: https://docs.rs/color-eyre/latest/color_eyre/
[`RUST_LOG`]: https://docs.rs/tracing-subscriber/latest/tracing_subscriber/filter/struct.EnvFilter.html
[cargo-semver-checks]: https://crates.io/crates/cargo-semver-checks
[ghworkflow-rust]: https://github.com/lpenz/ghworkflow-rust
