//! Platform-agnostic time types.
//!
//! On native platforms, this re-exports from `std::time`.
//! On WASM, this re-exports from `web-time` which uses the browser's `Performance` API.
//!
//! All code in pumpkin-core should use these types instead of `std::time` directly.

#[cfg(not(target_arch = "wasm32"))]
pub(crate) use std::time::{Duration, Instant};

#[cfg(target_arch = "wasm32")]
pub(crate) use web_time::{Duration, Instant};
