//! Contains the propagators used by Pumpkin.
//!
//! If you want to implement your own propagator then we recommend following the guide in
//! [`pumpkin_core::propagation`].
mod propagators;
pub use propagators::*;
