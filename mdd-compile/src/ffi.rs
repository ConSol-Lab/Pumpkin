use libc::c_int;
use libc::c_uint;
use libc::size_t;

#[repr(C)]
pub(crate) struct HaddockHandle {
    _private: [u8; 0],
}

#[repr(C)]
pub(crate) struct HaddockVarHandle {
    _private: [u8; 0],
}

#[repr(C)]
pub(crate) struct MddHandle {
    _private: [u8; 0],
}

#[repr(C)]
#[derive(Debug)]
pub(crate) struct MddNode {
    pub(crate) layer: c_uint,
    pub(crate) node_index: c_uint,
}

#[repr(C)]
#[derive(Debug)]
pub(crate) struct MddEdge {
    pub(crate) from: MddNode,
    pub(crate) to: MddNode,
    pub(crate) value: c_int,
}

#[repr(C)]
#[derive(Debug)]
pub(crate) struct MddGraph {
    pub(crate) success: bool,
    pub(crate) variables: *const *const HaddockVarHandle,
    pub(crate) n_variables: c_int,
    pub(crate) edges: *const MddEdge,
    pub(crate) n_edges: c_int,
    pub(crate) sink: MddNode,
}

#[link(name = "copl", kind = "static")]
extern "C" {
    pub(crate) fn init_haddock() -> *mut HaddockHandle;
    pub(crate) fn release_haddock(handle: *mut HaddockHandle);

    pub(crate) fn add_variable(
        haddock: *mut HaddockHandle,
        lb: c_int,
        ub: c_int,
    ) -> *mut HaddockVarHandle;
    pub(crate) fn impose_linear(
        haddock_handle: *mut HaddockHandle,
        mdd_handle: *mut MddHandle,
        vars: *const *const HaddockVarHandle,
        weights: *const c_int,
        n_vars: size_t,
        lb: c_int,
        ub: c_int,
    ) -> bool;
    pub(crate) fn impose_alldiff(
        haddock_handle: *mut HaddockHandle,
        mdd_handle: *mut MddHandle,
        vars: *const *const HaddockVarHandle,
        n_vars: size_t,
    ) -> bool;

    pub(crate) fn init_mdd(haddock: *mut HaddockHandle, init_mdd: size_t) -> *mut MddHandle;
    pub(crate) fn post_mdd(
        haddock_handle: *mut HaddockHandle,
        mdd_handle: *mut MddHandle,
    ) -> MddGraph;
    pub(crate) fn release_mdd(handle: *mut MddHandle);
}
