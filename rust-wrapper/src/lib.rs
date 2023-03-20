use std::{
    io::{Error, Write},
    marker::PhantomData,
};

use haskell_ffi::{
    from_haskell::marshall_from_haskell_var, to_haskell::marshall_to_haskell_var, ToHaskell,
};

/*******************************************************************************
  Tag (to ensure no orphans)
*******************************************************************************/

pub enum RW {}

/// cbindgen:ignore
pub const RW: PhantomData<RW> = PhantomData;

/*******************************************************************************
  Instances
*******************************************************************************/

impl ToHaskell<RW> for rustls::Certificate {
    fn to_haskell<W: Write>(&self, writer: &mut W, tag: PhantomData<RW>) -> Result<(), Error> {
        let rustls::Certificate(bytes) = self;
        bytes.to_haskell(writer, tag)
    }
}

impl ToHaskell<RW> for rustls::PrivateKey {
    fn to_haskell<W: Write>(&self, writer: &mut W, tag: PhantomData<RW>) -> Result<(), Error> {
        let rustls::PrivateKey(bytes) = self;
        bytes.to_haskell(writer, tag)
    }
}

/*******************************************************************************
  External API
*******************************************************************************/

#[no_mangle]
pub extern "C" fn rust_wrapper_rcgen_generate_simple_self_signed(
    alt_names: *const u8,
    alt_names_len: usize,
    out: *mut u8,
    out_len: &mut usize,
) {
    let alt_names: Vec<String> = marshall_from_haskell_var(alt_names, alt_names_len, RW);
    let result: Result<(rustls::Certificate, rustls::PrivateKey), String> =
        rcgen::generate_simple_self_signed(alt_names)
            .and_then(|c| {
                c.serialize_der().map(|der| {
                    (
                        rustls::Certificate(der),
                        rustls::PrivateKey(c.serialize_private_key_der()),
                    )
                })
            })
            .map_err(|err| format!("{}", err));
    marshall_to_haskell_var(&result, out, out_len, RW);
}
