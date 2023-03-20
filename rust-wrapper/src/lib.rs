use std::{io::Write, marker::PhantomData};

use haskell_ffi::{
    error::Result,
    from_haskell::marshall_from_haskell_var,
    to_haskell::{marshall_result_to_haskell_var, marshall_to_haskell_var},
    FromHaskell, ToHaskell,
};
use p256::{FieldBytes, SecretKey};
use x509_cert::Certificate;

/*******************************************************************************
  Tag (to ensure no orphans)
*******************************************************************************/

pub enum RW {}

/// cbindgen:ignore
pub const RW: PhantomData<RW> = PhantomData;

/*******************************************************************************
  Instances
*******************************************************************************/

impl ToHaskell<RW> for Certificate {
    fn to_haskell<W: Write>(&self, writer: &mut W, tag: PhantomData<RW>) -> Result<()> {
        let bytes: Vec<u8> = der::Encode::to_der(self)?;
        bytes.to_haskell(writer, tag)
    }
}

impl FromHaskell<RW> for Certificate {
    fn from_haskell(buf: &mut &[u8], tag: PhantomData<RW>) -> Result<Self> {
        let bytes = <Vec<u8>>::from_haskell(buf, tag)?;
        let result = der::Decode::from_der(&bytes)?;
        Ok(result)
    }
}

impl ToHaskell<RW> for SecretKey {
    fn to_haskell<W: Write>(&self, writer: &mut W, tag: PhantomData<RW>) -> Result<()> {
        let bytes: FieldBytes = self.to_bytes();
        let array: &[u8; 32] = bytes.as_ref();
        array.to_haskell(writer, tag)
    }
}

/*******************************************************************************
  The functions that we want to wrap
*******************************************************************************/

fn generate_simple_self_signed(alt_names: Vec<String>) -> Result<(Certificate, SecretKey)> {
    let rcgen_cert = rcgen::generate_simple_self_signed(alt_names)?;
    let der_cert = rcgen_cert.serialize_der()?;
    let der_pkey = rcgen_cert.serialize_private_key_der();

    let cert: Certificate = der::Decode::from_der(&der_cert)?;
    let pkey: SecretKey = pkcs8::DecodePrivateKey::from_pkcs8_der(&der_pkey)?;

    Ok((cert, pkey))
}

/*******************************************************************************
  External API
*******************************************************************************/

#[no_mangle]
pub extern "C" fn rust_wrapper_generate_simple_self_signed(
    alt_names: *const u8,
    alt_names_len: usize,
    out: *mut u8,
    out_len: &mut usize,
) {
    let alt_names: Vec<String> = marshall_from_haskell_var(alt_names, alt_names_len, RW);
    let result = generate_simple_self_signed(alt_names);
    marshall_result_to_haskell_var(&result, out, out_len, RW);
}

#[no_mangle]
pub extern "C" fn rust_wrapper_get_certificate_subject(
    cert: *const u8,
    cert_len: usize,
    out: *mut u8,
    out_len: &mut usize,
) {
    let cert: Certificate = marshall_from_haskell_var(cert, cert_len, RW);
    let result = format!("{}", cert.tbs_certificate.subject);
    marshall_to_haskell_var(&result, out, out_len, RW);
}
