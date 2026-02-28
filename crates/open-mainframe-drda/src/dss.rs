//! DSS (Data Stream Structure) frame parsing and serialization.
//!
//! Every DRDA message is wrapped in one or more DSS segments. Each segment has
//! a 6-byte header followed by payload data (DDM objects).
//!
//! ```text
//! Offset  Len  Field
//! 0       2    Total length (big-endian, includes the 6-byte header)
//! 2       1    Magic byte: 0xD0
//! 3       1    Format flags (DSS type in low nibble, chain/continue bits)
//! 4       2    Correlation ID (big-endian)
//! ```

use bytes::{Buf, BufMut, BytesMut};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

use crate::code_points::*;
use crate::error::{DrdaError, DrdaResult};

/// Minimum DSS header length.
pub const DSS_HEADER_LEN: usize = 6;
/// Maximum DSS segment size.
pub const DSS_MAX_LENGTH: usize = 32767;

/// A parsed DSS segment.
#[derive(Debug)]
pub struct DssSegment {
    /// DSS type (request=1, reply=2, object=3, communication=4).
    pub dss_type: u8,
    /// Whether this segment is chained to the next (more DSS segments follow).
    pub chained: bool,
    /// Whether the next chained segment has the same correlation ID.
    /// Only meaningful when `chained` is true.
    pub same_correlator: bool,
    /// Whether this is a continuation of a previous segment.
    pub continuation: bool,
    /// Correlation ID.
    pub correlation_id: u16,
    /// Payload bytes (DDM objects).
    pub payload: BytesMut,
}

impl DssSegment {
    /// Create a new reply DSS segment.
    pub fn new_reply(correlation_id: u16, payload: BytesMut) -> Self {
        Self {
            dss_type: DSS_TYPE_REPLY,
            chained: false,
            same_correlator: false,
            continuation: false,
            correlation_id,
            payload,
        }
    }

    /// Create a new reply DSS segment with chaining.
    pub fn new_reply_chained(correlation_id: u16, payload: BytesMut) -> Self {
        Self {
            dss_type: DSS_TYPE_REPLY,
            chained: true,
            same_correlator: false,
            continuation: false,
            correlation_id,
            payload,
        }
    }

    /// Create a new reply DSS segment with chaining and same-correlator bit.
    pub fn new_reply_chained_same_corr(correlation_id: u16, payload: BytesMut) -> Self {
        Self {
            dss_type: DSS_TYPE_REPLY,
            chained: true,
            same_correlator: true,
            continuation: false,
            correlation_id,
            payload,
        }
    }

    /// Create a new object DSS segment (for query data).
    pub fn new_object(correlation_id: u16, payload: BytesMut) -> Self {
        Self {
            dss_type: DSS_TYPE_OBJECT,
            chained: false,
            same_correlator: false,
            continuation: false,
            correlation_id,
            payload,
        }
    }

    /// Create a new object DSS segment with chaining and same-correlator bit.
    pub fn new_object_chained_same_corr(correlation_id: u16, payload: BytesMut) -> Self {
        Self {
            dss_type: DSS_TYPE_OBJECT,
            chained: true,
            same_correlator: true,
            continuation: false,
            correlation_id,
            payload,
        }
    }

    /// Serialize this DSS segment to bytes.
    pub fn serialize(&self) -> BytesMut {
        let total_len = DSS_HEADER_LEN + self.payload.len();
        let mut buf = BytesMut::with_capacity(total_len);

        // Length (2 bytes, big-endian)
        buf.put_u16(total_len as u16);
        // Magic byte
        buf.put_u8(DSS_MAGIC);
        // Format flags: type in low nibble, chain/same-correlator/continue bits
        let mut format = self.dss_type & 0x0F;
        if self.chained {
            format |= DSS_CHAIN_BIT;
        }
        if self.same_correlator {
            format |= DSS_SAME_CORRELATOR_BIT;
        }
        if self.continuation {
            format |= DSS_CONTINUE_BIT;
        }
        buf.put_u8(format);
        // Correlation ID (2 bytes, big-endian)
        buf.put_u16(self.correlation_id);
        // Payload
        buf.extend_from_slice(&self.payload);

        buf
    }
}

/// Read a DSS segment from a TCP stream.
pub async fn read_dss<R: AsyncReadExt + Unpin>(reader: &mut R) -> DrdaResult<DssSegment> {
    // Read the 6-byte header
    let mut header = [0u8; DSS_HEADER_LEN];
    match reader.read_exact(&mut header).await {
        Ok(_) => {}
        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
            return Err(DrdaError::ConnectionClosed);
        }
        Err(e) => return Err(DrdaError::Io(e)),
    }

    let mut hdr = &header[..];
    let length = hdr.get_u16() as usize;
    let magic = hdr.get_u8();
    let format = hdr.get_u8();
    let correlation_id = hdr.get_u16();

    // Validate magic byte
    if magic != DSS_MAGIC {
        return Err(DrdaError::InvalidDss(format!(
            "Expected magic 0xD0, got 0x{:02X}",
            magic
        )));
    }

    // Validate length
    if length < DSS_HEADER_LEN {
        return Err(DrdaError::InvalidDss(format!(
            "DSS length {} is less than header size {}",
            length, DSS_HEADER_LEN
        )));
    }

    if length > DSS_MAX_LENGTH {
        return Err(DrdaError::InvalidDss(format!(
            "DSS length {} exceeds maximum {}",
            length, DSS_MAX_LENGTH
        )));
    }

    // Read payload
    let payload_len = length - DSS_HEADER_LEN;
    let mut payload = BytesMut::zeroed(payload_len);
    if payload_len > 0 {
        reader.read_exact(&mut payload).await?;
    }

    let dss_type = format & 0x0F;
    let chained = (format & DSS_CHAIN_BIT) != 0;
    let same_correlator = (format & DSS_SAME_CORRELATOR_BIT) != 0;
    let continuation = (format & DSS_CONTINUE_BIT) != 0;

    Ok(DssSegment {
        dss_type,
        chained,
        same_correlator,
        continuation,
        correlation_id,
        payload,
    })
}

/// Write a DSS segment to a TCP stream.
pub async fn write_dss<W: AsyncWriteExt + Unpin>(
    writer: &mut W,
    segment: &DssSegment,
) -> DrdaResult<()> {
    let data = segment.serialize();
    writer.write_all(&data).await?;
    Ok(())
}

/// Write multiple DSS segments (e.g., chained reply + object data).
pub async fn write_dss_chain<W: AsyncWriteExt + Unpin>(
    writer: &mut W,
    segments: &[DssSegment],
) -> DrdaResult<()> {
    let mut buf = BytesMut::new();
    for segment in segments {
        buf.extend_from_slice(&segment.serialize());
    }
    writer.write_all(&buf).await?;
    writer.flush().await?;
    Ok(())
}
