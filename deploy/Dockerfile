# zOS-clone Production Dockerfile
# Multi-stage build for minimal, secure container image

# =============================================================================
# Stage 1: Build
# =============================================================================
FROM rust:1.75-slim-bookworm AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy dependency files first for better layer caching
COPY Cargo.toml Cargo.lock ./
COPY crates/zos-*/Cargo.toml ./crates/

# Create stub files to compile dependencies
RUN mkdir -p src && echo "fn main() {}" > src/main.rs
RUN for dir in crates/*/; do \
    mkdir -p "$dir/src" && \
    echo "pub fn placeholder() {}" > "$dir/src/lib.rs"; \
    done

# Build dependencies only
RUN cargo build --release --workspace 2>/dev/null || true

# Copy actual source code
COPY . .

# Touch files to invalidate cache and rebuild with real sources
RUN find . -name "*.rs" -exec touch {} \;

# Build the release binary
RUN cargo build --release --workspace

# =============================================================================
# Stage 2: Runtime
# =============================================================================
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies only
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    libpq5 \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /var/cache/apt/*

# Create non-root user
RUN groupadd -r zos -g 1000 && \
    useradd -r -g zos -u 1000 -d /app -s /sbin/nologin zos

WORKDIR /app

# Copy binaries from builder
COPY --from=builder /build/target/release/zos-* /usr/local/bin/

# Create required directories
RUN mkdir -p /etc/zos-clone /var/log/zos-clone /tmp/zos-clone && \
    chown -R zos:zos /etc/zos-clone /var/log/zos-clone /tmp/zos-clone

# Copy configuration template
COPY --from=builder /build/deploy/kubernetes/configmap.yaml /etc/zos-clone/

# Switch to non-root user
USER zos

# Environment variables
ENV ZOS_LOG_LEVEL=info \
    ZOS_LOG_FORMAT=json \
    ZOS_SERVER_HOST=0.0.0.0 \
    ZOS_SERVER_PORT=8080 \
    ZOS_METRICS_PORT=9090

# Expose ports
EXPOSE 8080 9090

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Default command - can be overridden
CMD ["zos-runtime"]

# Labels
LABEL org.opencontainers.image.title="zOS-clone" \
      org.opencontainers.image.description="Mainframe-compatible runtime for COBOL, CICS, and IMS" \
      org.opencontainers.image.version="1.3.0" \
      org.opencontainers.image.vendor="zOS-clone Team" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://github.com/zos-clone/zos-clone"
