#!/usr/bin/env nix-shell
#!nix-shell -p poppler_utils -p pdftk -i bash

set -euo pipefail # Fail on errors and whatnot

TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

BASE_URL="https://asmadigames.com/MottainaiKS_PnP.pdf"
BASE_PDF="$TMPDIR/base.pdf"

EXPANSION_URL="https://asmadigames.com/WutaiPnP.pdf"
EXPANSION_PDF="$TMPDIR/expansion.pdf"

OUTPUT="output.pdf"

# Download the PDFs
curl -L -o "$BASE_PDF"      "$BASE_URL"
curl -L -o "$EXPANSION_PDF" "$EXPANSION_URL"

# Base cards (twice, for up to 6 players)
pdftk "$BASE_PDF" cat 2-7 output "$TMPDIR/part1.pdf"
cp "$TMPDIR/part1.pdf" "$TMPDIR/part2.pdf"

# Base temple (thrice, for 2 players per copy)
pdftk "$BASE_PDF" cat 9 output "$TMPDIR/part3.pdf"
cp "$TMPDIR/part3.pdf" "$TMPDIR/part4.pdf"
cp "$TMPDIR/part4.pdf" "$TMPDIR/part5.pdf"

# Expansion cards
pdftk "$EXPANSION_PDF" cat 2-7 output "$TMPDIR/part6.pdf"

# Combine all parts
pdfunite \
  $TMPDIR/part*.pdf \
  $OUTPUT

echo "🚀 Created $OUTPUT"
