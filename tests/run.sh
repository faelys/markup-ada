#!/bin/sh

# Copyright (c) 2015, Natacha Porté
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

set -Ceu

: ${TEST_DIR:=$(dirname "$0")}
: ${EXPECTED_DIR:=${TEST_DIR}/expected}
: ${GENERATED_DIR:=${TEST_DIR}/generated}
: ${MARKDOWN_BIN:=${TEST_DIR}/../bin/markdown}

rm -rf "${GENERATED_DIR}"
mkdir "${GENERATED_DIR}"

test_mkd(){
	NAME="$1"
	shift
	"${MARKDOWN_BIN}" "$@" \
	    <"${TEST_DIR}/${NAME}.mkd" \
	    >"${GENERATED_DIR}/${NAME}.html"
}

test_mkd syntax-summary --html --special
test_mkd corner-cases   --html --discount

diff -Nur "${EXPECTED_DIR}" "${GENERATED_DIR}"
