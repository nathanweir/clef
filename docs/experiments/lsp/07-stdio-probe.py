#!/usr/bin/env python3
"""Drive the built clef binary over real stdio, the way an editor does.

    python3 docs/experiments/lsp/07-stdio-probe.py

Why this exists, and why it is Python in a Lisp repo: every other test in this
project calls handlers *in process*. That is fast and precise and it cannot see
anything about clef as a running program -- whether it frames its output
correctly, whether it survives a closed pipe, or whether it ever stops. Driving
it as a subprocess over a pipe is the only way to ask those questions, and doing
that from inside the image under test would defeat the point.

It earned its keep immediately. Four bugs, none visible to the 155-test suite:

  * the server could not be stopped. `exit' said "For now do nothing" and the
    read loop had no exit condition, so neither the notification nor stdin
    closing ended it -- it spun on a dead stream and outlived every session.
  * `exit' was blocked by the not-initialized guard, so a client that quit
    mid-handshake could not clean up, and the process exited 0 where the spec
    requires 1.
  * `shutdown' returned {"result": {"result": null}} -- handlers return results,
    not response envelopes -- and never recorded that it had happened, leaving
    the exit code undecidable.
  * go-to-definition in a buffer whose file is not on disk answered "Internal
    server error", because an empty scope list reached a type-checked accessor.

Each is now pinned by a test in lsp/test/protocol-tests.lisp. This script stays
as the thing that would notice the next one.
"""
import json
import os
import re
import subprocess
import sys
import time

BINARY = "./lsp/clef"


def frame(obj):
    body = json.dumps(obj).encode()
    return b"Content-Length: " + str(len(body)).encode() + b"\r\n\r\n" + body


def parse_replies(raw):
    """Every framed message on stdout, in order."""
    out = []
    for m in re.finditer(rb"Content-Length: (\d+)\r\n\r\n", raw):
        body = raw[m.end():m.end() + int(m.group(1))]
        try:
            out.append(json.loads(body.decode(errors="replace")))
        except Exception:
            out.append({"unparseable": body[:80].decode(errors="replace")})
    return out


def scenario(label, messages, timeout=25, expect_exit=None):
    payload = b"".join(frame(m) for m in messages)
    start = time.time()
    try:
        proc = subprocess.run([BINARY], input=payload,
                              capture_output=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        print(f"  {label:<38} DID NOT EXIT within {timeout}s")
        return False
    elapsed = time.time() - start
    replies = parse_replies(proc.stdout)
    ids = [r.get("id", r.get("method", "?")) for r in replies]
    errors = [r for r in replies if "error" in r]
    bad_code = expect_exit is not None and proc.returncode != expect_exit
    flags = ""
    if errors:
        flags += "  ERRORS"
    if bad_code:
        flags += f"  WRONG EXIT (wanted {expect_exit})"
    print(f"  {label:<38} exit={proc.returncode} {elapsed:5.1f}s "
          f"replies={ids}{flags}")
    for e in errors:
        msg = " ".join(str(e["error"].get("message", "")).split())
        print(f"      id={e.get('id')} error {e['error'].get('code')}: {msg[:400]}")
    if proc.stderr.strip():
        first = proc.stderr.decode(errors="replace").strip().splitlines()[0]
        print(f"      stderr: {first[:150]}")
    return not errors and not bad_code


def init_msg(root):
    return {"jsonrpc": "2.0", "id": 1, "method": "initialize",
            "params": {"processId": None, "rootUri": root, "capabilities": {}}}


INITIALIZED = {"jsonrpc": "2.0", "method": "initialized", "params": {}}
EXIT = {"jsonrpc": "2.0", "method": "exit", "params": {}}


def shutdown(msg_id):
    return {"jsonrpc": "2.0", "id": msg_id, "method": "shutdown", "params": {}}


def main():
    if not os.path.exists(BINARY):
        sys.exit(f"{BINARY} not found -- run `mise run build` first")

    os.makedirs("tmp/emptyws", exist_ok=True)
    empty = "file://" + os.path.abspath("tmp/emptyws")
    ok = True

    # The spec fixes these codes: 0 when a shutdown request came first, 1
    # otherwise. A client that never asked for shutdown is being told its
    # server died unexpectedly, and that distinction is the whole point.
    print("\nTermination:")
    ok &= scenario("initialize only, then EOF", [init_msg(None)], expect_exit=0)
    ok &= scenario("initialize + shutdown + exit",
                   [init_msg(None), INITIALIZED, shutdown(2), EXIT], expect_exit=0)
    ok &= scenario("exit with no shutdown",
                   [init_msg(None), EXIT], expect_exit=1)
    ok &= scenario("exit before initialized",
                   [init_msg(None), EXIT], expect_exit=1)

    print("\nWorkspace roots:")
    ok &= scenario("rootUri = empty dir",
                   [init_msg(empty), INITIALIZED, shutdown(2), EXIT], expect_exit=0)
    ok &= scenario("rootUri = the clef repo",
                   [init_msg("file://" + os.path.abspath(".")),
                    INITIALIZED, shutdown(2), EXIT],
                   timeout=90, expect_exit=0)

    # A buffer whose file is not on disk -- an unsaved file, which every editor
    # produces and which nothing else here covers.
    print("\nA real edit session (file not on disk):")
    uri = "file:///nonexistent/never/written.lisp"
    text = ("(defpackage :probe (:use :cl))\n"
            "(in-package :probe)\n"
            "(defun hello (x) x)\n"
            "(defun caller () (hello 1))\n")
    at = {"textDocument": {"uri": uri}, "position": {"line": 3, "character": 18}}
    ok &= scenario("didOpen + definition + hover + symbols",
                   [init_msg(empty), INITIALIZED,
                    {"jsonrpc": "2.0", "method": "textDocument/didOpen",
                     "params": {"textDocument": {"uri": uri, "languageId": "lisp",
                                                 "version": 1, "text": text}}},
                    {"jsonrpc": "2.0", "id": 2,
                     "method": "textDocument/definition", "params": at},
                    {"jsonrpc": "2.0", "id": 3,
                     "method": "textDocument/hover", "params": at},
                    {"jsonrpc": "2.0", "id": 4,
                     "method": "textDocument/documentSymbol",
                     "params": {"textDocument": {"uri": uri}}},
                    EXIT],
                   expect_exit=1)

    print("\n" + "=" * 40)
    print("all scenarios clean" if ok else "FINDINGS above")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
