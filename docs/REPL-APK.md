# REPL-APK.md — From CLEF Image to Signed Installable APK

CC0 1.0 Universal — public domain. See LICENSE.

The recipe: **CLEF image → `.so` → signed installable APK**, including
**slynk bundling** for remote programming. Every packaging step runs on the
host in pure Common Lisp (no `aapt`, no `jarsigner`, no C toolchain).

Status legend: ✅ done · 🚧 spec'd, pending backend · ⬜ blocked.

---

## 1. Pipeline overview

```
 CLEF image (boot + your app)
      │  (1) image builder ── cross-compile to aarch64        🚧 issue #9
      ▼
 libclef.so  (static, no DT_NEEDED, ANativeActivity_onCreate export)
      │  (2) package: AXML manifest + zip + v1 JAR signing     ✅ src/android/
      ▼
 app.apk  ──(3) adb install──▶  device
      │
      └─(4) slynk over adb forward ──▶  remote REPL from host  🚧 needs core
```

## 2. Step 1 — image → `.so` 🚧

Blocked on the aarch64 backend (issue #9) and the cold core (issue #1).
The image builder emits a static `.so` satisfying `docs/ANDROID-ABI.md`:

- no `DT_NEEDED`; the only export is `ANativeActivity_onCreate`;
- 16 KB `PT_LOAD` alignment;
- W^X via `memfd_create` dual mapping at runtime.

Your application code is part of the image: anything `load`ed or `defun`ed
before the image is saved ships in the APK. The REPL itself ships too —
that is what makes step 4 possible.

## 3. Step 2 — `.so` → signed APK ✅

Implemented in `src/android/` (commit a0468a1). All in Common Lisp.

```lisp
;; Build an unsigned APK: manifest + native lib + assets.
(let* ((pkg (find-package :clef/android))
       (manifest (funcall (intern "WRITE-ANDROID-MANIFEST" pkg)
                          :package "org.clef.repl"
                          :version-code 1
                          :version-name "0.1"
                          :min-sdk 30 :target-sdk 34
                          :has-code nil          ; NativeActivity, no dex
                          :lib-name "clef"))
       (apk (funcall (intern "BUILD-APK" pkg)
                     (list (funcall (intern "MAKE-APK-FILE" pkg)
                                    "AndroidManifest.xml" manifest)
                           (funcall (intern "MAKE-APK-FILE" pkg)
                                    "lib/arm64-v8a/libclef.so" so-bytes)))))
  ...)

;; Sign it (v1 JAR signing: MANIFEST.MF + CLEF.SF + CLEF.RSA).
(funcall (intern "SIGN-APK" pkg) apk private-key cert-der)
```

Pieces:

- **AXML writer** (`axml.lisp`) — binary `AndroidManifest.xml`, spec-valid
  string pool + element tree. `android:hasCode="false"`,
  `android.app.NativeActivity`, `android.app.lib_name` meta-data.
- **Zip/APK builder** (`zip.lisp`, `apk.lisp`) — stored entries, CRC32,
  correct local + central headers (round-trip verified against Python's
  `zipfile`).
- **v1 JAR signing** (`sign.lisp`) — SHA-256 digests per entry
  (`MANIFEST.MF`), per-section digests (`CLEF.SF`), and a real RSA PKCS#1
  v1.5 signature in a PKCS#7 block (`CLEF.RSA`). Verified: a 1024-bit test
  key signs, the public key verifies, a 1-bit tamper fails.
- Keys for release come from your own keystore; `tools/dev/gen-test-key.lisp`
  mints throwaway test keys (deterministic, dev-only, never for release).

## 4. Step 3 — install

```bash
adb install -r app.apk
```

(`adb` is a host-side tool; using it is not a C-toolchain dependency in our
tree — it's the delivery mechanism, same as `scp` for the Linux target.)

## 5. Step 4 — slynk remote REPL 🚧

The app runs a **slynk server on a localhost port** on the device. Expose it
to the host over USB:

```bash
adb forward tcp:4005 tcp:4005
```

Then from the host editor (Emacs/SLY, or any slynk client):

```
M-x sly-connect RET localhost RET 4005
```

You now have a live REPL *into the running Android process*: redefine
functions, inspect the event loop, hot-patch the UI DSL — the full CLEF
dev-loop, on-device.

Bundling notes:

- slynk ships inside the CLEF image (it's just Lisp code loaded before the
  image is saved — step 1). No extra APK entries.
- The server binds `127.0.0.1` only; `adb forward` is the only bridge.
  No `INTERNET` permission needed for the REPL use-case — keep the manifest
  permission-free.
- On `APP_CMD_DESTROY` the runtime should close the slynk listener; on the
  next launch it re-binds. (Lifecycle per ANDROID-ABI.md §4.3.)

## 6. What remains (tracker)

| Item | Issue |
|------|-------|
| aarch64 backend so the image can become a real `.so` | #9 |
| cold core so Lisp runs on-device at all | #1 |
| UI DSL live calls over the JNI bridge (spec'd in `src/android/ui.lisp`) | this issue |
| Linux mock shim running the lifecycle state machine (ABI §7) | #7 |
