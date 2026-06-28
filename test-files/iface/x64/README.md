# Generating the dummy iface files

Two sets of dummy `*.hi` files are used in the tests:

1.  `X.hi` and `Main.hi` used in the tests of 'normal' `*.hi` files; and

2.  in a `signatures` directory, `Consumer.hi`, `LogHelper.hi` and `Main.hi`
    used in the tests of Backpack-related `*.hi` files.

## Tests of 'normal' `*.hi` files

### With Nix

Update the `supportedVersions` in the `shell.nix` and then run the following
command `nix-shell --pure --run "generate"`.

### On Windows (PowerShell)

1.  Move to the `\test-files\iface\x64` directory:

    ~~~text
    > cd \test-files\iface\x64
    ~~~

2.  Make a `ghcxyyx` directory (for GHC x.y.z):

    ~~~text
    > mkdir ghcxyyz
    ~~~

3.  Use the correct version of GHC to create the `*.hi` files in the correct
    directory (here, we assume that a YAML file already exists in the repository
    root directory specifying the relevant snapshot):

    ~~~text
    > stack --snapshot ..\..\..\stack-ghc-x.y.z.yaml ghc -- -fforce-recomp -hidir ghcxyyz Main.hs
    ~~~

4.  Clean up the build artefacts that are not required, the `*.o` files and the
    `Main.exe`:

    ~~~text
    > rm *.o
    > rm Main.exe
    ~~~

## Tests of Backpack-related `*.hi` files

See the Stack project located at `\test-files\iface\x64\backpack` and documented
in file `README.md` in that project directory.

### On Windows (Powershell)

1.  Move to the `\test-files\iface\x64\ghcxyyz` directory (for GHC x.y.z)
    created for the tests of 'normal' `*.hi` files:

    ~~~text
    > cd \test-files\iface\x64\ghcxyyz
    ~~~

2.  Make a `signatures` directory:

    ~~~text
    > mkdir signatures
    ~~~

3.  Move to the project directory of the Stack project referred to above.

4.  Build the project but override the specified `snapshot` to specify the
    relevant GHC version. For example (for GHC 9.10.3):

    ~~~text
    > stack --snapshot ghc-9.10.3 build
    ~~~

5.  From the project directory, copy the three generated `*.hi` to the desired
    location. For example (for GHC 9.10.3):

    First, check that `stack path --dist-dir` (with the same specified `snapshot`)
    yields the correct relative path to a `dist` directory. For example:

    ~~~text
    > stack --snapshot ghc-9.10.3 path --dist-dir
    .stack-work\dist\1a191874
    ~~~

    Once that is confirmed, copy the files. For example (for GHC 9.10.3):

    ~~~text
    > cp logger-sig\$(stack --snapshot ghc-9.10.3 path --dist-dir)\build\LogHelper.hi ../ghc9103/signatures

    > cp consumer-pkg\$(stack --snapshot ghc-9.10.3 path --dist-dir)\build\Consumer.hi ../ghc9103/signatures

    > cp consumer-pkg\$(stack --snapshot ghc-9.10.3 path --dist-dir)\build\consumer-demo\consumer-demo-tmp\Main.hi ../ghc9103/signatures
    ~~~
