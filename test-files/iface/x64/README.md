# Generating the dummy iface

## With Nix

Update the `supportedVersions` in the `shell.nix` and then run the following
command `nix-shell --pure --run "generate"`.

## On Windows (PowerShell)

1.  Move to the `\test-files\iface\x64` directory:

    ~~~text
    cd \test-files\iface\x64
    ~~~

2.  Make a `ghcxyyx` directory (for GHC x.y.z):

    ~~~text
    mkdir ghcxyyz
    ~~~

3.  Use the correct version of GHC to create the `*.hi` files in the correct
    directory (here, we assume that a YAML file already exists in the repository
    root directory specifying the relevant snapshot):

    ~~~text
    stack --snapshot ..\..\..\stack-ghc-x.y.z.yaml ghc -- -fforce-recomp -hidir ghcxyyz Main.hs
    ~~~

4.  Clean up the build artefacts that are not required, the `*.o` files and the
    `Main.exe`:

    ~~~text
    rm *.o
    rm Main.exe
    ~~~
