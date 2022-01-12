# Using prebuilt container images

We host prebuilt container images on Docker Hub under the
[`terrorjack/asterius`](https://hub.docker.com/r/terrorjack/asterius)
repository. The images work with [`podman`](https://podman.io) or
[`docker`](https://www.docker.com).

## About versioning

Whenever the `master` branch gets a new commit, we trigger an image build on our
infrastructure. After the build completes, we push to the
`terrorjack/asterius:latest` tag. When trying `asterius` locally, it's
recommended to use `terrorjack/asterius:latest` since it follows `master`
closely.

The images are built with the `gitrev` label to indicate the exact `asterius`
repository revision. Use `docker inspect terrorjack/asterius | grep "gitrev"` to
find out the revision info.

You may want to stick with a specific version of the prebuilt image for some
time for more reproducibility in e.g. CI builds. In that case, browse for the
[tags](https://hub.docker.com/r/terrorjack/asterius/tags?page=1&ordering=last_updated)
page and use an image with a specific tag, e.g. `terrorjack/asterius:200520`. We
always push a versioned tag first before we update the `latest` tag.

## Using the image

We recommend `podman` for running containers from our prebuilt images. The
following commands are compatible with `docker` as well; simply change `podman`
to `docker`.

The images can be used interactively. Navigate to the project directory and use
the following command to start an interactive `bash` session, mounting the
current directory to `/workspace`. In the `bash` session we can use tools like
`ahc-cabal`, `ahc-dist` or `ahc-link` to compile the Haskell sources.

```console
terrorjack@hostname:/project$ podman run -it --rm -v $(pwd):/workspace -w /workspace terrorjack/asterius
root@hostname:/workspace#
```

It's also possible to use the images in a non-interactive manner:

```console
terrorjack@hostname:/project$ podman run --rm -v $(pwd):/workspace -w /workspace terrorjack/asterius ahc-link --input-hs example.hs
```

Check the reference of the [`docker
run`](https://docs.docker.com/engine/reference/commandline/run) command for
details. [`podman
run`](https://github.com/containers/libpod/blob/master/docs/source/markdown/podman-run.1.md)
accepts most arguments of `docker run` and has its own extensions.

## `podman`-specific tips

When using the prebuilt image with `podman`, things should work out of the box
with the default configuration. Check the official installation
[guide](https://podman.io/getting-started/installation) on how to install
`podman` in your environment. It's likely that you'd like to use `podman` with a
non-root user, in which case make sure to check the official
[tutorial](https://github.com/containers/libpod/blob/master/docs/tutorials/rootless_tutorial.md)
for non-root users before usage.

## `docker`-specific tips

When using the prebuilt image with `docker`, there's a file permission problem
with the default configuration: the default user in the container is `root`, and
the processes will be run with the host `root` users as well. So programs like
`ahc-link` will create output files owned by `root` in the host file system,
which is a source of annoyance. Things still work fine as long as you don't mind
manually calling `chown` to fix the permissions.

The proper solution is remapping the `root` user inside the container to the
current non-root user. See the docker official
[`userns-remap`](https://docs.docker.com/engine/security/userns-remap) guide and
this blog [post](https://www.jujens.eu/posts/en/2017/Jul/02/docker-userns-remap)
for further explanation.
