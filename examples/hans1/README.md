
To build locally (in Asterius top-level dir):

```
docker build -t tmp:123 -f dev.Dockerfile .
docker run -it --rm -v $(pwd):/workspace -w /workspace tmp:123
```

In separate shell (inside dir containing this file):

```
nix-shell --pure ~/code/blog/default.nix --command "python server.py"
```


In container shell (from `/workspace`)

```
stack build
cd example/test1
ahc-link  --input-hs Hello.hs --browser --bundle
```

TODO do not use root in container

TODO need to setup paths etc in container?

