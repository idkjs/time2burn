# Time 2 Burn

Everything runs browser-side, there is no backend, just flat files to serve over HTTP.

### Local development

#### Setup
- `brew install sassc`
1. Install OPAM from your package manager
2. In repo: `opam switch create . ocaml-variants.4.10.0+flambda --deps-only`

#### Development
```bash
dune clean && dune build src/app.bc.js files/style.css -w

# Or any other server
python3 -m http.server --directory files/ 8081
```

### Deploy
```bash
dune clean && DUNE_PROFILE=release dune build src/app.bc.js files/style.css

./scripts/deploy.sh
```

#### Test deployment image
```bash
docker build . -t time2burn:latest

docker run -it --rm -p 8081:8081 time2burn:latest
```
