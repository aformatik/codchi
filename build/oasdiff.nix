{ lib
, buildGoModule
, fetchFromGitHub
}:

# oasdiff is not packaged in nixpkgs; we build it here so the OpenAPI
# breaking-change gate (see .github/workflows/ci.yml) has a pinned, cached
# binary. Bump `version`/`hash`/`vendorHash` together.
buildGoModule rec {
  pname = "oasdiff";
  version = "1.18.5";

  src = fetchFromGitHub {
    owner = "oasdiff";
    repo = "oasdiff";
    rev = "v${version}";
    hash = "sha256-zqxsxgj7kDSnOl+nPE40+zpynmKxGjlrUlzyEWEm/uw=";
  };

  vendorHash = "sha256-+bRE23X6KL2Y7hdXPRxPu3WFPMWrjipINyf+5lJn0Q0=";

  # The repo's own tests need network / fixtures we don't ship.
  doCheck = false;

  ldflags = [ "-s" "-w" ];

  meta = {
    description = "OpenAPI diff and breaking-change detector";
    homepage = "https://github.com/oasdiff/oasdiff";
    license = lib.licenses.asl20;
    mainProgram = "oasdiff";
  };
}
