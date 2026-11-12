{ fetchFromGitHub, buildPythonApplication, lib }:
buildPythonApplication {
  name = "love-web-builder";
  src = fetchFromGitHub {
    owner = "rozenmad";
    repo = "love-web-builder";
    rev = "c8d377b30a06138a8b3954d556ca7a6e3f4147b5";
    sha256 = "0jzdzkpiwc169p2k5r3lp8hsjmh3cl4cm1m1rwn189rzjs5py7pf";
  };

  format = "other";
  installPhase = ''
    mkdir -p $out/bin
    echo "#!/usr/bin/env python" > $out/bin/love-web-builder
    cat $src/build.py >> $out/bin/love-web-builder
    substituteInPlace $out/bin/love-web-builder \
      --replace-fail "lovejs_source/" "$src/lovejs_source/"
    chmod +x $out/bin/love-web-builder
  '';
}
